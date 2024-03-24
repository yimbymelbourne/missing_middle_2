

find_profitable_apartments <- function(data){
base_cost = 3334*1.1*1.06*80 * 1.30 #figure here, plus 6% inflation since release, plus 30% developer profit https://content.knightfrank.com/research/911/documents/en/melbourne-new-apartments-insight-q3-2023-10663.pdf
demolition_costs = 70000



variables_to_include <- c("dist_rail",
                          "property_price_per_sqm",
                          "lat",
                          "lon",
                          "missing_middle_yield",
                          "lga_name_2022",
                          "zone_short_mm",
                          "heritage_status",
                          "traffic_pollution",
                          "prox_walk_time_s_tram",
                          "prox_walk_time_s_train",
                          "sa2_code_2021",
                          "zone_short",
                          "cbd_dist",
                          "vacant_in_2016",
                          "lot_size",
                          "mm_yield_net")



house_prices <- data %>% 
  st_drop_geometry() %>%
  filter(zoning_permits_housing == "Housing permitted",
         !feature_preventing_development,
         zone_short != "Low density residential",
         dwellings_est<2) %>% 
  mutate(vacant_in_2016 = replace_na(vacant_in_2016,"No")) %>% 
  select(any_of(variables_to_include)) %>% 
  left_join(prices_estimates) %>% 
  mutate(missing_middle_storeys = case_when(zone_short_mm == "General residential" ~ 3,
                                            zone_short_mm == "Residential Growth" ~ 4,
                                            zone_short_mm == "Missing middle" ~ 6,
                                            zone_short_mm == "Mixed use" ~ 12),
         missing_middle_apartments_per_floor = missing_middle_yield / missing_middle_storeys,
         parking_cost = if_else(zone_short_mm == "Residential Growth",50000,0)) %>% 
  #mutate(zone_short = if_else(zone_short == "Residential Growth","General residential",zone_short)) %>%  #mixed use land is worth more because of future expectations zoning rents, we should exclude those expectations from land value calcs. 
  mutate(property_price = property_price_per_sqm * lot_size ) %>% 
  rowwise() %>% 
  mutate(storey = list(seq_len(missing_middle_storeys))) %>%
  unnest(storey) %>% 
  mutate(cost_of_building_one_apartment_on_this_floor = parking_cost + (base_cost + (.008 * storey * base_cost))) %>% 
  group_by(lat,lon) %>%
  arrange(storey) %>% 
  mutate(apartments_if_built_to_this_height = floor(cumsum(missing_middle_apartments_per_floor)),
         cost_if_built_to_this_height = apartments_if_built_to_this_height * cummean(cost_of_building_one_apartment_on_this_floor)) %>% 
  ungroup() %>% 
  group_by(lat,lon) %>%
  mutate(land_cost = property_price + demolition_costs,
         profit = apartment_prices * apartments_if_built_to_this_height - cost_if_built_to_this_height - land_cost,
         profit_per_apartment = profit/apartments_if_built_to_this_height) %>% 
  group_by(lat,lon) %>% 
  arrange(desc(profit)) %>% 
  filter(row_number() == 1) %>%
  mutate(profitable_apartments = if_else(profit<0,0,apartments_if_built_to_this_height),
         profit_from_buildable_apartments = if_else(profit<0,0,profit)) %>%
  select(lat,lon,
         profitable_apartments,
         profit,
         apartments_if_built_to_this_height,
         profit_from_buildable_apartments,
         storey,
         missing_middle_apartments_per_floor,
         missing_middle_storeys,
         cost_of_building_one_apartment_on_this_floor)

  
  output <- data %>% left_join(house_prices)
  return(output)
}

# 
# ########
# house_prices %>% 
#   st_drop_geometry() %>% 
#   select(lat,lon,apartments_if_built_to_this_height,profit,storey,missing_middle_storeys) %>% 
#   filter(profit>0,
#          between(apartments_if_built_to_this_height,1,150)
#   ) %>% 
#   right_join(df_mel_props) %>% 
#   group_by(lga_name_2022,zone_short_mm,storey) %>% 
#   summarise(n = sum(apartments_if_built_to_this_height,na.rm = TRUE)) %>%
#   ggplot(aes(x = storey,
#              y = n,
#              fill = zone_short_mm))+
#   facet_wrap(~lga_name_2022)+
#   geom_bar(stat = "identity")+
#   labs(title = "where are the profitable apartments in MM zoning?",
#        x = "Building height")
# 
# 
# density_by_km = house_prices %>% 
#   st_drop_geometry() %>% 
#   select(lat,lon,apartments_if_built_to_this_height,profit,storey,missing_middle_storeys,cbd_dist,lot_size) %>% 
#   mutate(apartments_if_built_to_this_height = if_else(profit<0,
#                                                       0,
#                                                       apartments_if_built_to_this_height)) %>% 
#   mutate(dist_kbd_km = 1000*(floor(cbd_dist/1000))) %>% 
#   group_by(dist_kbd_km) %>% 
#   summarise(density = sum(apartments_if_built_to_this_height,na.rm = TRUE)/sum(lot_size,na.rm = TRUE),
#             n = sum(apartments_if_built_to_this_height,na.rm = TRUE)) 
# 
# density_by_km%>%
#   ggplot(aes(x = dist_kbd_km, y = density))+
#   geom_line(stat = "identity")
# 
# 
# lga_prices <- 
#   house_prices %>% 
#   select(lat,lon,apartments_if_built_to_this_height,profit) %>% 
#   filter(profit>0,
#          apartments_if_built_to_this_height >1) %>% 
#   right_join(df_mel_props) %>% 
#   group_by(lga_name_2022) %>% 
#   summarise(existing_dwellings = sum(dwellings_est,.na.rm = TRUE),
#             mm_profitable_apartments = sum(apartments_if_built_to_this_height,na.rm = TRUE),
#             mm_profit_from_apartments = sum(profit, na.rm = TRUE),
#             lga_area = sum(lot_size),
#             .groups = "drop") %>% 
#   mutate(across(where(is.numeric), 
#                 ~ round(.x / sum(.x, na.rm = TRUE) * 100,1), 
#                 .names = "{.col}_percent"))
# 
# lga_prices %>%
#   ungroup() %>% 
#   mutate(lga_name_2022 = fct_reorder(lga_name_2022,mm_profitable_apartments_percent)) %>% 
#   pivot_longer(c("existing_dwellings_percent",
#                  #"mm_profit_from_apartments_percent",
#                  "mm_profitable_apartments_percent"),
#                names_to = "type",
#                values_to = "values") %>%
#   mutate(type = if_else(type == "existing_dwellings_percent", 
#                         "Share of state's existing homes",
#                         type)) %>% 
#   ggplot(aes(x = lga_name_2022, 
#              y = values, 
#              fill = type))+
#   geom_bar(stat = "identity",
#            position = "dodge")+
#   coord_flip()
# 
# 
# lga_prices %>%
#   ungroup() %>% 
#   mutate(change_in_density_required = mm_profitable_apartments/lga_area) %>% 
#   mutate(lga_name_2022 = fct_reorder(lga_name_2022,change_in_density_required)) %>% 
#   ggplot(aes(x = lga_name_2022, 
#              y = change_in_density_required))+
#   geom_bar(stat = "identity",
#            position = "dodge")+
#   coord_flip()
# 
# 
# 
# sf_mel_props %>% 
#   select(lat,lon,zone_short_mm,lot_size,geom) %>% 
#   left_join(house_prices %>% 
#               select(select(lat,lon,apartments_if_built_to_this_height,profit,storey,missing_middle_storeys)
#                      )
#             )%>% 
#   st_write("test/test2.shp")
# 
# 
# house_prices %>% 
#   filter(profit>0) %>% 
#   filter(zone_short_mm == "Residential Growth") %>%
#   group_by(apartments_if_built_to_this_height) %>% 
#   summarise(n=n(),
#             apartments = sum(apartments_if_built_to_this_height)) %>% view()
#   