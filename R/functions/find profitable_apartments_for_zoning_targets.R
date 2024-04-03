
apartment_inflation = 1.07653 #https://reiv.com.au/market-insights/victorian-insights#metro
house_inflation = 909/812 #https://reiv.com.au/property-data/rmx # Although other indexes are much higher so we may need to change. 

calculate_apartment_cost <- function(max_storeys, zone) {
  apartment_size <- 85
  variable_costs <- 1.15  # Finance, architect fees etc
  profit <- 1.2
  fixed_costs <- 30000
  
  construction_cost <- ifelse(max_storeys <= 3, 3350,
                              ifelse(max_storeys <= 8, 3800, 4000))
  
  car_park_cost <- ifelse(zone %in% c("Mixed use", "Missing middle"), 0, 58000)
  
  total_costs <- (construction_cost * apartment_size + car_park_cost + fixed_costs) * variable_costs * profit
  
  return(total_costs)
}

calculate_apartment_cost <- Vectorize(calculate_apartment_cost)

find_profitable_apartments <- function(data){
  


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
  lazy_dt() %>% 
  mutate(vacant_in_2016 = replace_na(vacant_in_2016,"No")) %>% 
  select(any_of(variables_to_include)) %>% 
  left_join(all_predicted_prices) %>% 
  ungroup() %>%
  filter(property_price_per_sqm>0) %>% 
  mutate(property_price_per_sqm = property_price_per_sqm * house_inflation ,
         apartment_price = apartment_price * apartment_inflation) %>% 
  mutate(property_price   = property_price_per_sqm * lot_size ) %>% 
  mutate(missing_middle_storeys = case_when(zone_short_mm == "General residential" ~ 3,
                                            zone_short_mm == "Residential growth"  ~ 4,
                                            zone_short_mm == "Missing middle"      ~ 6,
                                            zone_short_mm == "Mixed use"           ~ 12),
         missing_middle_apartments_per_floor = missing_middle_yield / missing_middle_storeys) %>% 
  as_tibble() %>%
  rowwise() %>% 
  mutate(storey = list(seq_len(missing_middle_storeys))) %>%
  unnest(storey) %>% 
  mutate(construction_cost_per_apt_this_height = calculate_apartment_cost(storey,zone_short_mm)) %>% 
  lazy_dt() %>% 
  group_by(lat,lon) %>%
  arrange(storey) %>% 
  mutate(apartments_if_built_to_this_height = floor(cumsum(missing_middle_apartments_per_floor)),
         construction_cost_if_built_to_this_height   = apartments_if_built_to_this_height  * construction_cost_per_apt_this_height,
         revenue_if_build_to_this_height = apartments_if_built_to_this_height * apartment_price ) %>% 
  ungroup() %>% 
  group_by(lat,lon) %>%
  mutate(profit = revenue_if_build_to_this_height - construction_cost_if_built_to_this_height - property_price,
         profit_per_apartment = profit/apartments_if_built_to_this_height) %>% 
  group_by(lat,lon) %>% 
  arrange(desc(profit)) %>% 
  filter(row_number() == 1) %>%
  mutate(profitable_apartments = if_else(profit<0,0,apartments_if_built_to_this_height),
         profit_from_buildable_apartments = if_else(profit<0,0,profit)) %>%
  as_tibble() %>% 
  select(lat,lon,
         profitable_apartments,
         profit_per_apartment,
         profit,
         apartments_if_built_to_this_height,
         profit_from_buildable_apartments,
         storey,
         missing_middle_apartments_per_floor,
         missing_middle_storeys,
         construction_cost_per_apt_this_height,
         construction_cost_if_built_to_this_height,
         revenue_if_build_to_this_height,
         apartment_price,
         property_price)

  
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
#   filter(zone_short_mm == "Residential growth") %>%
#   group_by(apartments_if_built_to_this_height) %>% 
#   summarise(n=n(),
#             apartments = sum(apartments_if_built_to_this_height)) %>% view()
#   