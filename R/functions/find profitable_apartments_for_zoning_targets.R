




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



### FAILED ATTEMPT TO USE suburb level inflators 
## FAILED BECAUSE THERE"S JUST TOO MUCH YEAR ON YEAR VARIATION WITHIN SUBURBS. 
# unit_price_file <- "data/vic_unit_prices_by_suburb.xlsx"
# if(!file.exists(unit_price_file)) {
# 
#   download.file("https://www.land.vic.gov.au/__data/assets/excel_doc/0021/660135/Copy-of-Suburb_UnitV2022z.xlsx",destfile = unit_price_file)
# }
# 
# unit_prices_by_suburb <- readxl::read_excel(unit_price_file,skip = 1) %>%
#   janitor::clean_names() %>% 
#   rename(locality = x1) %>% 
#   filter(!is.na(locality),
#          locality != "locality") %>%
#   mutate(across(x2012:prelim_2023,parse_number)) %>% 
#   rowwise() %>% 
#   mutate(typical_unit_price_now = mean(c(prelim_2023,prelim_2023,prelim_2023,x2022),na.rm = TRUE),
#          typical_unit_price_2018 = mean(c(x2018,x2018,x2018,x2017,x2019),na.rm = TRUE),
#          apartment_inflation = typical_unit_price_now/typical_unit_price_2018)