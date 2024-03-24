
source("R/00renv.R")
dwellings_with_prices <- read_csv("data/dwellings_with_prices.csv")

all_prices <- dwellings_with_prices %>% 
  mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
  lazy_dt() %>% 
  filter(!feature_preventing_development,
         zoning_permits_housing == "Housing permitted",
         (zone_short %in% c("Neighbourhood residential",
                            "General residential",
                            "Residential growth",
                            "Residential Growth",
                            "Mixed use"))) %>%
  filter(!(pp_propertyType %in% c("Acreage","Unit"))) %>%  #Exclude units because they're the type of land we exclude from development as being already built on. 
  mutate(prop_type_short = if_else(pp_propertyType %in% c("Apartment","Unit"),"unit","other"))  %>% 
  mutate(land_size = parse_number(pp_landSize),
         year_num = parse_number(pp_soldDate),
         year_parse = year(dmy(pp_soldDate)),
         year = coalesce(year_parse,year_num)) %>% 
  mutate(dist_rail = replace_na(pmin(prox_walk_time_s_tram,
                                     prox_walk_time_s_train,
                                     na.rm = T),9999)) %>% 
  mutate(dist_rail_fct = cut(dist_rail, breaks = c(0,
                                                   100,
                                                   200,
                                                   300,
                                                   400,
                                                   500,
                                                   800,
                                                   1000,
                                                   1500,
                                                   Inf)),
         sale_price = parse_number(pp_soldPrice)) %>%
  filter(!is.na(sale_price)) %>% 
  as_tibble() %>% 
  filter(between(year,2010,2019)) %>%
  as_tibble() %>% 
  mutate(year = fct_relevel(as.factor(year),"2010"),
         zone_short = fct_relevel(zone_short, "Neighbourhood residential"),
         heritage_status = fct_relevel(heritage_status, "No heritage"),
         dist_rail_fct = fct_relevel(dist_rail_fct, "(1.5e+03,Inf]"),
         price_per_sqm = sale_price/lot_size)  %>% 
  filter(lga_name_2022 %in% c(inner_lgas,middle_lgas)) %>% 
  mutate(mm_eligible = if_else(dist_rail < 500,T,F))


house_dataset <- all_prices %>% filter(prop_type_short == "other") %>% 
  filter(between(price_per_sqm, 
                 mean(price_per_sqm)-3*sd(price_per_sqm),
                 mean(price_per_sqm)+3*sd(price_per_sqm))) %>% 
  mutate(land_size = coalesce(land_size,lot_size)) %>% 
  filter(between(land_size,lot_size*.8,
                            lot_size*1.2)) %>% 
  filter(lot_size <3000) %>% 
  filter(dwlgs_in_2016 <2) 

train_split_pct = 0.8
n_reps = 10
n_models = 1
results_df = matrix(nrow = n_reps, ncol = n_models)

for (n in 1:n_reps) {
  sample_size <- floor(train_split_pct * nrow(all_prices))
  train_ind <- sample(seq_len(nrow(all_prices)), size = sample_size)
  
  train <- house_dataset %>% filter(row_number() %in% train_ind) %>% select(zone_short,dist_rail_fct,cbd_dist,vacant_in_2016,heritage_status,traffic_pollution,sa2_code_2021,price_per_sqm,year,lot_size)
  test <- house_dataset %>% filter(!(row_number() %in% train_ind))
  
  #run train model 1
  model_1 <- feols(price_per_sqm ~  zone_short + dist_rail_fct+ log(cbd_dist)+vacant_in_2016+heritage_status+traffic_pollution |as.factor(sa2_code_2021)+year, data = train )
  summary(model_1)
  predicted_prices <- predict(model_1, test, type='response') 
  
  #train_output <- test %>% bind_cols(tibble(predicted_price = predicted_prices)) %>% 
 #   mutate(distince = abs(predicted_price-price_per_sqm))
  RMSE_1 <-  sqrt(median((test$price_per_sqm - predicted_prices)^2,na.rm = TRUE))
  results_df[n,1] <- RMSE_1
}

print(colMeans(results_df)*250)
summary(model_1)

apartments <- all_prices %>% filter(prop_type_short == "unit") %>% 
  filter(sale_price>250000,
         sale_price<2000000)

apartment_model <- feols(sale_price ~   log(cbd_dist) +dwellings_est  +traffic_pollution |lga_name_2022+year, data = apartments)

summary(apartment_model)

predicted_prices <- predict(apartment_model, apartments, type='response') 

#train_output <- test %>% bind_cols(tibble(predicted_price = predicted_prices)) %>% 
#   mutate(distince = abs(predicted_price-price_per_sqm))
RMSE_1 <-  sqrt(median((apartments$sale_price - predicted_prices)^2,na.rm = TRUE))


house_price_inflation <- readabs::read_abs(series_id = "A83728392R",path = "data",check_local = T)%>% 
  summarise(value = value[date == ymd("2021-12-01")] /value [date == ymd("2018-03-01")]) #House prices in Mel have been relatively flat since the most recent ABS data - but a good qc would be to calculate precisely. 



full_dataset_for_prediction <- dwelling_data_raw %>% 
  lazy_dt() %>% 
  filter(zone_short %in% unique(test$zone_short)) %>% 
  mutate(year = 2018,
         dist_rail = replace_na(pmin(prox_walk_time_s_tram,
                                     prox_walk_time_s_train,
                                     na.rm = T),9999),
         dist_rail_fct = cut(dist_rail, breaks = c(0,
                                                   100,
                                                   200,
                                                   300,
                                                   400,
                                                   500,
                                                   800,
                                                   1000,
                                                   1500,
                                                   Inf))) %>% 
  filter(lga_name_2022 %in% c(inner_lgas,middle_lgas)) %>% 
  as_tibble()


predicted_apartment_prices <- predict(apartment_model, full_dataset_for_prediction, type='response') 
predicted_house_prices <- predict(model_1, full_dataset_for_prediction,
                                  type='response')* 1.285121107 #(abs property price inflation )

all_predicted_prices <- full_dataset_for_prediction %>% 
  select(lat,lon) %>% 
  bind_cols(tibble(apartment_prices = predicted_apartment_prices)) %>% 
  bind_cols(tibble(property_price_per_sqm = predicted_house_prices)) 

prices_estimates <- dwelling_data_raw %>% 
  select(lat,lon) %>% 
  left_join(all_predicted_prices) 

prices_erstimates %>% 
  write_sf("test/predicted_prices.shp")
