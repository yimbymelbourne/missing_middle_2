
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
         year = coalesce(as.integer(year_parse),as.integer(year_num))) %>% 
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


osm_linkage = fread('data/osm_linkage_to_property_db.csv')
osm_parquet = read_parquet('data/walkability_by_node.parquet')

all_prices = all_prices %>%
  left_join(osm_linkage, by = c('lat', 'lon')) %>%
  left_join(osm_parquet, by = 'osmid')

all_prices = all_prices %>% 
  rename(coffee_closest = `coffee_available - closest`,
         restaurant_closest = `restaurant - closest`,
         grocery_closest = `grocery or supermarket - closest`,
         cafe_closest = `cafe - closest`,
         bar_closest = `bar or pub - closest`,
         worship_closest = `place of worship - closest`,
         tourist_closest = `tourist attraction - closest`,
         community_area_closest = `community area - closest`,
         aged_care_closest = `aged care - closest`,
         park_closest = `park area - closest`,
         school_closest = `school - closest`,
         child_care_closest = `child care - closest`,
         library_closest = `library - closest`,
         emergency_closest = `emergency services - closest`,
         medical_closest = `medical facility - closest`,
         entertainment_closest = `entertainment centre - closest`,
         pool_closest = `swimming pool - closest`,
         tertiary_closest = `tertiary institution - closest`,
         art_closest = `art gallery - closest`,
         museum_closest = `museum - closest`
         )


all_prices = all_prices %>% mutate(across(all_of(c('coffee_closest',
                                                   'restaurant_closest',
                                                   'grocery_closest',
                                                   'cafe_closest',
                                                   'bar_closest',
                                                   'worship_closest',
                                                   'tourist_closest',
                                                   'community_area_closest',
                                                   'aged_care_closest',
                                                   'park_closest',
                                                   'school_closest',
                                                   'child_care_closest',
                                                   'library_closest',
                                                   'emergency_closest',
                                                   'medical_closest',
                                                   'entertainment_closest',
                                                   'pool_closest',
                                                   'tertiary_closest',
                                                   'art_closest',
                                                   'museum_closest')), ~ ifelse(is.na(.x), 5500, .x ) ))


all_prices = all_prices %>% mutate(across(all_of(c('coffee_closest',
                                           'restaurant_closest',
                                           'grocery_closest',
                                           'cafe_closest',
                                           'bar_closest',
                                           'worship_closest',
                                           'tourist_closest',
                                           'community_area_closest',
                                           'aged_care_closest',
                                           'park_closest',
                                           'school_closest',
                                           'child_care_closest',
                                           'library_closest',
                                           'emergency_closest',
                                           'medical_closest',
                                           'entertainment_closest',
                                           'pool_closest',
                                           'tertiary_closest',
                                           'art_closest',
                                           'museum_closest')), ~ cut(.x, breaks = c(0,500,1000, 2000,3000,4000,5000 ,Inf))))



house_dataset <- all_prices %>% filter(prop_type_short == "other") %>% 
  filter(between(price_per_sqm, 
                 mean(price_per_sqm)-3*sd(price_per_sqm),
                 mean(price_per_sqm)+3*sd(price_per_sqm))) %>% 
  mutate(land_size = coalesce(land_size,lot_size)) %>% 
  filter(between(land_size,lot_size*.8,
                 lot_size*1.2)) %>% 
  filter(lot_size <3000) %>% 
  filter(dwlgs_in_2016 <2) %>%
  mutate(ln_price_per_sqm = log(price_per_sqm))


train_split_pct = 0.8
n_reps = 10
n_models = 2
results_df = matrix(nrow = n_reps, ncol = n_models)

fml2 = as.formula("price_per_sqm ~ i(lga_name_2022, lot_size) + poly(lot_size,7) + i(zone_short, ref = 'Neighbourhood residential') + i(dist_rail_fct, ref = '(0,100]') + log(cbd_dist)+vacant_in_2016+ i(heritage_status, ref= 'No heritage') + traffic_pollution | lga_name_2022 + sa2_code_2021^year")


for (n in 1:n_reps) {
  sample_size <- floor(train_split_pct * nrow(all_prices))
  train_ind <- sample(seq_len(nrow(all_prices)), size = sample_size)
  
  train <- house_dataset %>% filter(row_number() %in% train_ind) %>% select(zone_short,dist_rail_fct,cbd_dist,vacant_in_2016,heritage_status,traffic_pollution,sa2_code_2021,price_per_sqm,year,lot_size, lga_name_2022)
  test <- house_dataset %>% filter(!(row_number() %in% train_ind))
  
  #run train model 1
  model_1 <- feols(price_per_sqm ~  zone_short + dist_rail_fct+ log(cbd_dist)+vacant_in_2016+heritage_status+traffic_pollution |as.factor(sa2_code_2021)+year, data = train )
  summary(model_1)
  predicted_prices_1 <- predict(model_1, test, type='response') 
  
  model_2 = feols(fml2, data = train, combine.quick = FALSE)
  summary(model_2)
  predicted_prices_2 <- predict(model_2, test, type='response')
  
  #train_output <- test %>% bind_cols(tibble(predicted_price = predicted_prices)) %>% 
  #   mutate(distince = abs(predicted_price-price_per_sqm))
  RMSE_1 <-  sqrt(median((test$price_per_sqm - predicted_prices_1)^2,na.rm = TRUE))
  RMSE_2 <-  sqrt(median((test$price_per_sqm - predicted_prices_2)^2,na.rm = TRUE))
  results_df[n,1] <- RMSE_1
  results_df[n,2] <- RMSE_2
}

print(colMeans(results_df))
summary(model_1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ APARTMETNS MODELLING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

apartment_fml_1 = as.formula("log(sale_price) ~ log(cbd_dist) + dwellings_est  + traffic_pollution | lga_name_2022+year")
apartment_fml_2 = as.formula("log(sale_price) ~ i(lga_name_2022, lot_size) + poly(lot_size,7) + i(zone_short, ref = 'Neighbourhood residential') + i(dist_rail_fct, ref = '(0,100]') + log(cbd_dist)+vacant_in_2016+ i(heritage_status, ref= 'No heritage') + traffic_pollution | lga_name_2022 + sa2_code_2021^year")
apartment_fml_3 = as.formula("log(sale_price) ~ i(lga_name_2022, lot_size) + poly(lot_size,7) + i(zone_short, ref = 'Neighbourhood residential') + dist_rail_fct + cbd_dist + heritage_status + traffic_pollution +
                              coffee_closest +
                                           restaurant_closest+
                                           grocery_closest+
                                           cafe_closest+
                                           bar_closest+
                                           worship_closest+
                                           tourist_closest+
                                           community_area_closest+
                                           aged_care_closest+
                                           park_closest+
                                           school_closest+
                                           child_care_closest+
                                           library_closest+
                                           emergency_closest+
                                           medical_closest+
                                           entertainment_closest+
                                           pool_closest+
                                           tertiary_closest+
                                           art_closest+
                                           museum_closest |  lga_name_2022^year")

apartments <- all_prices %>% filter(prop_type_short == "unit") %>% 
  filter(sale_price>250000,
         sale_price<2000000)

train_split_pct = 0.75
n_reps_apt = 5
n_models_apt = 4
results_df_apt = matrix(nrow = n_reps_apt, ncol = n_models_apt)

for(n in 1:n_reps_apt) {
  sample_size <- floor(train_split_pct * nrow(apartments))
  train_ind <- sample(seq_len(nrow(apartments)), size = sample_size)
  
  train <- apartments %>% filter(row_number() %in% train_ind) 
  test <- apartments %>% filter(!(row_number() %in% train_ind))
  
  #run train model 1
  apartment_model_1 <- feols(apartment_fml_1, data = train, combine.quick = FALSE)
  summary(apartment_model_1)
  predicted_prices_1 <- predict(apartment_model_1, test, type='response') 
  
  apartment_model_2 <- feols( apartment_fml_2 , data = train, combine.quick = FALSE)
  summary(apartment_model_2)
  predicted_prices_2 <- predict(apartment_model_2, test, type='response')
  
  apartment_model_3 <- feols( apartment_fml_3 , data = train, combine.quick = FALSE)
  summary(apartment_model_3)
  predicted_prices_3 <- predict(apartment_model_3, test, type='response')
  
  #train_output <- test %>% bind_cols(tibble(predicted_price = predicted_prices)) %>% 
  #   mutate(distince = abs(predicted_price-price_per_sqm))
  RMSE_1 <-  sqrt(mean(( log(test$sale_price) - predicted_prices_1)^2,na.rm = TRUE))
  RMSE_2 <-  sqrt(mean(( log(test$sale_price) - predicted_prices_2)^2,na.rm = TRUE))
  RMSE_3 <-  sqrt(mean(( log(test$sale_price) - predicted_prices_3)^2,na.rm = TRUE))
  
  results_df_apt[n,1] <- RMSE_1
  results_df_apt[n,2] <- RMSE_2
  results_df_apt[n,3] <- RMSE_3
  
  
  #glmnet stuff 
  
  train_matrix = train %>% mutate(sale_price = log(sale_price)) %>% select(!c('osmid', 'lon', 'lat', 'prox_walk_time_s_train', 'prox_walk_time_s_tram', 'pp_soldPrice', 'pp_soldDate', 'pp_address', 'dwellings_est', 'zoning_permits_housing..24', 'mm_eligible', 'year_parse', 'year_num', 'feature_preventing_development', 'vacant_in_2016', 'zoning_permits_housing', 'dist_from_osm_point_to_property', 'x', 'y', 'node_weight', 'pp_bathrooms', 'pp_bedrooms', 'pp_propertyType', 'prop_type_short', 'heritage', 'dwlgs_in_2016', 'pp_landSize', 'pp_closestSecondaryRank', 'address' , 'sa1_code_2021', 'cbd_lon', 'cbd_lat', 'geometry', 'land_size', 'dist_rail', 'price_per_sqm'))
  test_matrix = test %>% mutate(sale_price = log(sale_price)) %>% select(!c('osmid', 'lon', 'lat', 'prox_walk_time_s_train', 'prox_walk_time_s_tram', 'pp_soldPrice', 'pp_soldDate', 'pp_address', 'dwellings_est', 'zoning_permits_housing..24', 'mm_eligible', 'year_parse', 'year_num', 'feature_preventing_development', 'vacant_in_2016', 'zoning_permits_housing', 'dist_from_osm_point_to_property', 'x', 'y', 'node_weight', 'pp_bathrooms', 'pp_bedrooms', 'pp_propertyType', 'prop_type_short', 'heritage', 'dwlgs_in_2016', 'pp_landSize', 'pp_closestSecondaryRank', 'address' , 'sa1_code_2021', 'cbd_lon', 'cbd_lat', 'geometry', 'land_size', 'dist_rail', 'price_per_sqm'))

  
  train_matrix = na.omit(train_matrix)
  test_matrix = na.omit(test_matrix)
  
  colnames(train_matrix)
  
  factor_vars = c('zone_short', 'heritage_status', 'lga_name_2022', 'sa2_code_2021', 'dist_rail_fct', 'year', 'coffee_closest',
                  'restaurant_closest',
                  'grocery_closest',
                  'cafe_closest',
                  'bar_closest',
                  'worship_closest',
                  'tourist_closest',
                  'community_area_closest',
                  'aged_care_closest',
                  'park_closest',
                  'school_closest',
                  'child_care_closest',
                  'library_closest',
                  'emergency_closest',
                  'medical_closest',
                  'entertainment_closest',
                  'pool_closest',
                  'tertiary_closest',
                  'art_closest',
                  'museum_closest')
  
  
  
  train_factor_matrix = model.matrix(~ zone_short + heritage_status + lga_name_2022:year + sa2_code_2021 + dist_rail_fct + coffee_closest + 
                                       restaurant_closest+
                                       grocery_closest+
                                       cafe_closest+
                                       bar_closest+
                                       worship_closest+
                                       tourist_closest+
                                       community_area_closest+
                                       aged_care_closest+
                                       park_closest+
                                       school_closest+
                                       child_care_closest+
                                       library_closest+
                                       emergency_closest+
                                       medical_closest+
                                       entertainment_closest+
                                       pool_closest+
                                       tertiary_closest+
                                       art_closest+
                                       museum_closest, train_matrix)
  
  test_factor_matrix =  model.matrix(~ zone_short + heritage_status + lga_name_2022:year + sa2_code_2021 + dist_rail_fct + coffee_closest +
                                       restaurant_closest+
                                       grocery_closest+
                                       cafe_closest+
                                       bar_closest+
                                       worship_closest+
                                       tourist_closest+
                                       community_area_closest+
                                       aged_care_closest+
                                       park_closest+
                                       school_closest+
                                       child_care_closest+
                                       library_closest+
                                       emergency_closest+
                                       medical_closest+
                                       entertainment_closest+
                                       pool_closest+
                                       tertiary_closest+
                                       art_closest+
                                       museum_closest, test_matrix)
  
  train_matrix = train_matrix %>% select(!any_of(factor_vars))
  test_matrix = test_matrix %>% select(!any_of(factor_vars))
  
  train_matrix = data.frame(train_factor_matrix,train_matrix)
  test_matrix = data.frame(test_factor_matrix,test_matrix)
 
  y_train_matrix = (as.matrix(train_matrix$sale_price))
  y_test_matrix = (as.matrix(test_matrix$sale_price))
  
  train_matrix = (as.matrix(train_matrix %>% select(!sale_price)))
  test_matrix = (as.matrix(test_matrix %>% select(!sale_price)))
  
  #train_matrix = remove_constant(train_matrix)
  #test_matrix = remove_constant(test_matrix)
  
  #cvfit <- cv.glmnet(train_matrix, y_train_matrix, family = 'gaussian')
  #best_lambda = cvfit$lambda.min
  best_lambda = 0.0000001
  plot(cvfit)
  
  model_4 <- glmnet(train_matrix, y_train_matrix, alpha = 1, lambda = best_lambda, family = 'gaussian', standardize = FALSE, intercept = TRUE)
  predicted_prices_4 <- predict(model_4, test_matrix)
  
  RMSE_4 <-  sqrt( mean(( y_test_matrix - predicted_prices_4)^2, na.rm = TRUE))
  results_df_apt[n,4] <- RMSE_4
}

adf = data.frame( apartments[unlist(apartment_model_2$obs_selection),] %>% 
                    mutate(ln_price = log(sale_price)) %>%
                    select(zone_short, heritage_status, lga_name_2022, ln_price)
                  , apartment_model_2$residuals, apartment_model_2$fitted.values)
#tibble(adf)

print(colMeans(results_df_apt))

#predicted_prices <- predict(apartment_model, apartments, type='response') 
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


predicted_apartment_prices <- predict(apartment_model_2, full_dataset_for_prediction, type='response') 
predicted_house_prices <- predict(model_2, full_dataset_for_prediction,
                                  type='response')* 1.285121107 #(abs property price inflation )

all_predicted_prices <- full_dataset_for_prediction %>% 
  select(lat,lon) %>% 
  bind_cols(tibble(apartment_prices = exp(predicted_apartment_prices))) %>% 
  bind_cols(tibble(property_price_per_sqm = predicted_house_prices)) 

prices_estimates <- dwelling_data_raw %>% 
  select(lat,lon) %>% 
  left_join(all_predicted_prices) 

prices_estimates %>% 
  write_sf("test/predicted_prices.shp")

rmarkdown::render("r_experimental/model-qc.Rmd",output_format = "html_document",clean = T)
