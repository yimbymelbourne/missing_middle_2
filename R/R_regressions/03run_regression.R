
library(data.table)
library(arrow)
#library(multidplyr)

if(!exists("dwelling_data_raw")){
source("R/functions/00renv.R")
}

#Create actual regression dataset with price specific variables and attempts to filter out messy data


#This dataset is price data that we've merged with information we already have about each property so we can regress both bits of information against price
dwellings_with_prices <- read_csv("data/dwellings_with_prices.csv")

#get data on which sa1 is in which sa3 which was deleted from the previous dataframe a little too early! 
sa3_dataframe <- strayr::read_absmap("sa12021") %>% 
  st_drop_geometry() %>% 
  select(sa1_code_2021,sa3_code_2021,sa4_code_2021)


fix_errant_sa3 <- function(data){
  print("One sa3 only has 2 obs, let's just assign it to a nearby one so fixed effect regressions work better")
  output <- data %>% 
  mutate(sa3_code_2021 = if_else(sa3_code_2021 == "20903","20901",sa3_code_2021) ,
         sa3_code_2021 = if_else(sa3_code_2021 == "21105","21104",sa3_code_2021))
  return(output)
}



#Walkability metrics are ready to go but I've commented them out just for now to save ram.
#Import walkability metrics to use in the regression
osm_linkage = fread('data/osm_linkage_to_property_db.csv')

#https://tompisel.com/data/walkability_by_node.parquet
 osm_parquet = read_parquet('data/walkability_by_node.parquet')%>%
   janitor::clean_names() %>%
   select(c(osmid,contains("closest")))

walkability_metrics <- names(osm_parquet)[-1]
#walkability_metrics <- "walkability not included"
#Function to create regression and prediction ready datasets. (function because we do the same things twice on two datasets)

clean_data_set_for_regression <- function(x){
  
  output <- x %>% 
    lazy_dt() %>% 
    mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
    mutate(lga_name_2022 = if_else(lga_name_2022 == "Moreland","Merri-bek",lga_name_2022)) %>%  # Moreland changed name in 2021
    mutate(zone_short = if_else(zone_short == "Residential Growth","Residential growth",zone_short)) %>% # Old version of the file mispelt "Growth" - should be fixed but just in case...
    mutate(sa1_code_2021 = as.character(sa1_code_2021)) %>%  #Make sure that R doesn't treat sa3s as numerical when doing regression
    filter(!is.na(sa1_code_2021)) %>% # Only about 20 obs missing an sa1 because they're in weird locations - and they're mostly civic land. 
    filter(lga_name_2022 %in% c(inner_lgas,middle_lgas)) %>% # We're only concerned with missing middle. 
    filter(!feature_preventing_development, #We don't want to build on schools, hospitals etc. 
           zoning_permits_housing == "Housing permitted", # We don't want to build on bits of land that are zoned industrial or commercial only etc. 
           (zone_short %in% c("Neighbourhood residential", # Also lets filter out low density residential which is a semi-rural zone that's quite uncommon. Also "rural/regional" (? might be some of that in green wedge) 
                              "General residential",
                              "Residential growth",
                              "Residential Growth",
                              "Mixed use"))) %>%
    left_join(osm_linkage, by = c('lat', 'lon')) %>%
    left_join(osm_parquet, by = 'osmid') %>% 
    mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% # Some lgas have "(Vic.)" in it to separate it from same name LGAs in ohter states - let's make sure that doesn't happen
    mutate(dist_rail = replace_na(pmin(prox_walk_time_s_tram, # we want to regress on PT distance, regardless of tram/train
                                       prox_walk_time_s_train,
                                       na.rm = T),9999)) %>% #Our pt distance measurer is "NA" if the distance is over ~2km - since this is all becomming factor variables anyway let's roof out at a high number.  
    group_by(sa1_code_2021) %>% #if any of our location variables are missing, take the average of the small area (this is <100 obs in the price dataset)
    mutate(across(any_of(c(walkability_metrics,"dist_rail","traffic_pollution")), ~replace_na(.x,mean(.x, na.rm = TRUE)))) %>% # Not sure why some walkability metrics are na... It's a small number though. 
    ungroup() %>% 
    mutate(across(any_of(c(walkability_metrics,"dist_rail")), ~replace_na(.x,5001))) %>% #Seem to have the same problem with NAs persisting despite asking politely! This may well have helped. 
    mutate(across(any_of(c("dist_rail","lot_size","traffic_pollution",walkability_metrics,"cbd_dist")), ~winsorize(.x,threshold = .05),.names = "wins_{.col}")) %>% #Windsorize turns outliers into the 95th percentile
    mutate(across(any_of(paste0("wins_",c("dist_rail","lot_size","traffic_pollution",walkability_metrics,"cbd_dist"))), ~log(.x),.names = "ln_{.col}")) %>%  #Log sometimes helpful for regressions
    mutate(across(any_of(paste0("wins_",c("dist_rail","lot_size","traffic_pollution",walkability_metrics,"cbd_dist"))), ~1/(.x),.names = "inv_{.col}")) %>%  #inverse sometimes helpful for regressions
    mutate(fct_lot_size = cut(lot_size, breaks = c(0, 300, 500, 750, 1000, 1250,Inf),right = F)) %>% #Create various facotr variables for the next 5 lines
    mutate(across(any_of(c(walkability_metrics,"dist_rail")), ~cut(.x, breaks = c(0,100,200,300,400,500,800,Inf),right = F), .names = "fct_{.col}")) %>% 
    mutate(fct_bedrooms =  case_when(pp_bedrooms == 1 ~ "1",
                                     pp_bedrooms == 2 ~ "2",
                                     pp_bedrooms >  2 ~ "3 or more"),
           fct_bathrooms =  case_when(pp_bathrooms == 1 ~ "1",
                                      pp_bathrooms == 2 ~ "2",
                                      pp_bathrooms >  2 ~ "3 or more"),
           fct_traffic_pollution = cut(traffic_pollution,breaks = c(seq(0,1,.1),Inf),right = F))%>% 
    as_tibble() 
  
  return(output)
  
}




all_prices <- dwellings_with_prices %>% 
  clean_data_set_for_regression() %>% 
  left_join(sa3_dataframe, by = "sa1_code_2021") %>% 
  fix_errant_sa3() %>% 
  lazy_dt() %>% 
  filter(!(pp_propertyType %in% c("Acreage"))) %>%  #Exclude units because they're the type of land we exclude from development as being already built on. 
  mutate(prop_type_short = if_else(pp_propertyType %in% c("Apartment","Unit"),"unit","other"))  %>% 
  mutate(land_size  = suppressWarnings(parse_number(pp_landSize)),
         year_num   = parse_number(pp_soldDate),
         year_parse = suppressWarnings(year(dmy(pp_soldDate))),
         year       = coalesce(as.integer(year_parse),as.integer(year_num)),
         sale_price = suppressWarnings(parse_number(pp_soldPrice))) %>% 
  filter(!is.na(sale_price)) %>% 
  as_tibble() %>% 
  filter(between(year,2010,2018)) %>% # We chose 2010 as the earliest date semi-randomly. 
  mutate(year = fct_relevel(as.factor(year),"2010"),
         zone_short = fct_relevel(zone_short, "Neighbourhood residential"),
         heritage_status = fct_relevel(heritage_status, "No heritage"),
         across(any_of(paste0("fct_",c(walkability_metrics,"dist_rail"))),~fct_relevel(.x, "[800,Inf)")),
         fct_lot_size = fct_relevel(fct_lot_size,"[1.25e+03,Inf)"))


house_dataset <- all_prices %>% 
  filter(prop_type_short == "other") %>% 
  filter(lot_size>150, #small lots are likely data errors, and big lots are also not often included in the house price dataset so let's exclude. 
         lot_size <3000) %>% 
  mutate(price_per_sqm = sale_price/lot_size) %>% 
  filter(between(price_per_sqm, 
                 mean(price_per_sqm)-3*sd(price_per_sqm),
                 mean(price_per_sqm)+3*sd(price_per_sqm))) %>% # Exclude price outliers
  mutate(land_size = coalesce(land_size,lot_size)) %>%  # Exclude cases where sales data and us disagree on how big the lot is (often data entry errors on the sales data)
  filter(between(land_size,lot_size*.8,
                 lot_size*1.2)) %>% 
  filter(dwlgs_in_2016 <2) %>%  # Exclude anything we think might be apartments despite sales data not suggesting that That's cases where there were more than 1 dwellings in 2016
  filter(!is.na(sale_price))

apartment_dataset <- all_prices %>% 
  filter(prop_type_short == "unit") %>% 
  filter(dwellings_est >6) %>% #Try and exclude townhouses 
  filter(sale_price>250000, #Exclude errors. 
         sale_price<2000000) %>% 
  filter(!is.na(sale_price)) %>% 
  mutate(dwellings_est = replace_na(dwellings_est,0)) %>% #this is probably not ideal... maybe should filter  
  filter(!is.na(wins_traffic_pollution)) %>%  #small number to exclude 
  filter(!is.na(pp_bedrooms)) #only needed if bedrooms are included in the estimate



#This code runs the regression for both house and apartment dataset, it's designed to make it easier for us to run lots of different regression specifications. 
regression_runner <- function(dataframe, outcome_var, input_vars,interaction_vars = NA, fixed_vars=NA, n_reps = 10, train_split_pct = 0.99,log_outcome = F) {
  
  if(log_outcome){ dataframe <- dataframe %>% mutate(across(any_of(outcome_var),~log(.x)))}
  results_df <- tibble(RMSE = numeric(), run = integer(), formula = character(), run_time = POSIXct())
  models_list <- list()
  
  base_var_char <- paste0(input_vars, collapse = "+")
  
  if(!is.na(fixed_vars[1])){ fixed_var_char <- paste0("|",paste0(fixed_vars, collapse = "+")) }else{
    fixed_var_char <- c("")
  }
  if(!is.na(interaction_vars[1])){ interactions_var_char <- paste0("+",paste0(interaction_vars, collapse = ":")) }else{
    interactions_var_char <- c("")
  }
    
  formula = as.formula(paste0(outcome_var, "~",base_var_char ,interactions_var_char,fixed_var_char))
  
  print(formula)

    for (n in 1:n_reps) {
    sample_size <- floor(train_split_pct * nrow(dataframe))
    train_ind <- sample(seq_len(nrow(dataframe)), size = sample_size)
    
    train <- dataframe %>% filter(row_number() %in% train_ind)
    test <- dataframe %>% filter(!(row_number() %in% train_ind))
    
    model_output <- feols(formula, data = train)
    predicted_prices <- predict(model_output, newdata = test, type = 'response')
    
    RMSE <- sqrt(mean(((test[[outcome_var]]) - predicted_prices)^2, na.rm = TRUE))
    
    # Use deparse with a higher width.cutoff to prevent splitting
    formula_str <- deparse(formula, width.cutoff = 500)
    
    results_df <- bind_rows(results_df, tibble(RMSE = RMSE, 
                                               run = n, 
                                               formula = formula_str, 
                                               run_time = Sys.time()))
    models_list[[n]] <- model_output
  }
  print(paste0("Average error:",mean(results_df$RMSE)))
  print(paste0("This is not right but haven't figure out how to fix it: exp of average error:",exp(mean(results_df$RMSE)))) 
  return(list(results = results_df, models = models_list))
}


#Now to run the regressions!
#Because of collinearity I've removed walkability down to the bare essentials, being near a park, school or cafe. but we could add a couple more. 

#Each of these have been tested to give the best relationship:
#ggplot(house_dataset) +
#  geom_smooth(aes(x = wins_cbd_dist, y = log(price_per_sqm)))


# Logic for first run: We know that too many walkability metrics results in colinearity, so I'm trying without any. year is fixed effect.

minimal_regression_terms <- c("fct_dist_rail",
                              "vacant_in_2016",
                              "heritage_status",
                              "fct_traffic_pollution",
                              paste0("fct_",walkability_metrics), #Tried some the walk ability metrics but they all produced counter-intuitive results. I've commented out for now just to save ram but please try again Paul! 
                              "fct_lot_size") 

base_regression_terms  <- c(minimal_regression_terms,
                            "wins_cbd_dist")


house_price_model_using_logs <- F
house_price_model <- regression_runner(house_dataset, "price_per_sqm", base_regression_terms,fixed_vars = c("sa3_code_2021","year"),log_outcome = house_price_model_using_logs)

summary(house_price_model$models[[1]])

#works ok but could be better. 
#ggplot(apartment_dataset) +
#  geom_smooth(aes(x = ln_wins_traffic_pollution, y = log(sale_price)))

apartment_minimal_regression_terms <- c(#"dwellings_est",#  excluded because past apartments on big lots don't reflect better MM design standards
                                        "fct_dist_rail", 
                                        #"lot_size", # excluded because past apartments on big lots don't reflect better MM design standards
                                        "wins_cbd_dist",
                                        "fct_bedrooms",
                                        "fct_bathrooms",
                                        "heritage_status",
                                        "fct_traffic_pollution",
                                        "zone_short")

print("Because we are testing the model, sometimes this line of code has to be run a few times to get a good breakdown between test and train models, just run it again if it breaks!")

apartment_price_model_using_logs <- T
apartment_price_model <- regression_runner(apartment_dataset, "sale_price", c(apartment_minimal_regression_terms,"sa3_code_2021"),fixed_vars = c("year"),log_outcome = apartment_price_model_using_logs)
summary(apartment_price_model$models[[1]])

apartment_price_model_draft <- regression_runner(apartment_dataset, "sale_price", apartment_minimal_regression_terms,fixed_vars = c("year","sa4_code_2021"),log_outcome = apartment_price_model_using_logs)
summary(apartment_price_model$models[[1]])

#apartment_price_model <- regression_runner(apartment_dataset, "sale_price", c(apartment_minimal_regression_terms,"dwellings_est"),fixed_vars = c("sa3_code_2021","year")) # Didn't help much - introduced collineraity




#Import our predictions back into the new full model. 
full_dataset_for_prediction <- dwelling_data_raw %>%
  st_drop_geometry() %>% 
  select(any_of(c(names(dwellings_with_prices), "sa3_code_2021","sa4_code_2021"))) %>%  #Reduce ram usage for bigger dataset
  fix_errant_sa3() %>% 
  mutate(year         = 2018,
         pp_bedrooms  = 2,
         pp_bathrooms = 2) %>% 
  clean_data_set_for_regression()


if(apartment_price_model_using_logs) {
  predicted_apartment_prices <- exp( (predict(apartment_price_model$models[[1]], newdata = full_dataset_for_prediction, type='response') ))
} else {
  predicted_apartment_prices <- ( (predict(apartment_price_model$models[[1]], newdata = full_dataset_for_prediction, type='response') ))
}

if(house_price_model_using_logs) {
  predicted_house_prices <- exp(predict(house_price_model$models[[1]],     newdata = full_dataset_for_prediction, type='response'))
} else {
  predicted_house_prices <- (predict(house_price_model$models[[1]],     newdata = full_dataset_for_prediction, type='response'))
}

                            

#Now get rid of all the variables that we've changed, so we only have the id of ecah property (lat/lon), and the price estimates. 
all_predicted_prices <- full_dataset_for_prediction %>% 
  select(lat,lon) %>% 
  bind_cols(tibble(apartment_price = predicted_apartment_prices)) %>% 
  bind_cols(tibble(property_price_per_sqm = (predicted_house_prices))) 

#QC for mapping in QGIS or similar. 

dwelling_data_raw %>% 
  select(lat,lon) %>% 
 inner_join(all_predicted_prices) %>% 
  rename(apt   = apartment_price, 
         house = property_price_per_sqm) %>% 
  write_sf("test/predicted_prices.shp", quiet = T)

#RMD that looks at price predictions and sees if they make sense. These use a simple and quicker way to estimate profitability that is not quite right but good for testing. 
#rmarkdown::render("R/experimental/model-qc.Rmd",output_format = "html_document",clean = T)

#When you're finished you can clean up your ram by removing these objects. Now is time to move on to run_rmd.R! 
#rm(full_dataset_for_prediction,dwellings_with_prices,sa3_dataframe,apartment_dataset,house_dataset,all_prices)
