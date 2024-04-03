library(data.table)
library(sf)
library(glmnet)
library(tidyverse)
library(fixest)

housing_data = fread('pricing_regression/ksou.csv/ksou.csv')
housing_data$longitude = as.character(housing_data$longitude)
housing_data = housing_data %>% drop_na(c('longitude', 'latitude', 'soldPrice'))

#gc()

#cleaing and processing

x_vars = c('suburb', 'soldPrice', 'soldDate', 'propertyType', 'bedrooms', 'carSpots', 'landSize', 'distanceToCBD', 'distanceToTransport', 'closestPrimaryDistance', 'latitude', 'longitude')

x = as.data.frame(housing_data) %>% select(all_of(x_vars)) %>% 
  mutate(soldPrice = gsub("[\\$,]", "", soldPrice)) %>% 
  filter(!is.na(soldPrice)) %>% 
  filter(soldPrice != 'undisclosed')
x = slice_sample(x, prop = 1)
x$soldPrice = log(as.numeric(x$soldPrice))
x = x %>% filter(!is.infinite(soldPrice))
x = as.data.table(x)

#x = x %>% rowwise() %>% mutate(across(all_of('landSize'), ~ str_replace(gsub("\\D", "", .), "," ,"")))
x[ , (c('landSize')) := lapply(.SD, FUN = function(dt) {str_replace(gsub("\\D", "", dt), "," ,"")}), .SDcols = c('landSize')]
x[ , (c('distanceToCBD')) := lapply(.SD, cleanDistances), .SDcols = c('distanceToCBD')]


#for (j in c('distanceToCBD')) set(x, j = j, value = cleanDistances(x[[j]]))
#x = x %>% rowwise() %>% mutate(distanceToCBD = if_else( grepl('km', distanceToCBD, fixed = TRUE) , as.numeric(str_extract(distanceToCBD, "\\d+\\.*\\d*")) * 1000 , as.numeric(str_extract(distanceToCBD, "\\d+\\.*\\d*")) ) )

#get rid of houses clearly out of Melbourne
x = x %>% filter(distanceToCBD < 50000)

cols = c('distanceToTransport', 'closestPrimaryDistance')
x[ , (cols) := lapply(.SD, cleanDistances), .SDcols = cols]

cleanDistances = function(x) {
  return( if_else( grepl( 'km', x, fixed = TRUE) , as.numeric(str_extract(x, "\\d+\\.*\\d*")) * 1000 , as.numeric(str_extract(x, "\\d+\\.*\\d*"))))
}

#x = x %>% rowwise() %>% mutate_at(c('distanceToTransport', 'closestPrimaryDistance', 'closestSecondaryDistance'), if_else( grepl('km', distanceToTransport, fixed = TRUE) , as.numeric(str_extract(distanceToTransport, "\\d+\\.*\\d*")) * 1000, as.numeric(str_extract(distanceToTransport, "\\d+\\.*\\d*"))  ))

#x = x %>% rowwise() %>% mutate(distanceToTransport = if_else( grepl('km', distanceToTransport, fixed = TRUE) , as.numeric(str_extract(distanceToTransport, "\\d+\\.*\\d*")) * 1000, as.numeric(str_extract(distanceToTransport, "\\d+\\.*\\d*"))  ))
#x = x %>% rowwise() %>% mutate(closestPrimaryDistance = if_else( grepl( 'km', closestPrimaryDistance, fixed = TRUE) , as.numeric(str_extract(closestPrimaryDistance, "\\d+\\.*\\d*")) * 1000 , as.numeric(str_extract(closestPrimaryDistance, "\\d+\\.*\\d*")) ))
#x = x %>% rowwise() %>% mutate(closestSecondaryDistance = if_else( grepl( 'km', closestSecondaryDistance, fixed = TRUE) , as.numeric(str_extract(closestSecondaryDistance, "\\d+\\.*\\d*")) * 1000 , as.numeric(str_extract(closestSecondaryDistance, "\\d+\\.*\\d*")) ))

#x = x %>% select(!c(distanceToCBD, distanceToTransport, closestPrimaryDistance))
#x = x %>% mutate_at( c("distanceToTransport_m", "closestPrimaryDistance_m", "closestSecondaryDistance_m") , ~ na_if(., ''))

x = x %>% mutate(month = str_extract(soldDate, "\\b[A-Za-z]{3}\\b"),
                 year = str_extract(soldDate, "\\b\\d{4}\\b"))

x = x %>% filter(as.numeric(year) <= 2023)

#drop messed up suburbs

x = x %>% group_by(suburb) %>% filter(n() >= 50)

#impute mean values at a suburb level
x$landSize = as.numeric(x$landSize)
x = x %>% group_by(suburb) %>% mutate_at(c('carSpots', 'bedrooms', 'distanceToTransport', 'closestPrimaryDistance', 'landSize'), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)  )
x = x %>% group_by(suburb) %>% mutate_at(c('carSpots', 'bedrooms', 'distanceToTransport', 'closestPrimaryDistance', 'landSize'), ~ifelse(is.nan(.), mean(., na.rm = TRUE), .)  )

x = na.omit(x)

x$year_int = as.numeric(x$year)

x = x %>% filter(year_int > 2000)

colMeans(is.na(x))

#test-train split

sample <- sample(c(TRUE, FALSE), nrow(x), replace=TRUE, prob=c(0.7,0.3))

train  <- x[sample, ]
test   <- x[!sample, ]
test = test %>% filter((year %in% train$year))

#one hot encode the variables

y_train = train$soldPrice

x_train_matrix <- model.matrix( ~ 0 + propertyType + bedrooms + carSpots + landSize + distanceToCBD + distanceToTransport + closestPrimaryDistance + month + year + suburb + year_int , train)
x_test_matrix <- model.matrix( ~ 0 + propertyType + bedrooms + carSpots + landSize + distanceToCBD + distanceToTransport + closestPrimaryDistance + month + year + suburb + year_int , test)
#

#test and train need to have the same categorical variables (remove any not in the overlap)
x_train_matrix = x_train_matrix[, intersect(colnames(x_train_matrix), colnames(x_test_matrix))]
x_test_matrix = x_test_matrix[, intersect(colnames(x_train_matrix), colnames(x_test_matrix))]


#y_train_scaled = scale(y_train, center = TRUE, scale = FALSE)
#x_train_scaled_matrix = scale(x_train_matrix, center = TRUE, scale = FALSE)
#x_test_scaled_matrix = scale(x_test_matrix, center = TRUE, scale = FALSE)
#predicting house prices in constant x year dollars

cv_model <- cv.glmnet(x_train_matrix, y_train, alpha = 1, standardize = TRUE)

plot(cv_model) 

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
lambda <- best_lambda
#print(best_lambda)

best_model <- glmnet(x_train_matrix, y_train, alpha = 1, lambda = lambda, standardize = TRUE, family = 'gaussian')

linear_model <- feols(as.numeric(soldPrice) ~ as.factor(propertyType) + bedrooms + as.factor(propertyType) + carSpots + landSize + distanceToCBD + distanceToTransport + closestPrimaryDistance + as.factor(month) + as.factor(year) + as.factor(suburb) + as.factor(suburb)*as.factor(propertyType), data = train)

#evaluating model fit

#run predictions on test set

test_LASSO_predictions = predict.glmnet(best_model, x_test_matrix)
test_LINEAR_predictions = predict(linear_model,test)

mse_LASSO = mean(as.vector(as.numeric(test$soldPrice) - as.vector(test_LASSO_predictions))^2)
mse_LINEAR = mean(as.vector(as.numeric(test$soldPrice) - as.vector(test_LINEAR_predictions))^2)

test_comparisons = data.frame(as.numeric(test$soldPrice), test_LASSO_predictions, test_LINEAR_predictions, test)
test_comparisons$diff_real_linear = as.numeric(test_comparisons$soldPrice) - test_comparisons$test_LINEAR_predictions


#now imaging every house was sold in 2018

#new_matrix = as.data.frame(x) %>% mutate(across(contains("year"), ~ 0))
#new_matrix$year2018 = 1

#x_new_scaled_matrix = scale(as.matrix(new_matrix), center = TRUE, scale = FALSE)

#fitted_values = predict.glmnet(best_model, x_new_scaled_matrix, type = 'response')

#sample_Df = data.frame(fitted_values, y, x$year)

#linear regression alternative

new_linear_df = as.data.frame(x) %>% mutate(year = 2018)
linear_fitted_values = predict(linear_model, new_linear_df)
#joining to dwelling data

housing_data_with_pred = as.data.frame(x) %>% mutate(pred_2018 = linear_fitted_values)
#housing_data_with_pred$pred_2018 = linear_fitted_values

housing_data_with_pred = st_as_sf(housing_data_with_pred, coords = c('longitude', 'latitude'), na.fail = FALSE)
housing_data_with_pred = st_set_crs(housing_data_with_pred, 7844)

dwelling_data_raw <-read_sf("data/dwelling_data.gpkg")
joined <- st_join( dwelling_data_raw %>% slice_sample(prop = 1), housing_data_with_pred %>% slice_sample(prop = 1)) 

jc = joined %>% filter(!is.na(pred_2018))


final_regression <- feols( pred_2018/lot_size ~ i(zone_short, ref = "Neighbourhood residential") + i(heritage_status, ref = "No heritage") + prox_walk_dist_m_bus + traffic_pollution, fixef = c('lga_name_2022', 'sa2_code_2021'), data = jc)
etable(final_regression)

#price_per_lot_size = predict(final_regression, joined)


#regression without 2018 preds
reg <- feols( soldPrice/lot_size ~ i(zone_short, ref = "Neighbourhood residential") + i(heritage_status, ref = "No heritage") + prox_walk_dist_m_bus + traffic_pollution | sa2_code_2021 + year + month + lga_name_2022 + sa2_code_2021^year ,
              
              data = jc %>% filter(propertyType %in% c('House', 'Townhouse')) %>% filter(feature_type_short == 'developable land'))

etable(reg)

etable(reg, final_regression)




