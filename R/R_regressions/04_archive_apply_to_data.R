

# Assuming 'con' is your database connection
sql_query <- 'SELECT lon,lat,
zone_short,
heritage_status,
heritage,
feature_preventing_development,
prox_walk_time_s_train,
prox_walk_time_s_tram,
vacant_in_2016,
lga_name_2022,
sa2_code_2021,
"zoning_permits_housing",
address,
dwellings_est,
lot_size,
traffic_pollution,
zoning_permits_housing,
sa1_code_2021 FROM dwelling_data_clean'

all_dwellings <- dbGetQuery(con,sql_query) %>% 
  mutate(cbd_lon = 144.962646,
         cbd_lat = -37.810272)%>%
  rowwise() %>% 
  mutate(cbd_dist = distHaversine(c(lon, lat), 
                                  c(cbd_lon, cbd_lat)),
         year = 2018,
         pp_bedrooms = 2,
         pp_bathrooms = 2) %>% 
  lazy_dt() %>% 
  filter(!feature_preventing_development,
         zoning_permits_housing == "Housing permitted",
         (zone_short %in% c("Neighbourhood residential",
                            "General residential",
                            "Residential Growth",
                            "Mixed use")),
         dwellings_est <2) %>% 
  mutate(dist_rail = replace_na(pmin(prox_walk_time_s_tram,
                                     prox_walk_time_s_train,na.rm = T),9999)) %>% 
  mutate(dist_rail_fct = cut(dist_rail, breaks = c(0,
                                                   100,
                                                   200,
                                                   300,
                                                   400,
                                                   500,
                                                   800,
                                                   1000,
                                                   1500,
                                                   Inf))) %>%
  as_tibble()



dwellings_with_prices <- predict(house_price_model, newdata = all_dwellings)

apartment_prices <- predict(apartment_model, newdata = all_dwellings)

lat_lon_with_prices <- all_dwellings %>% 
  select(lat,lon,cbd_dist) %>% 
  bind_cols(tibble(apartment_price_2018 = apartment_prices,
                                                       property_price_p_sqm_2018 = dwellings_with_prices))



dbWriteTable(con, "temp_lat_lon_with_prices", lat_lon_with_prices, overwrite = TRUE, row.names = FALSE)
  
# Begin the transaction
dbExecute(con, "BEGIN;")

# Alter the table to add new columns if they don't exist
dbExecute(con, "
ALTER TABLE dwelling_data_clean
ADD COLUMN IF NOT EXISTS cbd_dist DOUBLE PRECISION,
ADD COLUMN IF NOT EXISTS apartment_price_2018 DOUBLE PRECISION,
ADD COLUMN IF NOT EXISTS property_price_p_sqm_2018 DOUBLE PRECISION;
")

# Update the table with the data from the temporary table
dbExecute(con, "
UPDATE dwelling_data_clean ddc
SET
  cbd_dist = tmp.cbd_dist,
  apartment_price_2018 = tmp.apartment_price_2018,
  property_price_p_sqm_2018 = tmp.property_price_p_sqm_2018
FROM temp_lat_lon_with_prices tmp
WHERE ddc.lat = tmp.lat AND ddc.lon = tmp.lon;
")

# Commit the transaction
dbExecute(con, "COMMIT;")

# Drop the temporary table
dbExecute(con, "DROP TABLE temp_lat_lon_with_prices;")

