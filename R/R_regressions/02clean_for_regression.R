

source("R/00renv.R")


library(geosphere)

#Import relevant columns
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
"pp_soldPrice",
"pp_soldDate",
address,
pp_address,
dwlgs_in_2016,
lot_size,
traffic_pollution,
"pp_landSize",
pp_bathrooms,
geometry, 
dwellings_est,
zoning_permits_housing,
"pp_closestSecondaryRank",
pp_bedrooms,

sa1_code_2021,
"pp_propertyType" FROM dwellings_with_prices'

dwellings_with_prices <- read_sf(con,query = sql_query) %>% 
  mutate(cbd_lon = 144.962646,
         cbd_lat = -37.810272)%>%
  rowwise() %>% 
  mutate(cbd_dist = distHaversine(c(lon, lat), 
                                  c(cbd_lon, cbd_lat)))


dwellings_with_prices %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  write_csv("dwellings_with_prices.csv")


