
#one-time use
#renv::init()
#save packages status
#renv::snapshot()
#load package from save file
renv::restore()

library(DBI)
library(RPostgres)
library(sf)
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(furrr)
library(strayr)
library(showtext)
library(ggtext)
library(scales)
library(gt)
library(ggridges)
library(aws.s3)
library(dtplyr)
library(fixest)
library(readabs)
library(janitor)
library(datawizard)

if(Sys.info()[7] == "jonathannolan1") {
# create connection to postgres
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)


dwelling_data_raw <- read_sf(con,
                             "dwelling_data_clean")

} else{ print("Make sure you have the dwelling map file inside your data folder', you can download it here https://github.com/jonathananolan/Melbourne-dwelling-map 
              selecting down to a smaller group of variables, so if your code breaks you might need to add more variables to the list in 00renv.R")

dwelling_data_raw <-read_sf("data/Melbourne dwelling data.gpkg", query = "SELECT geom,lat,lon,zone_short,sa1_code_2021,
                            dwellings_est,sa2_code_2021,sa4_code_2021,
                              lga_name_2022,feature_preventing_development,zoning_permits_housing,zone_short,prox_walk_time_s_tram,
                            prox_walk_time_s_train,prox_dist_m_tram,prox_dist_m_train,traffic_pollution,lot_size,zone_short,sa3_code_2021,heritage_status,heritage,vacant_in_2016 FROM 'Melbourne dwelling data'") %>% 
  mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>% 
  mutate(cbd_lon = 144.962646,
         cbd_lat = -37.810272)%>%
  rowwise() %>% 
  mutate(cbd_dist = geosphere::distHaversine(c(lon, lat), 
                                  c(cbd_lon, cbd_lat))) %>% 
  ungroup() %>% 
  select(-cbd_lon,
         -cbd_lat) %>% 
  st_set_geometry('geom')

 }

options(digits=6)


meters_to_numeric <- function(distances) {
  # Check if the units attribute exists and is in meters
  if (inherits(distances, "units") && attr(distances, "units") == "m") {
    # Convert to numeric
    distances_numeric <- as.numeric(distances)
    return(distances_numeric)
  } else {
    # Raise an error if units are not meters
    stop("Distance units are not in meters.")
  }
}


source("theme/yimby_melbounre_ggplot_theme.R")

inner_lgas <- c("Melbourne",
                "Yarra",
                "Moreland",
                "Port Phillip",
                "Stonnington",
                "Maribyrnong")

middle_lgas <- c("Boroondara",
                 "Darebin",
                 "Glen Eira",
                 "Banyule",
                 "Bayside",
                 "Bayside (Vic.)",
                 "Hobsons Bay",
                 "Kingston",
                 "Kingston (Vic.)",
                 "Manningham",
                 "Monash",
                 "Moonee Valley",
                 "Moreland",
                 "Whitehorse",
                 "Greater Dandenong",
                 "Maroondah",
                 "Brimbank",
                 "Frankston")

outer_lgas <- c("Knox",
                "Mornington Peninsula",
                "Nillumbik",
                "Yarra Ranges")

greenfield <- c("Cardinia",
                "Casey",
                "Hume",
                "Melton",
                "Whittlesea",
                "Wyndham")

mel_lgas <- c(inner_lgas,middle_lgas,outer_lgas)
