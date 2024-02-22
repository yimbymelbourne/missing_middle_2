
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

# if(Sys.info()[7] == "jonathannolan") {
# # create connection to postgres 
# con <- dbConnect(RPostgres::Postgres(),
#                  host = 'localhost', # host name, can be website/server
#                  port = 5433, # default port of postgres
#                  dbname = 'postgres', # name of database in postgres
#                  user = 'postgres', # the default user
#                  password = rstudioapi::askForPassword(), # password of user
#                  options="-c search_path=public" # specify what schema to connect to
# ) 
# 
# 
# dwelling_data_raw <- read_sf(con,
#                              "dwelling_data_clean")  
# 
# } else{ print("Make sure you have the dwelling map file inside your data folder', you can download it here https://github.com/jonathananolan/Melbourne-dwelling-map ")

dwelling_data_raw <-read_sf("data/Melbourne dwelling data.gpkg")

# }

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
