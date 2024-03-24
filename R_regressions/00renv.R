
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
library(showtext)
library(ggtext)
library(scales)
library(gt)
library(ggridges)
library(aws.s3)


# create connection to postgres
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)

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
                 "Whitehorse")

