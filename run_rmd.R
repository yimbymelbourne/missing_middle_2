source("R/00renv.R")
# create connection to postgres 
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = 'jnjnjn', # password of user
                 options="-c search_path=public" # specify what schema to connect to
)

options(digits=6)

source("R/ggplot_theme.R")
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


dwelling_data_raw <- read_sf(con,
                            "dwelling_data_clean")  

run_for_area <- function(area_name) {
dwelling_data_sf <- dwelling_data_raw %>% 
  filter(lga_name_2022 == area_name) %>% 
  mutate(type_short = case_when(feature_preventing_development ~"Civic land",
                                dwellings_est>1 ~ "Already developed",
                                T ~ zone_short),
         heritage_nice = if_else(heritage,
                                 "Subject to heritage controls",
                                 "Free from heritage")
         )

dwelling_data <-  dwelling_data_sf %>% st_drop_geometry() 

title <- paste0("Heritage and zoning in ",
                area_name)

rmarkdown::render("run_city.Rmd", 
                  params = list(doc_title = title,
                                area = area_name),
                  output_file = paste0("rmd/",title,".html")
                  )

                  
}
lgas <- dwelling_data_raw %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022)

walk(lgas,run_for_area)

run_for_area("Yarra")
run_for_area("Melbourne")

