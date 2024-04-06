#Because CoM has lots of tall buildings in zones where housing could be built, but never would since the offices there are so tall... 

fix_com_commercial_zoning <- function(input_data) {

file_name_com <- "data/com_buildings.csv"
if(!file.exists(file_name_com)){
download.file("https://data.melbourne.vic.gov.au/api/v2/catalog/datasets/buildings-with-name-age-size-accessibility-and-bicycle-facilities/exports/csv?delimiter=%2C",destfile = file_name_com)
}

com_building_data <- read_csv(file_name_com)


com_buildings_filtered <- com_building_data %>% 
  filter(number_of_floors_above_ground>4) %>%  # We assume anywhere with >4 storeys will be hard to redevelop. 
  filter(!is.na(longitude)) %>% 
  group_by(latitude,longitude) %>% 
  arrange(desc(census_year)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude","latitude")) %>% 
  mutate(com_tall_building = T) %>%
  st_set_crs("wgs84") %>% 
  select(com_tall_building) %>% 
  st_transform(st_crs(dwelling_data_raw))

dwelling_data_already_developed <- input_data %>% 
  select(lat,lon) %>% 
  st_join(com_buildings_filtered) %>% 
  distinct(lat,
           lon,
           .keep_all = T) %>% 
  mutate(com_tall_building = replace_na(com_tall_building,F)) %>% 
  st_drop_geometry()

input_data %>% 
  left_join(dwelling_data_already_developed) 

}
