source("R/00renv.R")


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

run_for_area("Bayside (Vic.)")

walk(lgas,run_for_area)



