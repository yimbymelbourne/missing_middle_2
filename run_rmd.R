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
                                area = area_name
                                ),
                            
                  output_file = paste0(title,".html")
                  )

}


lgas <- dwelling_data_raw %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022)


n_dwellings <- dwelling_data_raw %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000)

source("data/import_lga_pop_data.R")

useful_lots <- dwelling_data_raw %>% filter(zoning_permits_housing == "Housing permitted", dwellings_est<2, !(feature_preventing_development))

melbourne_averages = data.frame(
  useful_lots_heritage_pc = (useful_lots %>% filter(heritage == TRUE) %>% nrow / useful_lots %>% nrow) * 100,
  melbourne_density_change_pc = total_pop_density_growth,
  melbourne_population_change_pc = total_pop_growth,
  melbourne_average_density_raw = sum(mel_lga_populations$`2022`) / sum(mel_lga_populations$AREASQKM21)
  #melbourne_average_density_area_weighting = sum(mel_lga_populations$final_density * mel_lga_populations$area_weights) / mel_lga_populations %>% nrow,
  #melbourne_average_density_population_weighting = sum(mel_lga_populations$final_density * mel_lga_populations$population_weights) / mel_lga_populations %>% nrow
)

walk(lgas, run_for_area)




