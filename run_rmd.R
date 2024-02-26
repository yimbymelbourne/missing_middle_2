walk(list.files(path = "R/",
                pattern = "*.R",
                full.names = T),source)


sf_mel_props <- add_missing_middle_zoning_info() 

df_mel_props <- sf_mel_props %>% st_drop_geometry() 

sf::sf_use_s2(FALSE)  # Disable s2 engine

lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022)


missing_middle_lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022,area) %>% 
  summarise(n=n(),
            .groups = "drop") %>% 
  filter(n > 10000) %>% 
  filter(area != "greenfield") %>%
  pull(lga_name_2022)


run_for_area <- function(area_name) {
  
  sf_lga_props <- sf_mel_props %>% filter(lga_name_2022 %in% area_name)

  df_lga_props <- sf_lga_props %>% st_drop_geometry()

  sf_lga_useful_props <- sf_lga_props %>% 
    filter(!feature_preventing_development,
           dwellings_est <=1,
           zoning_permits_housing == "Housing permitted")

  df_lga_useful_props <- sf_lga_useful_props %>% st_drop_geometry()
  

  rmarkdown::render("run_city.Rmd", 
                    params = list(area = area_name),
                    output_file = paste0("rmd/",area_name,".html")
                    )

                  
}




walk(missing_middle_lgas,run_for_area)



rmarkdown::render("index.Rmd", 
                  params = list(area = area_name),
                  output_file = paste0("rmd/index.html")
)

list_rmds_with_path <- list.files(pattern = "*.html",
                        path = "rmd",
                        full.names = T)

list_rmds <- list.files(pattern = "*.html",
                        path = "rmd")

upload_object <- function(file_location,url){

put_object(file = file_location, object = url, bucket = "yimby-mel",multipart = T,show_progress = T,
           headers = list("Content-Type" = "text/html"))
}

walk2(list_rmds_with_path,list_rmds,upload)
