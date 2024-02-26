library(purrr)

# Start by importing all the functions that make this project tick. 
# The most important is 00renv.R which you'll want to look at - it points to the source file as well as 
# loading it into memory. 
# The rest are functions which run only when called later on in the Rmarkdown. 

r_files <- list.files(path = "R/",
                      pattern = "*.R$",
                      full.names = T)

walk(r_files,source)

#Add beskope info on how YIMBY Melbourne wants to re-zone Melbourne
sf_mel_props <- add_missing_middle_zoning_info() 
#Create a version without geometries
df_mel_props <- sf_mel_props %>% st_drop_geometry() 

sf::sf_use_s2(FALSE)  # Disable s2 engine

lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022)

#We want to look at all LGAs but greenfield. We really don't have a solution for Greenfield right now. 
missing_middle_lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022,area) %>% 
  summarise(n=n(),
            .groups = "drop") %>% 
  filter(n > 10000) %>% 
  filter(area != "greenfield") %>%
  pull(lga_name_2022)

#filter the big dataset for a given LGA (area name) and then render the rmarkdown, saving it into the RMD folder.

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

#Create the index page as well

rmarkdown::render("index.Rmd", 
                  params = list(area = area_name),
                  output_file = paste0("rmd/index.html")
)


#Upload to AWS - let JN know if you want access to the bucket. If not commit your changes and he can run. 
 if(Sys.info()[7] == "jonathannolan") {

list_rmds_with_path <- list.files(pattern = "*.html",
                        path = "rmd",
                        full.names = T)

list_rmds <- list.files(pattern = "*.html",
                        path = "rmd")


aws.s3::bucketlist(add_region = T)
Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-2")

Sys.setenv(AWS_DEFAULT_REGION = rstudioapi::askForPassword())
Sys.setenv(AWS_ACCESS_KEY_ID = rstudioapi::askForPassword())

upload_object <- function(file_location,url){

put_object(file = file_location, bucket = "yimby-mel",multipart = T,show_progress = T,
           headers = list("Content-Type" = "text/html"),verbose = T)
}

walk2(list_rmds_with_path,list_rmds,upload_object)

}
