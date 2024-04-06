## This project is the data, analysis and outputs for the Missing Middle 2 report. 
# This projet is not 'set and forget' - It has been designed to run line by line, and you can inpsect to need to download files, install packages etc to get it to work. 


# Start by importing all the functions that make this project tick. 
# The most important is 00renv.R which you'll want to look at - it points to the source file as well as 
# loading it into memory. 
# The rest are functions which run only when called later on in the Rmarkdown. 

r_files <- list.files(path = "R/functions/",
                      pattern = "*.R$",
                      full.names = T)

purrr::walk(r_files,source)


#Perform regressions. While in dev it's probably better to run this code line by line.  
source("R/R_regressions/03run_regression.R")

#Add beskope info on how YIMBY Melbourne wants to re-zone Melbourne
sf_mel_props <- dwelling_data_raw %>% 
  filter(lga_name_2022 %in% c(inner_lgas,middle_lgas)) %>% 
  fix_com_commercial_zoning() %>% 
  add_missing_middle_zoning_info() %>% #Complex steps are put out into functions so they can be compartmentalised. 
  find_profitable_apartments()  

source("R/experimental/qc_graphs.R") # no need to run if you don't want to! 
  
#Create a version without geometries
df_mel_props <- sf_mel_props %>% st_drop_geometry() 

lga_zoning_numbers <- create_summary_table_by_lga(df_mel_props)



sf::sf_use_s2(FALSE)  # Disable s2 engine which sometimes causes errors - iirc there are github issues on this but it isn't fixed. 


#superfluous I think because we've already filtered lgas above....  
lgas <- c(inner_lgas,middle_lgas)

#We want to look at all LGAs but greenfield. We really don't have a solution for Greenfield right now. 
missing_middle_lgas <- lgas

rmarkdown::render("rmd/index.Rmd", 
                  output_file = paste0("../html/index.html"),clean = FALSE
)



#filter the big dataset for a given LGA (area name) and then render the rmarkdown, saving it into the RMD folder.

run_for_area <- function(area_name) {
  print(paste0("running for ",area_name))
# area_name = "Boroondara"
  sf_lga_props <- sf_mel_props %>% filter(lga_name_2022 %in% area_name)

  df_lga_props <- sf_lga_props %>% st_drop_geometry()

  sf_lga_useful_props <- sf_lga_props %>% 
    filter(!feature_preventing_development,
           dwellings_est <=1,
           zoning_permits_housing == "Housing permitted")

  df_lga_useful_props <- sf_lga_useful_props %>% st_drop_geometry()
  
  lga_summary <- lga_zoning_numbers %>% 
    filter(lga_name_2022 == area_name)

  rmarkdown::render("rmd/run_city.Rmd", 
                    params = list(area = area_name),
                    output_file = paste0("../html/",area_name,".html")
                    )

                  
}


walk(missing_middle_lgas[!(missing_middle_lgas %in% c("Kingston (Vic.)","Bayside (Vic.)"))],run_for_area)



#Upload to AWS - let JN know if you want access to the bucket. If not commit your changes and he can run. 
 if(Sys.info()[7] == "jonathannolan") {

list_rmds_with_path <- list.files(pattern = "*.html",
                        path = "html",
                        full.names = T)

list_rmds <- list.files(pattern = "*.html",
                        path = "html")


Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-2")

#Sys.setenv(AWS_ACCESS_KEY_ID = rstudioapi::askForPassword())
#Sys.setenv(AWS_SECRET_ACCESS_KEY = rstudioapi::askForPassword())

upload_object <- function(file_location,url){

put_object(file = file_location, bucket = "yimby-mel",multipart = T,show_progress = T,
           headers = list("Content-Type" = "text/html"),verbose = T)
}

aws.s3::bucketlist(add_region = T)
walk2(list_rmds_with_path,list_rmds,upload_object)

}
