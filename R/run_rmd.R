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
  add_missing_middle_zoning_info() %>% #Complex steps are put out into functions so they can be compartmentalised. 
  find_profitable_apartments()  
  



#A series of further QC graphs but this time unlike hte RMD in 03_run_regression we're using the full complex way of calculating what apartments are profitable. 
#Test for good apartments.... 
sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  ggplot(aes(x = profit_per_apartment,
             fill = `profitable?`)) +
  geom_histogram() +
  facet_wrap(~zone_short_mm)

sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  filter(apartments_if_built_to_this_height <100) %>% 
  ggplot(aes(x = apartments_if_built_to_this_height,
             fill = `profitable?` ))+
  geom_histogram()

hist(sf_mel_props$missing_middle_storeys)
sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  filter(apartments_if_built_to_this_height <100) %>% 
  ggplot(aes(x = storey,
             fill = `profitable?` ))+
  geom_histogram()

hist(sf_mel_props$construction_cost_per_apt_this_height)
hist(sf_mel_props$profit_per_apartment)


sf_mel_props %>% 
  distinct(construction_cost_per_apt_this_height,storey,zone_short_mm) %>% 
  ggplot(aes(y = construction_cost_per_apt_this_height,
             x = storey,
             colour =  zone_short_mm)) +
  geom_point()

sf_mel_props %>% 
  mutate(prf_ap = profit_per_apartment) %>% 
  select(profitable_apartments,prf_ap,zone_short_mm,mm_yield_net,zone_short_mm,storey,lot_size,apartments_if_built_to_this_height) %>% 
  filter(!is.na(profitable_apartments)) %>% 
  write_sf("test/profit_per_apartment.shp")
  
#Create a version without geometries
df_mel_props <- sf_mel_props %>% st_drop_geometry() 

mm_total_target = 80000*.7

heritage_zones <- c("Missing middle","General residential","Residential growth","Low density residential") # zones where we think you can't increase dwelling numbers if there's heritage


lga_zoning_numbers <- df_mel_props %>% 
  mutate(heritage_affecting_zoned_capacity = if_else(heritage & zone_short %in% heritage_zones,0,buxton_yield_net),
         heritage_affecting_zoned_capacity_mm = if_else(heritage & zone_short_mm %in% heritage_zones,0,mm_yield_net),
         profitable_new_dwellings_net = pmax(0,profitable_apartments- dwellings_est )) %>% 
  group_by(lga_name_2022) %>% 
  summarise(profitable_apartments = sum(profitable_apartments,na.rm = TRUE),
            profitable_new_dwellings_net = sum(profitable_new_dwellings_net,na.rm = TRUE),
            profit_from_buildable_apartments = sum(pmax(profit_from_buildable_apartments,50000),na.rm = TRUE),
            existing_dwellings = sum(dwellings_est,na.rm = TRUE),
            zoned_capacity = sum(buxton_yield_net,na.rm = TRUE),
            zoned_capacity_heritage = sum(heritage_affecting_zoned_capacity,na.rm = TRUE),
            mm_zoned_capacity = sum(mm_yield_net,na.rm = TRUE),
            mm_zoned_capacity_heritage = sum(heritage_affecting_zoned_capacity_mm,na.rm = TRUE)
) %>% 
  ungroup() %>% 
  mutate(change_to_zoned_capacity = mm_zoned_capacity/zoned_capacity,
         share_of_mm_profitable_apartments = profitable_apartments/sum(profitable_apartments,na.rm = TRUE),
         share_of_mm_prof_from_apartments = profit_from_buildable_apartments/sum(profit_from_buildable_apartments,na.rm = TRUE),
         mm_target = share_of_mm_profitable_apartments * mm_total_target,
         share_of_dwelling_capacity = existing_dwellings/sum(existing_dwellings),
         target_relative_to_other_missing_middle_lgas = share_of_mm_profitable_apartments/share_of_dwelling_capacity
           
  ) 



sf::sf_use_s2(FALSE)  # Disable s2 engine which sometimes causes errors - iirc there are github issues on this but it isn't fixed. 


#superfluous I think because we've already filtered lgas above....  
lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022) 

#We want to look at all LGAs but greenfield. We really don't have a solution for Greenfield right now. 
missing_middle_lgas <- lgas

rmarkdown::render("rmd/index.Rmd", 
                  output_file = paste0("../html/index.html"),clean = FALSE
)



#filter the big dataset for a given LGA (area name) and then render the rmarkdown, saving it into the RMD folder.

run_for_area <- function(area_name) {
  
#  area_name = "Melbourne"
  sf_lga_props <- sf_mel_props %>% filter(lga_name_2022 %in% area_name)

  df_lga_props <- sf_lga_props %>% st_drop_geometry()

  sf_lga_useful_props <- sf_lga_props %>% 
    filter(!feature_preventing_development,
           dwellings_est <=1,
           zoning_permits_housing == "Housing permitted")

  df_lga_useful_props <- sf_lga_useful_props %>% st_drop_geometry()
  

  rmarkdown::render("run_city.Rmd", 
                    params = list(area = area_name),
                    output_file = paste0("html/",area_name,".html")
                    )

                  
}



walk(missing_middle_lgas,run_for_area)



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
walk2(list_rmds_with_path[7],list_rmds[7],upload_object)

}
