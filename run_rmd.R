# Start by importing all the functions that make this project tick. 
# The most important is 00renv.R which you'll want to look at - it points to the source file as well as 
# loading it into memory. 
# The rest are functions which run only when called later on in the Rmarkdown. 

r_files <- list.files(path = "R/",
                      pattern = "*.R$",
                      full.names = T)

purrr::walk(r_files,source)

#Add beskope info on how YIMBY Melbourne wants to re-zone Melbourne
sf_mel_props <- dwelling_data_raw  %>% 
                add_missing_middle_zoning_info() %>% 
                find_profitable_apartments()  %>% 
  #We add a baseline yield to account for the fact that the model doesn't account well for 'townhouse' style developments in RGZ GRZ zones which could increase density signifiantly. 
  #These townhouses would likely sell more than what an apartment would sell for. 
  mutate(baseline_demand = case_when(zone_short_mm == "Missing middle" ~ .1*missing_middle_yield,
                                     zone_short_mm %in% c("General residential","Residential Growth") ~ .05*missing_middle_yield,
                                     T ~ 0),
        profitable_apartments = pmax(profitable_apartments, baseline_demand))

runnable <- sf_mel_props%>% 
  filter(!is.na(profit)) %>%
  filter(lot_size <2000,
         abs(profit)<5e6) %>% 
  mutate(profit_per_apartment = profit/apartments_if_built_to_this_height) %>% 
  filter(profit_per_apartment>-1e6)


hist(runnable$apartments_if_built_to_this_height)
hist(runnable$profitable_apartments)
hist(runnable$missing_middle_storeys)
hist(runnable$storey)
hist(runnable$cost_of_building_one_apartment_on_this_floor)
hist(runnable$profit_per_apartment)

runnable %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  ggplot(aes(x = profit_per_apartment,
             fill = `profitable?`)) +
  geom_histogram()+
  facet_wrap(~zone_short_mm)



runnable %>% 
  filter(zone_short_mm == "Residential Growth") %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  ggplot(aes(x = profit_per_apartment,
             fill = `profitable?`)) +
  geom_histogram()+
  facet_wrap(~storey)



runnable %>% 
  ggplot(aes(x = profit))+
  geom_histogram()+
  facet_wrap(~zone_short_mm)


runnable %>% 
  mutate(pp_app = profit/apartments_if_built_to_this_height) %>% 
  select(profitable_apartments,pp_app,zone_short_mm,mm_yield_net) %>% 
  filter(!is.na(profitable_apartments)) %>% 
  write_sf("test/profit_per_apartment.shp")
  
#Create a version without geometries
df_mel_props <- sf_mel_props %>% st_drop_geometry() 

df_mel_props %>% 
  filter(lga_name_2022 %in% c(middle_lgas,inner_lgas)) %>% 
  group_by(lga_name_2022) %>% 
  summarise(profitable_apartments = sum(profitable_apartments, na.rm = TRUE),
            profitable_apartments_w_floor = max(sum(profitable_apartments),
                                                .05 * sum(missing_middle_yield[zone_short_mm == "Missing middle"]))
            ) %>% 
            view()

sf::sf_use_s2(FALSE)  # Disable s2 engine

lgas <- df_mel_props %>% 
  st_drop_geometry() %>% 
  group_by(lga_name_2022) %>% 
  summarise(n=n()) %>% 
  filter(n > 10000) %>% 
  pull(lga_name_2022) 

#We want to look at all LGAs but greenfield. We really don't have a solution for Greenfield right now. 
missing_middle_lgas <- df_mel_props %>% 
  group_by(lga_name_2022,area) %>% 
  summarise(n=n(),
            .groups = "drop") %>% 
  filter(n > 10000) %>% 
  filter(area != "greenfield")  %>% 
  filter(!(lga_name_2022 %in% 
            c("Yarra Ranges",
              "Mornington Peninsula",
              "Nillumbik")
          )
        ) %>% #as discussed at 24/3/2 meeting - no longer 'middle'%>%
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
                    output_file = paste0("html/",area_name,".html")
                    )

                  
}




walk(missing_middle_lgas,run_for_area)

#Create the index page as well

rmarkdown::render("index.Rmd", 
                  output_file = paste0("html/index.html")
)


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
