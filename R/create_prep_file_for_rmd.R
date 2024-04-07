#This is code that JN made to make it easier to run the RMD files. It creates a qs file which everyone else can just download from AWS

#Or if you want to update something in the file you can run the whole code... 



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

sf_mel_props %>% qs::qsave("data/rmd_data.qs")

#I manually update this to AWS. 

source("R/experimental/qc_graphs.R") # no need to run if you don't want to! 

