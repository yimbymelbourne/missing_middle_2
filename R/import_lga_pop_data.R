

#This version imports population which looks a bit janky because of COVID changes in population. 
 lga_pop_change_importer <- function(lga,type = "population"){


if(type == "population") {
population_data <- readxl::read_excel("data/32350DS0006_2001-22.xlsx",sheet = 4,skip = 8) %>%
  select(year = Year,
         count = no....24,
         lga_name_2021 =  `LGA name`,
         lga_code_2021 = `LGA code`) %>%
         filter(!is.na(lga_name_2021)) %>%
  filter(year >2013)

mel_lga_populations <- population_data %>% filter(lga_name_2021 %in% lgas)

mel_total <- population_data %>%
  group_by(year) %>%
  summarise(count = sum(count))  %>%
  ungroup() %>%
  mutate(change = count/count[year == 2014],
         lga_name_2021 = "All of Melbourne")

output <- mel_lga_populations%>%
  filter(lga_name_2021 %in% lga) %>%
  group_by(lga_name_2021) %>% 
  mutate(change = count/count[year == 2014]) %>%
  bind_rows(mel_total)

return(output)

} else {
#Else pulls dwelling numbers
  all_csv <-list.files("data/tablebuilder_dwellings_lga/","*.csv",full.names = T)

  abs_dwelling_count <- map_df(all_csv, ~read_csv(.x,skip = 9) %>% 
                               mutate(filename = .x)
                     ) %>%
    mutate(year = parse_number(filename)) %>% 
    mutate(lga_name_2021 = coalesce(`Local Government Areas`,`LGA (EN)`,LGA)) %>% 
    select(lga_name_2021,
           count = Count,
           year) %>% 
    filter(lga_name_2021 != "Total",
           !is.na(count)) %>% 
    mutate(lga_name_2021 = str_remove_all(lga_name_2021, "\\s*\\(.*?\\)\\s*"),
           lga_name_2021 = str_replace(lga_name_2021,"Colac Otway","Colac-Otway")) %>% 
    group_by(lga_name_2021) %>% 
    filter(n() == 4) %>%
    mutate(change = count/count[year == 2006]) %>% 
    ungroup() %>% 
    filter(lga_name_2021 %in% str_remove_all(lgas, "\\s*\\(.*?\\)\\s*"))
  
  
  mel_total <- abs_dwelling_count %>%
    group_by(year) %>%
    summarise(count = sum(count))  %>%
    ungroup() %>%
    mutate(change = count/count[year == 2006],
           lga_name_2021 = "All of Melbourne")

  output <- abs_dwelling_count%>%
    filter(lga_name_2021 %in% lga) %>% 
    bind_rows(mel_total)

  return(output)
  
  
  
  
    }
   
 }

