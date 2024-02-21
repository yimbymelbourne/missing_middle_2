
lga_pop_change_importer <- function(lga){
population_data <- readxl::read_excel("data/32350DS0006_2001-22.xlsx",sheet = 4,skip = 8) %>% 
  select(year = Year, 
         pop = no....24,
         lga_name_2021 =  `LGA name`,
         lga_code_2021 = `LGA code`) %>% 
         filter(!is.na(lga_name_2021)) %>%
  filter(year >2013)

mel_lga_populations <- population_data %>% filter(lga_name_2021 %in% lgas) 

mel_total <- population_data %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop))  %>% 
  ungroup() %>% 
  mutate(population_change = pop/pop[year == 2014],
         lga_name_2021 = "All of Melbourne") 

output <- mel_lga_populations%>% 
  filter(lga_name_2021 == lga) %>% 
  mutate(population_change = pop/pop[year == 2014]) %>% 
  bind_rows(mel_total) 

return(output)

}

