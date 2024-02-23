

population_data <- readxl::read_excel("data/32350DS0006_2001-22.xlsx",sheet = 4,skip = 8) %>% 
  select(year = Year, 
         pop = no....24,
         lga_name_2021 =  `LGA name`,
         lga_code_2021 = `LGA code`) %>% 
         filter(!is.na(lga_name_2021)) %>%
  filter(year >2013)

mel_lga_populations <- population_data %>% filter(lga_name_2021 %in% lgas) %>% 
  filter(!lga_name_2021 %in% c("Casey","Cardinia","Wyndham","Melton","Hume","Whittlesea"))

mel_total <- population_data %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop))  %>% 
  ungroup() %>% 
  mutate(`All of Melbourne (excluding growth LGAs)` = pop/pop[year == 2014]) %>% 
  select(-pop)

plot_data <- mel_lga_populations %>% 
  mutate(`Single LGA` = pop/pop[year == 2014]) %>% 
  group_by(lga_name_2021) %>% 
  left_join(mel_total) %>% 
  pivot_longer(c(`All of Melbourne (excluding growth LGAs)`,`Single LGA` ), 
               names_to = "series",
               values_to = "population_change") 

plot_data %>%
  ggplot(aes( x= year, y = population_change,
              colour = series))+
  geom_line(stat = "identity",linewidth = 1)+
  facet_wrap(~lga_name_2021) +
  theme_yimby_mel_caption(caption = "source: ABS.",
                          text_size = "small",
                          plot_type = "line",
                          colour_scale = "c")+
  labs(title = "Population growth for Melbourne Local Government Areas",
       subtitle = "Growth since 2014 compared to Melbourne Average",
       labs = element_blank(),
       colour = "Area",
       x = element_blank())+
  scale_y_continuous(labels = scales::percent_format())


#decomposed bar chart

mel_lga_populations = as.data.table(mel_lga_populations)
mel_lga_populations <- dcast(mel_lga_populations, lga_code_2021 + lga_name_2021 ~ year, value.var = 'pop') # cast long to wide just because I like it that way :)
mel_lga_populations = as.data.frame(mel_lga_populations)
#test with 2015 first

start_year = '2014'
end_year = '2019'

n_dwellings = n_dwellings %>% rename(lga_name_2021 = lga_name_2022)
mel_lga_populations = mel_lga_populations %>% left_join(n_dwellings, by = 'lga_name_2021')

#read areas
lga_info = as.data.frame(fread('data/lga_area.csv')) %>% rename(lga_code_2021 = LGA_CODE21) %>% mutate_at('lga_code_2021', as.numeric)
mel_lga_populations = mel_lga_populations %>% left_join(lga_info, by = 'lga_code_2021')

#population growth

  total_pop_growth = (sum(mel_lga_populations %>% select(any_of(end_year))) / sum(mel_lga_populations %>% select(any_of(start_year))) - 1 ) * 100

  mel_lga_populations = mel_lga_populations %>% mutate(population_growth = unlist(((mel_lga_populations %>% select(any_of(end_year))) / (mel_lga_populations %>% select(any_of(start_year))) - 1 ) * 100) )
  suburb_growth =  unlist(((mel_lga_populations %>% select(any_of(end_year))) / (mel_lga_populations %>% select(any_of(start_year))) - 1 ) * 100)
  
  #suburb_pop_area_growth = unlist(((mel_lga_populations %>% select(any_of(end_year))) / ( (mel_lga_populations %>% select(any_of(start_year)) / (mel_lga_populations %>% select(AREASQKM21)) ))  - 1 )  * 100) 
  weights = unlist(mel_lga_populations %>% select(any_of(start_year)) / sum(mel_lga_populations %>% select(any_of(start_year))))
  
  #area_weights = unlist(mel_lga_populations %>% select(any_of(start_year)) / (sum(mel_lga_populations %>% select(any_of(start_year))) / sum(mel_lga_populations %>% select(AREASQKM21)))  )
  
  mel_lga_populations <- mel_lga_populations %>% mutate( 
      weighted_growth = (suburb_growth * weights))
  mel_lga_populations <- mutate(mel_lga_populations, percent_of_growth = ((weighted_growth / total_pop_growth)) )
  mel_lga_populations$lga_name_2021 = as.factor(mel_lga_populations$lga_name_2021)


population_growth_chart <- ggplot( as.data.frame(mel_lga_populations) , mapping = aes(reorder(lga_name_2021, -percent_of_growth), percent_of_growth)) +
  geom_bar(stat = 'identity') + theme_yimby_mel_caption(caption = "source: ABS.",
                                                                                                                                                                     text_size = "small",                                                                                                                                                                     plot_type = "bar",
                                                                                                                                                                     colour_scale = "c")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Melbourne Population Growth",
       subtitle = paste0(paste0(paste0("% of growth in Melbourne attributable to each LGA, from ", start_year), ' until '), end_year),
       labs = element_blank(),
       colour = "Area",
       x = element_blank() ) +
  scale_y_continuous(labels = scales::percent_format())
#print(population_growth_chart)


  
  #population density
  total_pop_density_growth = ( (sum(mel_lga_populations %>% select(any_of(end_year))) / sum(mel_lga_populations %>% select(AREASQKM21))) / ( sum(mel_lga_populations %>% select(any_of(start_year))) / sum(mel_lga_populations %>% select(AREASQKM21)) ) -1)*100
  
  
  initial_population_density = sum(mel_lga_populations %>% select(any_of(start_year))) / sum(mel_lga_populations %>% select(AREASQKM21))
  print(initial_population_density)
  final_population_density = sum(mel_lga_populations %>% select(any_of(end_year))) / sum(mel_lga_populations %>% select(AREASQKM21))
  print(final_population_density)
  
  mel_lga_populations <- mel_lga_populations %>% mutate(initial_density = unlist(mel_lga_populations %>% select(any_of(start_year)) / mel_lga_populations %>% select(AREASQKM21)) )
  mel_lga_populations <- mel_lga_populations %>% mutate(final_density = unlist(mel_lga_populations %>% select(any_of(end_year)) / mel_lga_populations %>% select(AREASQKM21)) )
  
  #density_change = ( (final_population_density / initial_population_density) -1) *100
  
  mel_lga_populations = mel_lga_populations %>% mutate(density_change_pc = ( (final_density / initial_density) - 1)*100  )
  
  mel_lga_populations <- mel_lga_populations %>% mutate(area_weights =  unlist( mel_lga_populations %>% select(AREASQKM21) / sum(mel_lga_populations %>% select(AREASQKM21))) )
  mel_lga_populations <- mel_lga_populations %>% mutate(population_weights =  unlist( mel_lga_populations %>% select(any_of(start_year)) / sum(mel_lga_populations %>% select(any_of(start_year)))) )
  
  #weight by area
  #mel_lga_populations <- mel_lga_populations %>% mutate(change_by_share = density_change_pc * area_weights)
  
  #weight by population
  mel_lga_populations <- mel_lga_populations %>% mutate(change_by_share = density_change_pc * population_weights )
  
  #print(sum(mel_lga_populations$change_by_share))
  mel_lga_populations <- mel_lga_populations %>% mutate(density_pc_of_growth = unlist(change_by_share / sum(change_by_share)))
  
  
  population_density_chart <- ggplot( as.data.frame(mel_lga_populations) , mapping = aes(reorder(lga_name_2021, -density_change_pc), (density_change_pc)/100)) +
    geom_bar(stat = 'identity') + theme_yimby_mel_caption(caption = "source: ABS.",
                                                          text_size = "small",                                                                                                                                                                     plot_type = "bar",
                                                          colour_scale = "c")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "Melbourne Population Density Growth",
         subtitle = paste0(paste0(paste0("% of population density growth in Melbourne attributable to each LGA, from ", start_year), ' until '), end_year),
         labs = element_blank(),
         colour = "Area",
         x = element_blank() ) +
    scale_y_continuous(labels = scales::percent_format())
  
  #print(population_density_chart)



