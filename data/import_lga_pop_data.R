

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

plot_data <- mel_lga_populations%>% 
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
