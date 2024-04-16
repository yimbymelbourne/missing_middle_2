#filter the big dataset for a given LGA (area name) and then render the rmarkdown, saving it into the RMD folder.

run_for_area <- function(area_name) {
  #print(paste0("running for ",area_name))
  # area_name = "Banyule"
  output <-ls()
  output$area_name <- area_name
  
  sf_lga_props <- sf_mel_props %>% filter(lga_name_2022 %in% area_name)
  
  df_lga_props <- sf_lga_props %>% st_drop_geometry()
  
  sf_lga_useful_props <- sf_lga_props %>% 
    filter(!feature_preventing_development,
           dwellings_est <=1,
           zoning_permits_housing == "Housing permitted")
  
  df_lga_useful_props <- sf_lga_useful_props %>% st_drop_geometry()
  
  lga_summary <- lga_zoning_numbers %>% 
    filter(lga_name_2022 == area_name)
  
  
  # knitr::opts_chunk$set(message = FALSE, warning = FALSE, error = FALSE, echo = FALSE, dpi = 300,fig.width = 7, fig.height = 4)
  # knitr::opts_knit$set(root.dir= '../')   
  
  output$parcels <- prettyNum(nrow(df_lga_props),big.mark = ",")
  
  output$fois <- df_lga_props %>% 
    filter(feature_preventing_development) %>% 
    nrow()%>% 
    prettyNum(big.mark = ",")
  
  output$bad_zoning <- df_lga_props %>% 
    filter(!feature_preventing_development) %>% 
    group_by(zoning_permits_housing) %>% 
    summarise(n=n()) %>% 
    filter(zoning_permits_housing == "Housing not generally permitted") %>% 
    pull(n) %>% 
    prettyNum(big.mark = ",")
  
  output$already_developed <- df_lga_props %>% 
    filter(zoning_permits_housing == "Housing permitted",
           !feature_preventing_development,
           dwellings_est>1) %>%
    nrow()%>% 
    prettyNum(big.mark = ",")
  
  
  output$df_lga_useful_props_num <-  prettyNum(nrow(df_lga_useful_props),big.mark = ",")
  
  
  output$area_name <- area_name
  
  output$mcg_size <- 20000 
  output$mm_number <- df_lga_useful_props %>% filter(zone_short_mm == "Missing middle") %>% nrow() %>% prettyNum(big.mark = ",")
  output$rgz_number <- df_lga_useful_props %>% filter(zone_short_mm == "Residential growth") %>% nrow() %>% prettyNum(big.mark = ",")
  output$grz_number <- df_lga_useful_props %>% filter(zone_short_mm == "General residential") %>% nrow() %>% prettyNum(big.mark = ",")
  
  output$all_lots_size <- prettyNum(sum(df_lga_useful_props$lot_size),big.mark = ",")
  output$all_lots_size_num <- sum(df_lga_useful_props$lot_size)
  
  
  output$mcgs_of_space = prettyNum(round(output$all_lots_size_num/output$mcg_size),big.mark = ",")
  
  output$existing_zoned_capacity <- prettyNum(round(lga_summary$zoned_capacity,-3), big.mark = ",")
  output$mm_zoned_capacity <- prettyNum(round(lga_summary$mm_zoned_capacity,-2), big.mark = ",")
  output$zoned_capacity_x <- lga_summary %>% 
    mutate( change_to_zoned_capacity = paste0(round(change_to_zoned_capacity,1),"x")) %>% 
    pull(change_to_zoned_capacity)
  
  
  output$mm_zoning_map <- make_map(sf_lga_props,"category_new",area_name)
  
  output$existing_zoning_map <-  make_map(sf_lga_props,"category",area_name)
  
  output$profitable_units <- prettyNum(round(lga_summary$profitable_apartments,-3), big.mark = ",")
  output$mm_target <- prettyNum(round(lga_summary$mm_target,-2), big.mark = ",")


##THIS ONE SHOULD BE REPLACED BY PLANNING DATA!!! 

mel_total <- lga_pop_change_importer(area_name, type = "population")


lga_change <- mel_total %>% 
  filter(year == 2021,
         lga_name_2021 == area_name) %>%
  pull(change)

mel_change <- mel_total %>% 
  filter(year == 2021,
         lga_name_2021 == "All of Melbourne") %>%
  pull(change)



change_ratio <- lga_change/mel_change

## TODO - get PT stops per LGA. 

output$text_on_growth <- case_when(change_ratio >.9 ~ paste0(area_name, " has been building housing, but but there is more work to be done."),
                            change_ratio >.7 ~ paste0(area_name, " has been building some housing, but it is still barely keeping up with the average for Melbourne."),
                            T ~ paste0(area_name, " is falling well behind the average for new housing in Melbourne.")
)

output$ggplot_on_growth <- mel_total %>%
  filter(year %in% c(2014,2022)) %>% 
  ggplot(aes( x= year, 
              y = change,
              colour = lga_name_2021))+
  geom_line(stat = "identity",linewidth = 1)+
  geom_point(stat = "identity",size = 3)+
  theme_yimby_mel_caption(caption = "Source: ABS. ",
                          text_size = "small",
                          plot_type = "line",
                          colour_scale = "r")+
  labs(title = paste("Growth in homes for",area_name,"compared to the rest of Melbourne"),
       labs = element_blank(),
       colour = "Area",
       x = element_blank(),
       y = element_blank())+
  scale_y_continuous(labels = scales::percent_format())



dwellings_by_type <- df_lga_useful_props %>%
  group_by(category) %>% 
  summarise(n = n(),
            lot_size = sum(lot_size),
            .groups = "drop") %>% 
  group_by(category) %>% 
  mutate(n_total = sum(n),
         lot_size_total = sum(lot_size))  %>% 
  ungroup() 

output$current_zoning_bar_chart <- dwellings_by_type %>% 
  mutate(category = fct_reorder(category,n_total)) %>% 
  ggplot(aes(x = category, 
             y = n))+
  coord_flip()+ 
  geom_bar(stat = "identity")+
  theme_yimby_mel_caption(text_size = "small",plot_type = "bar",colour_scale = "light_dark") +
  labs(x = element_blank(),
       title = paste0("Land in ",area_name," by type"),
       y = "Number of properties",
       fill = "Heritage status")

output$current_zoning_bar_chart_by_area <- dwellings_by_type %>% 
  mutate(category = fct_reorder(category,lot_size_total)) %>%
  ggplot(aes(x = category, 
             y = lot_size_total))+
  coord_flip()+ 
  geom_bar(stat = "identity")+
  theme_yimby_mel_caption(text_size = "small",plot_type = "bar",colour_scale = "light_dark") +
  labs(x = element_blank(),
       title = paste0("Land in ",area_name," by type"),
       y = "Area (sqm)",
       fill = "Heritage status")+
  scale_y_continuous(labels = scales::number_format(big.mark = ","))



## Prep work for table showing dwellings by type

dwellings_by_type_w_heritage <- df_lga_useful_props %>%
  group_by(category,heritage) %>% 
  summarise(n = n(),
            lot_size = sum(lot_size),
            .groups = "drop") %>% 
  group_by(category) %>% 
  mutate(n_total = sum(n),
         lot_size_total = sum(lot_size))  %>% 
  ungroup() %>% 
  mutate(heritage = if_else(heritage,
                            "Subject to heritage controls",
                            "Free from heritage"))

types_zonning <- df_lga_useful_props %>% 
  group_by(heritage) %>% 
  summarise(n = n(),
            lot_size = sum(lot_size)) %>% 
  mutate(category = "Total")%>% 
  mutate(heritage = if_else(heritage,
                            "Subject to heritage controls",
                            "Free from heritage"))


types_total_new <- df_lga_useful_props %>%
  filter(!feature_preventing_development,
         zoning_permits_housing == "Housing permitted",
         dwellings_est < 2) %>% 
  group_by(heritage) %>% 
  summarise(n = n(),
            lot_size = sum(lot_size),
            .groups = "drop") %>% 
  mutate(heritage = if_else(heritage,
                            "Subject to heritage controls",
                            "Free from heritage"),
         category = "Total where new housing can be built") 

output$dwellings_by_type_table <- dwellings_by_type_w_heritage %>% 
  bind_rows(types_zonning) %>% 
  bind_rows(types_total_new) %>% 
  group_by(category) %>% 
  mutate(`Share of properties that are heritage` = paste0(round(100*n/sum(n)),"%"),
         `Share of land that is heritage` =  paste0(round(100*lot_size/sum(lot_size)),"%")
  ) %>% 
  rename(`Number of properties` = n,
         `Area (sqm)` = lot_size,
         ` ` = heritage) %>% 
  select(-n_total,-lot_size_total) %>% 
  gt()  %>% 
  fmt_number(decimals = 0)


output$heritage_no <- df_lga_useful_props %>%
  filter(profitable_apartments>0) %>%
  summarise(apartments_affected_by_heritage = paste0(round(100*weighted.mean(heritage,profitable_apartments)),"%")) %>%
  pull(apartments_affected_by_heritage)


return(output)
}
