
create_summary_table_by_lga <- function(input_data) {
  
  lga_area <- strayr::read_absmap("lga2021") %>%
    filter(state_name_2021 == "Victoria") %>% 
    select(lga_name_2022 = lga_name_2021,areasqkm_2021) %>% 
    mutate(lga_name_2022 = str_remove_all(lga_name_2022, "\\s*\\(.*?\\)\\s*")) %>%
    mutate(lga_name_2022 = if_else(lga_name_2022 == "Moreland","Merri-bek",lga_name_2022)) %>% 
    st_drop_geometry()
  
  
  manual_lga_data <- tribble(~lga_name_2022,~`Current target`,~`Current yearly growth in homes (from 2016-2022)`,
                             "Banyule",         NA_real_ , 563,
                             "Bayside",     	492, 405,
                             "Boroondara",    627, 706,
                             "Brimbank",      608, 517,
                             "Darebin",     	1340,951,
                             "Glen Eira",	    520, 1283,
                             "Hobsons Bay",	  440, 428,
                             "Kingston",	    980, 753,
                             "Manningham",	  460, 906,
                             "Maribyrnong",	  820, 946,
                             "Maroondah",	    562, 489,
                             "Melbourne",	   2765, 5505,
                             "Monash",	      540, 1133,
                             "Moonee Valley",1082, 988,
                             "Merri-bek",    1905, 1515,
                             "Port Phillip",  859, 1085,
                             "Stonnington",	  900, 1080,
                             "Whitehorse",  	520, 1149,
                             "Yarra",       	895, 1205)
  
  mm_total_target = 80000*.5
  
  heritage_zones <- c("Missing middle","General residential","Residential growth","Low density residential") # zones where we think you can't increase dwelling numbers if there's heritage
  
output <- input_data %>% 
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
  left_join(manual_lga_data) %>% 
  left_join(lga_area) %>% 
  mutate(change_to_zoned_capacity = mm_zoned_capacity/zoned_capacity,
         share_of_mm_profitable_apartments = profitable_apartments/sum(profitable_apartments,na.rm = TRUE),
         share_of_mm_prof_from_apartments = profit_from_buildable_apartments/sum(profit_from_buildable_apartments,na.rm = TRUE),
         mm_target = share_of_mm_profitable_apartments * mm_total_target,
         share_of_dwelling_capacity = existing_dwellings/sum(existing_dwellings),
         target_relative_to_other_missing_middle_lgas = share_of_mm_profitable_apartments/share_of_dwelling_capacity,
         existing_dwelling_density = existing_dwellings/areasqkm_2021,
         change_to_target_required = mm_target/`Current yearly growth in homes (from 2016-2022)`
  )

return(output)

}
