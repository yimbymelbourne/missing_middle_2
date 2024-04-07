
create_summary_table_by_lga <- function(input_data) {
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
  mutate(change_to_zoned_capacity = mm_zoned_capacity/zoned_capacity,
         share_of_mm_profitable_apartments = profitable_apartments/sum(profitable_apartments,na.rm = TRUE),
         share_of_mm_prof_from_apartments = profit_from_buildable_apartments/sum(profit_from_buildable_apartments,na.rm = TRUE),
         mm_target = share_of_mm_profitable_apartments * mm_total_target,
         share_of_dwelling_capacity = existing_dwellings/sum(existing_dwellings),
         target_relative_to_other_missing_middle_lgas = share_of_mm_profitable_apartments/share_of_dwelling_capacity
         
  ) 

return(output)

}
