add_missing_middle_zoning_info <- function(input_data){


mel_lgas <- c(inner_lgas,middle_lgas,outer_lgas)


#Here are their yields: 
buxton_yields <- input_data %>% 
  st_drop_geometry() %>% 
  filter(lga_name_2022 %in% mel_lgas ) %>% 
  #filter(lga_name_2022 %in% area_name) %>% 
  mutate(buxton_yield = 
         case_when(feature_preventing_development ~ 0,
                   zone_short == "Low density residential" ~ 1,
                   zoning_permits_housing %in% c("Housing not generally permitted","Rural/regional") ~ 0, # Need to sort out CCZ, DZ etc.
                   zone_short == "Neighbourhood residential" & lot_size <500 ~ 1,
                   zone_short == "Neighbourhood residential" & lot_size >=500 & dwellings_est == 1 ~ 1,
                   zone_short == "Neighbourhood residential" & lot_size >= 500 ~ 2,
                   zone_short == "General residential" & lot_size < 450  ~ 1,
                   zone_short == "General residential" & lot_size < 675  ~ 2,
                   zone_short == "General residential" & lot_size < 1000 ~ 3,
                   zone_short == "General residential" & lot_size < 2000 ~ 8,
                   zone_short == "Residential Growth" & lot_size < 500  ~ 8,
                   zone_short == "Residential Growth" & lot_size < 1000 ~ 18,
                   zone_short == "Residential Growth" & lot_size < 2000 ~ 36,
                   zone_short %in% c("Mixed use","Commercial") & lot_size <500  ~ 11, #Buxton assumes 4 resi + 2 mixed use here which is very low! 
                   zone_short %in% c("Mixed use","Commercial") & lot_size <1000 ~ 24,
                   zone_short %in% c("Mixed use","Commercial")& lot_size <2000 ~ 48,
                   lga_name_2022 %in% inner_lgas  & lot_size >2000 ~  120*lot_size/10000, 
                   lga_name_2022 %in% middle_lgas & lot_size >2000 ~ 70*lot_size/10000,
                   lga_name_2022 %in% c(outer_lgas,greenfield) & lot_size >2000 ~ 70*lot_size/10000,
         T ~ NA_real_),
         buxton_yield_net = floor(pmax(0,buxton_yield - dwellings_est))
         )  


#We have data for developments >10 units, so we can look at where buxton predicted
# more than 10 units and check he got it right.

#Some initial exploration of zoned capacity to be moved into main report

# buxton_yields %>% 
#   filter(!feature_preventing_development) %>% 
#   filter(!zoning_permits_housing %in% c("Housing not generally permitted",
#                                         "Rural/regional") ) %>% 
#   st_drop_geometry() %>% 
#   mutate(lot_size_100 = 100*floor(lot_size/100)) %>% 
#   group_by(lot_size_100,
#            zone_short) %>% 
#   summarise(lot_yield = mean(buxton_yield_net),
#             n= n()) %>%
#   filter(n>100,
#          lot_size_100 <2500) %>% 
#   ggplot(aes(x = lot_size_100,
#              y = lot_yield,
#              colour = zone_short)
#          ) +
#   geom_point(stat = "identity")+
#   labs(title = "Dwelling yields under Buxton's paper",
#        subtitle = "Number of new dwellings that could fit on different sized sites",
#        x = "Lot size in square meters",
#        y = "Number of new dwellings per site")



# buxton_yields %>% 
#   filter(!feature_preventing_development) %>% 
#   filter(!zoning_permits_housing %in% c("Housing not generally permitted",
#                                         "Rural/regional"),
#          !is.na(dev_dwlgs_total),
#          buxton_yield>10) %>% 
#   st_drop_geometry() %>% 
#   mutate(lot_size_100 = 250*floor(lot_size/250)) %>% 
#   group_by(lot_size_100,
#            zone_short) %>% 
#   summarise(buxton_estimated_yield = mean(buxton_yield),
#             actual_yield = mean(dev_dwlgs_total),
#             n= n()) %>%
#   pivot_longer(c(buxton_estimated_yield,
#                  actual_yield),
#                names_to = "category",
#                values_to = "value") %>% 
#   filter(n>5,
#          lot_size_100 <2500) %>% 
#   ggplot(aes(x = lot_size_100,
#              y = value/lot_size_100,
#              colour = category)
#   ) +
#   geom_point(stat = "identity")+
#   labs(title = "Dwelling yields under Buxton's paper comapred to real life",
#        subtitle = "Number of new dwellings that could fit on different sized sites",
#        x = "Lot size in square meters",
#        y = "Number of new dwellings per square meter") +
#   facet_wrap(~zone_short)

#From this we can deduce that Buxton's yields for RGZ and GRZ are fairly good, 
#but for Mixed use zones we should assume a much higher yield density. 

# input_data %>% 
#   st_drop_geometry() %>% 
#   filter(zone_short == "Mixed use") %>% 
#   filter(!is.na(dev_dwlgs_total)) %>% 
#   filter(!feature_preventing_development,
#   lot_size < 4000) %>%
#   mutate(lot_size_250 = 250*floor(lot_size/250)) %>% 
#   group_by(lot_size_250,zone_code) %>% 
#   summarise(lots_per_sqm = 10000*sum(dev_dwlgs_total) / sum(lot_size)) %>% 
#   ggplot(aes(x = lot_size_250,
#              y = lots_per_sqm,
#           colour = zone_code))+
#   geom_smooth(stat = "identity")

#From that I think we can assume that dwellings per lots are around .06/sqm or 800 per hectare regardless of lot size, so buxton has woefullyu underestimated for small lots 

#But what about distance from the CBD as a predictor.... 


# buxton_yields_reestimated <- buxton_yields %>% 
#   filter(zone_short == "Mixed use") %>% 
#   filter(!is.na(dev_dwlgs_total)) %>% 
#   filter(!feature_preventing_development,
#          lot_size < 4000,
#          buxton_yield>10) %>% #Filter to where buxton expects >10 units that wayy it's apples and apples since our dev dataset only has >10 unit developments
#   st_drop_geometry() %>% 
#   summarise(lots_per_sqm = 10000*sum(dev_dwlgs_total) / sum(lot_size)) 
  

# Ok let's use these values for Mixed zones for inner/middle/outer LGAs, 
#creating a buxton corrected with more development on existing lots. 

#Let's also create a missing middle zone <500m from public transport, and assume it gets built to the same density as RGZ. 
#We also assume GRZ becomes RGZ and NRZ becomes GRZ. 

buxton_yields_corrected <- buxton_yields %>% 
mutate(area = case_when( lga_name_2022 %in% inner_lgas ~ "inner lgas",
                         lga_name_2022 %in% middle_lgas ~ "middle lgas",
                         lga_name_2022 %in% outer_lgas ~  "outer lgas",
                         lga_name_2022%in% greenfield ~ "greenfield")) %>% 
  mutate(buxton_yields_corrected = case_when(feature_preventing_development ~ 0,
                                             zoning_permits_housing %in% c("Housing not generally permitted",
                                                                           "Rural/regional") ~ 0, 
                                             zone_short %in% c("Mixed use","Commercial") & lga_name_2022 %in% inner_lgas ~ lot_size*830/10000,
                                             zone_short %in% c("Mixed use","Commercial") & lga_name_2022 %in% middle_lgas ~ lot_size*346/10000,
                                             zone_short %in% c("Mixed use","Commercial") & lga_name_2022 %in% outer_lgas ~ lot_size*252/10000,
                                             zone_short %in% c("Mixed use","Commercial") & lga_name_2022 %in% greenfield ~ lot_size*252/10000,
                                             zone_short %in% c("Resindetial Growth") ~ lot_size*240/10000,
                                             T ~buxton_yield),
         buxton_yeilds_corrected_net = floor(pmax(0,buxton_yields_corrected - dwellings_est)),
         zone_short_mm = case_when(zone_short %in% c("General residential",
                                                     "Neighbourhood residential",
                                                     "Residential Growth") & 
                                               (prox_dist_m_tram <= 500 | prox_dist_m_train <= 500) ~ "Missing middle",
                                               zone_short  == "Neighbourhood residential"  ~ "General residential",
                                               zone_short == "General residential" ~ "Residential Growth",
                                   T ~ zone_short),
         missing_middle_yield = 
                  case_when(feature_preventing_development ~ 0,
                            zone_short_mm == "Low density residential" ~ 1,
                            zoning_permits_housing %in% c("Housing not generally permitted",
                                                          "Rural/regional") ~ 0, # Need to sort out CCZ, DZ etc.
                            zone_short_mm == "Neighbourhood residential" & lot_size <500 ~ 1,
                            zone_short_mm == "Neighbourhood residential" & lot_size >=500 & dwellings_est == 1 ~ 1,
                            zone_short_mm == "Neighbourhood residential" & lot_size >= 500 ~ 2,
                            zone_short_mm == "General residential" & lot_size < 450  ~ 1,
                            zone_short_mm == "General residential" & lot_size < 675  ~ 2,
                            zone_short_mm == "General residential" & lot_size < 1000 ~ 3,
                            zone_short_mm == "General residential" & lot_size < 2000 ~ 8,
                            zone_short_mm == "Residential Growth"  & lot_size < 225  ~ 1,
                            zone_short_mm == "Residential Growth"  & lot_size < 500  ~ 8,
                            zone_short_mm == "Residential Growth"  & lot_size < 1000 ~ 18,
                            zone_short_mm == "Residential Growth"  & lot_size < 2000 ~ 36,
                           # zone_short_mm %in% c("Residential Growth") ~ lot_size*240/10000, # What actually happens
                            zone_short_mm %in% c("Residential Growth","Mixed use","Commercial","General residential") & lga_name_2022 %in% inner_lgas ~ lot_size*830/10000,
                            zone_short_mm %in% c("Residential Growth","Mixed use","Commercial","General residential") & lga_name_2022 %in% middle_lgas ~ lot_size*346/10000,
                            zone_short_mm %in% c("Residential Growth","Mixed use","Commercial","General residential") & lga_name_2022 %in% outer_lgas ~ lot_size*252/10000,
                            zone_short_mm %in% c("Residential Growth","Mixed use","Commercial","General residential") & lga_name_2022 %in% greenfield ~ lot_size*252/10000,
                            zone_short_mm %in% c("Missing middle") ~ lot_size*6/4*240/10000, # Missing middle will be 6 instead of 4 storeys so this is our best estimate... 
                            T ~ NA_real_),
                mm_yield_net = floor(pmax(0,missing_middle_yield - dwellings_est))
                ) %>% 
  mutate(category = case_when(feature_preventing_development                    ~ "Civic use makes development less likely",
                              dwellings_est >1                                  ~ "Already developed",
                              zoning_permits_housing != "Housing permitted"     ~ "Housing not permitted",
                              zone_short == "Neighbourhood residential"         ~ "2 storeys (NRZ)",
                              zone_short == "General residential"               ~ "3 storeys (GRZ)", 
                              zone_short == "Residential Growth"                ~ "4 storeys (RGZ)",
                              zone_short == "Mixed use"                         ~ "4+ storeys (Mixed use zones)",
                              T ~ zone_short),
         category_new = case_when(feature_preventing_development                ~ "Civic use makes development less likely",
                                  dwellings_est >1                              ~ "Already developed",
                                  zoning_permits_housing != "Housing permitted" ~ "Housing not permitted",
                                  zone_short_mm == "General residential"        ~ "3 storeys (GRZ)", 
                                  zone_short_mm == "Neighbourhood residential"  ~ "2 storeys (NRZ)",
                                  zone_short_mm == "Residential Growth"         ~ "4 storeys (RGZ)",
                                  zone_short_mm == "Mixed use"                  ~ "4+ storeys (Mixed use zones)",
                                  zone_short_mm == "Missing middle"             ~ "6 storeys (Missing middle)",
                                  T ~ zone_short_mm),
         type_short = case_when(feature_preventing_development ~"Civic land",
                                dwellings_est>1 ~ "Already developed",
                                T ~ zone_short),
         heritage_nice = if_else(heritage,
                                 "Subject to heritage controls",
                                 "Free from heritage")
  ) %>% 
  mutate(category    = as.factor(category),
         category_new = as.factor(category_new)) 


output <- input_data %>% 
  select(lat,lon) %>%  
  left_join(buxton_yields_corrected)#%>% 
 # rename_with(~ if_else(str_detect(.x, "geometry"), "geom", .x), .cols = contains("geometry")) %>% 
 # st_set_geometry("geom")

return(output)

}

#How do these different measured of zoned capacity change? 
# 
# buxton_yields_corrected %>% 
#   st_drop_geometry() %>% 
#   summarise(buxton_yield_net            = sum(buxton_yield_net),
#             buxton_yeilds_corrected_net = sum(buxton_yeilds_corrected_net),
#             mm_yield_net                = sum(mm_yield_net))
# 
# 
# 
# 
# 
# input_data %>% 
#  filter(str_detect(dev_prj_name,"Nightingale")) %>% 
#   st_drop_geometry() %>% 
#   select(dev_height_storeys,dwellings_est,lot_size,dev_prj_name) %>% 
#   mutate(units_per_lot = dwellings_est/(lot_size/1000),
#          units_if_it_were_6_storeys = units_per_lot*6/dev_height_storeys) %>%  
#   view()
#   
# 
# 
# buxton_yields_corrected %>% 
#   st_drop_geometry() %>%
#   group_by(area,zone_short_mm) %>% 
#   summarise(increase_required = sum(mm_yield_net)) %>% 
#   ungroup() %>% 
#   mutate(area = fct_reorder(area,increase_required)) %>% 
#   ggplot(aes(x = zone_short_mm, y = increase_required))+
#   geom_bar(stat = "identity") +
#   coord_flip()+
#   labs(title = "where would population go if homes were build evenly across\nYIMBY Melbourne's 'missing middle' plan")+
#   facet_wrap(.~area, scales = "free_y",ncol = 1)
# 
# 
# buxton_yields_corrected %>% 
#   st_drop_geometry() %>% 
#   group_by(zone_short_mm,
#            lga_name_2022,area) %>% 
#   summarise(n=n()) %>% 
#   spread(zone_short_mm,n) %>% view()
