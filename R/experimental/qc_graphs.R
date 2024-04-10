
#Designed to be run inside run_rmd.R

#A series of further QC graphs but this time unlike hte RMD in 03_run_regression we're using the full complex way of calculating what apartments are profitable. 
#Test for good apartments.... 
sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  ggplot(aes(x = profit_per_apartment,
             fill = `profitable?`)) +
  geom_histogram() +
  facet_wrap(~zone_short_mm)

sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  filter(apartments_if_built_to_this_height <100) %>% 
  ggplot(aes(x = apartments_if_built_to_this_height,
             fill = `profitable?` ))+
  geom_histogram()

hist(sf_mel_props$missing_middle_storeys)
sf_mel_props %>% 
  mutate(`profitable?` = if_else(profit>0,T,F)) %>% 
  filter(apartments_if_built_to_this_height <100) %>% 
  ggplot(aes(x = storey,
             fill = `profitable?` ))+
  geom_histogram()

hist(sf_mel_props$construction_cost_per_apt_this_height)
hist(sf_mel_props$profit_per_apartment)


sf_mel_props %>% 
  distinct(construction_cost_per_apt_this_height,storey,zone_short_mm) %>% 
  ggplot(aes(y = construction_cost_per_apt_this_height,
             x = storey,
             colour =  zone_short_mm)) +
  geom_point()

sf_mel_props %>% 
  mutate(prf_ap = profit_per_apartment) %>% 
  select(profitable_apartments,prf_ap,zone_short_mm,mm_yield_net,zone_short_mm,storey,lot_size,apartments_if_built_to_this_height) %>% 
  filter(!is.na(profitable_apartments)) %>% 
  write_sf("test/profit_per_apartment.shp")