# r_files <- list.files(path = "R/functions/",
#                       pattern = "*.R$",
#                       full.names = T)

# purrr::walk(r_files,source)

save_file <- c("data/rmd_data.qs")

if(!file.exists(save_file)){
  download.file("https://yimby-mel.s3.ap-southeast-2.amazonaws.com/rmd_data.qs",destfile = save_file)
}

sf_mel_props <- qs::qread(save_file)

df_mel_props <- sf_mel_props %>% st_drop_geometry() 


# Filtering for houses that can be upzoned
upzoned_props <- df_mel_props %>%
  filter(!feature_preventing_development,
         dwellings_est <=1,
         zoning_permits_housing == "Housing permitted") %>%
  filter(zone_short_mm %in% c("Missing middle", "Residential growth", "General residential"))


# X: upzoned properties total
total_upzoned_props <- upzoned_props %>% nrow()
total_upzoned_props
# 781956


# Y: median price
# Each property has both an apartment price and a property price.
# The logic above suggests we are looking at houses/properties

upzoned_value_median_property <- median(upzoned_props$property_price, na.rm = TRUE)
upzoned_value_median_property
# 1190347 

upzoned_value_mean_property <- mean(upzoned_props$property_price, na.rm = TRUE)
upzoned_value_mean_property
# 1345869 


# Z: median price
# Each property has both an apartment price and a property price.
# Presumably the vast majority of these upzone-potential properties are houses

total_upzoned_value_median_property <- upzoned_value_median_property * total_upzoned_props
total_upzoned_value_median_property
# 9.30799e+11 or $931B

total_upzoned_value_mean_property <- upzoned_value_mean_property * total_upzoned_props
total_upzoned_value_mean_property
# 1.05241e+12 or $1,052B
