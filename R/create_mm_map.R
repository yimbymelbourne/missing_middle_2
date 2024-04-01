
make_map <- function(sf_lga_props,map_type){
  
  
  # Assume lots_with_new_zoning$category has levels A, B, C, D, E
  # And you want to specify colors for A and B
  
  # Specific colors for certain levels
  
  #map_type = "category"
  
  specified_colors <- c("Already developed" = "#989898",
                        "Housing not permitted" = "#b1b1b1",
                        "Civic use makes development less likely" = "#cacaca", 
                        "Low density residential" = yimby_colours$blue_palette[5],
                        "2 storeys (NRZ)" =  yimby_colours$blue_palette[4],
                        "3 storeys (GRZ)" =  yimby_colours$blue_palette[3],
                        "4 storeys (RGZ)" =  yimby_colours$blue_palette[2],
                        "4+ storeys (Mixed use zones)" =  yimby_colours$blue_palette[1],
                        "6 storeys (Missing middle)" = yimby_colours$green_palette[3]
  )
  
  # Fallback colors
  fallback_colors <- rev(yimby_colours$yellow_palette)  # Assuming this is a vector of color codes
  
  # All unique levels of the factor
  all_levels <- levels(sf_lga_props[[map_type]])
  
  # Prepare a color vector that includes specified colors and fills the rest from fallback_colors
  colors_for_all_levels <- ifelse(all_levels %in% names(specified_colors),
                                  specified_colors[all_levels],
                                  fallback_colors[1:length(all_levels)])
  
  # Ensure the length of fallback colors is sufficient
  if (length(colors_for_all_levels) < length(all_levels)) {
    warning("Not enough fallback colors provided. Repeating fallback colors to match the number of levels.")
    needed <- length(all_levels) - length(colors_for_all_levels)
    colors_for_all_levels <- c(colors_for_all_levels, rep(fallback_colors, length.out = needed))
  }
  
  # Assign names to ensure the colors align with the factor levels
  names(colors_for_all_levels) <- all_levels
  
  # Use colors_for_all_levels in your plotting function
  palette <- colorFactor(palette = colors_for_all_levels, domain = sf_lga_props[[map_type]])
  
  
  output <- sf_lga_props %>%
    st_as_sf() %>% 
    mutate(map_var = as.factor(.data[[map_type]])) %>%
    group_by(map_var) %>% 
    summarise(geom = st_union(st_combine(geom))) %>% 
    st_simplify() %>% 
    st_transform("wgs84") %>% 
    st_simplify() %>% 
    mutate(map_var = fct_relevel(map_var,names(specified_colors))) %>%
    leaflet(width = "100%") %>%
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(weight = .1,
                color = "black",
                fillColor = ~palette(map_var),
                fillOpacity = .8,
                label = ~map_var,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = palette,
              values = ~map_var,
              title = "Type of land",
              position = "bottomright",
              opacity = .8)
  return(output)
  
}
