yimby_colours <- list(
  hero = "#10461B",
  body = "#000000",
  background  = "#FDFFEE",
  green_base  = "#10461B",
  blue_base   = "#283696",
  red_base    = "#BA1B21",
  yellow_base = "#F6AE00",
  green_palette  = c("#10461B", "#2C6F3A", "#579A64", "#8FC49A", "#D6EFDB"),
  blue_palette   = c("#1A235F", "#283696", "#9EA7E2", "#D0D6FF", "#E8ECFF"),
  red_palette    = c("#D92127", "#BA1B21", "#8E3437", "#C6A2A3", "#E3BABB"),
  yellow_palette = c("#F6AE00", "#DE9B00", "#C79E3E", "#F2DCA5", "#FFF7E3")
)

#Make sure inter, lato and inter tight are in your system font dialogue...
#Need to run this once to import system fonts...
#library(extrafont)
#loadfonts(device = "all")
#font_import()

library(showtext)
library(ggtext)

font_add("Inter", "theme/atlas/fonts/Inter/static/Inter-Regular.ttf")
font_add("Inter Tight", 
         "theme/atlas/fonts/Inter_Tight/static/InterTight-Regular.ttf",
         "theme/atlas/fonts/Inter_Tight/static/InterTight-Bold.ttf")
font_add("Lato", "theme/atlas/fonts/Lato/Lato-Regular.ttf")

showtext_auto()

yimby_colours$complementary <- c(yimby_colours$green_palette[1],
                             yimby_colours$red_palette[1],
                             yimby_colours$blue_palette[1],
                             yimby_colours$yellow_palette[1])

yimby_colours$light_dark <- c(yimby_colours$green_palette[1],
                              yimby_colours$green_palette[3],
                              yimby_colours$blue_palette[1],
                              yimby_colours$blue_palette[3])


computer_type <- Sys.info()[[1]]

theme_yimby_mel_caption <- function(caption_text = " ",
                             plot_type = "bar",
                             colour_scale = "complementary",
                             text_size = "big"){
  
  if(text_size == "big") {font_size = 70;  pic_height = 50  } else {font_size = 12;  pic_height = 20}
  
  if(text_size == 'Linux' & text_size == "giant"){font_size = 70}
  
  update_geom_defaults("bar",  list(fill = yimby_colours$green_palette[1], colour = yimby_colours$green_palette[1]))
  update_geom_defaults("col",  list(fill = yimby_colours$green_palette[1], colour = yimby_colours$green_palette[1]))
  update_geom_defaults("point",list(fill = yimby_colours$green_palette[1], colour = yimby_colours$green_palette[1]))
  update_geom_defaults("line", list(fill = yimby_colours$green_palette[1],  colour =yimby_colours$green_palette[1]))
  update_geom_defaults("smooth", list(fill = yimby_colours$green_palette[1],  colour =yimby_colours$green_palette[1]))
  update_geom_defaults("sf",   list(fill = yimby_colours$green_palette[1],  colour =yimby_colours$green_palette[1]))
  
  
  #Pull in fill types
  fill_colours <- function(){
    if(substr(tolower(colour_scale),1,1) == "c")  {   op <-   scale_fill_manual(values = yimby_colours$complementary)
    } else if(substr(tolower(colour_scale),1,4) == "gree") {   op <-   scale_fill_manual(values = yimby_colours$green_palette)
    } else if(substr(tolower(colour_scale),1,4) == "ligh") {   op <-   scale_fill_manual(values = yimby_colours$light_dark)
    } else if(substr(tolower(colour_scale),1,4) == "blue") {   op <-   scale_fill_manual(values = yimby_colours$blue_palette)
    } else if(substr(tolower(colour_scale),1,3) == "red") {   op <-   scale_fill_manual(values = yimby_colours$red_palette)
    } else if(substr(tolower(colour_scale),1,4) == "yellow") {   op <-   scale_fill_manual(values = yimby_colours$yellow_palette)
    
    } else {                                                  op <- list()}
    update_geom_defaults("bar",  list(colour = "white"))
    
    return(op) 
  }
  
  #Pull in fill types
  colour_colours <- function(){
    if(substr(tolower(colour_scale),1,1) == "c")  {  op <-   scale_colour_manual(values = yimby_colours$complementary)
    } else if(substr(tolower(colour_scale),1,4) == "ligh") {   op <-   scale_colour_manual(values = yimby_colours$light_dark)
    } else if(substr(tolower(colour_scale),1,4) == "gree") {   op <-   scale_colour_manual(values = yimby_colours$green_palette)
    } else if(substr(tolower(colour_scale),1,4) == "blue") {   op <-   scale_colour_manual(values = yimby_colours$blue_palette)
    } else if(substr(tolower(colour_scale),1,3) == "red") {   op <-   scale_colour_manual(values = yimby_colours$red_palette)
    } else if(substr(tolower(colour_scale),1,4) == "yellow") {   op <-   scale_colour_manual(values = yimby_colours$yellow_palette)
    
    } else {                                                  op <- list()}
    
    
    return(op) 
  }
  
  x <- glue::glue(
    "<span style='color:{yimby_colours$body}; display: inline;'>{caption_text}</span>
<img src='theme/atlas/Logo-transparent-green.png' height='{pic_height}' /><br>"
  )
  
  output <- list(theme(plot.background = element_rect(fill = yimby_colours$background, 
                                                      colour = yimby_colours$background),
                       text = element_text(size=font_size,
                                           family = "Lato"),
                       plot.title  = element_text(size=font_size, 
                                                  family = "Inter Tight",
                                                  face = "bold",
                                                  color=yimby_colours$hero, 
                                                  angle=0),
                       plot.subtitle  = element_text(color=yimby_colours$body,
                                                     family = "Inter"),
                       axis.title  = element_text(color=yimby_colours$body),
                       legend.title =    element_text(color=yimby_colours$body),
                       plot.caption   = ggtext::element_markdown(lineheight = 1.2)
  ),
  labs(caption = x))
  
  non_map_elements <- list(theme_minimal()+
                             theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank()))
  
  if(plot_type %in%  c("bar","column")) { 
    
    output <- list(non_map_elements,
                   fill_colours(),
                   output)
  }
  
  if(plot_type %in%  c("point","line","dot")){ 
    
    output <- list(non_map_elements,
                   colour_colours(),
                   output)
  }
  
  if(plot_type == "map"){ 
    output <- list(ggthemes::theme_map(),
                   colour_colours(),
                   fill_colours(),
                   output)}
  
  return(output)
}

#library(tidyverse)
# mtcars %>% ggplot(aes(x = mpg,
#                       y = carb
#                       #colour = as.factor(cyl))
#                       )) +
#   geom_point(stat = "identity",size = 5) + 
#   theme_yimby_mel_caption("test caption",plot_type = "point")+
#   labs(title = "Yimby Melbourne",
#        subtitle = "Graph subtitle")


