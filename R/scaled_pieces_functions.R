
###########################################################################
# READ DATA ----------------------------------------------------------------

read_data <- function(){
  tech_data <- read.csv("../data/raw_data/technology_dataset.csv")

}


###########################################################################
# MAPS --------------------------------------------------------------------

require(maptools)
require(ggplot2)
require(ggmap)
require(devtools)
require(legendMap)

Map <- function(){

  VBLocation <- c(lon = -8.808621, lat = 37.089902)

  VBMap <- get_googlemap(center = c(lon = -8.101284, lat = 39.486585),
                         zoom = 7,
                         scale = 2,
                         maptype = "roadmap")

  VBMap <-
    ggmap(VBMap) +
    geom_point(data = data.frame(t(VBLocation)),
               aes(lon,
                   lat),
               size = 5,
               colour = "black") +
    geom_text(data = data.frame(t(VBLocation)), aes(x = lon, y = lat, label = "Vale Boi"),
              size = 4, colour = "black", vjust = -1, hjust = 0.5)


  VBMap <- VBMap +
    legendMap::scale_bar(lon = -11.3,
                         lat = 37,
                         distance_lon = 50,
                         distance_lat = 8,
                         distance_legend = 20,
                         legend_colour = "white",
                         dist_unit = "km",
                         orientation = TRUE,
                         arrow_length = 50,
                         arrow_distance = 50,
                         arrow_north_size = 3)

  ggsave("VBMap.png")

}


###########################################################################
# CROSS TABLES ------------------------------------------------------------



cross_tb <- function(dataset,x,y) {

require(tab)
require(tidyverse)

  ct <- tabmulti(dataset, x, y,
                  cell = "n",
                  parenth = "col.percent",
                  p.include = FALSE,
                  n.headings = FALSE,
                  freq.text.label = "none")

  ct <- ct %>%
    as.tibble() %>%
    dplyr::select(" " = Variable, Chert, Quartz, Chalcedony, Total = Overall)

}


mean_tb <- function(dataset,x,y) {

  require(tab)
  require(tidyverse)

  mt <- tabmulti(dataset, x, y,
                 cell = "n",
                 parenth = "col.percent",
                 p.include = FALSE,
                 n.headings = FALSE,
                 freq.text.label = "none",
                 means.text.label = "none")

  mt <- mt %>%
    as.tibble() %>%
    dplyr::select(" " = Variable, Chert, Quartz, Chalcedony, Overall)

}


mean_plot <- function(dataset, x, y, z){

  ggbarplot(techno_data, x = x, y = y,
            add = c("mean_se", "jitter"), size = 1,
            color = z, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            position = position_dodge(0.8)) +
  theme_gray()

}


