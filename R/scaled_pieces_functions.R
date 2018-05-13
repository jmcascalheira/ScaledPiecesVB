
###########################################################################
# READ DATA ----------------------------------------------------------------

read_data <- function(){
  #tech_attributes <- readcsv("../data/raw_data/technology_attributes.csv")
  #morpho_attributes <- read.csv("../data/raw_data/morpho_attributes.csv")
  techno_data <- read_csv("../data/raw_data/technology_dataset.csv")
  morpho_data <- read_csv("../data/raw_data/morpho_dataset.csv")

  return(list(morpho_data = morpho_data,
              techno_data = techno_data))

}


###########################################################################
# MAPS --------------------------------------------------------------------

Map <- function(){

  require(maptools)
  require(ggplot2)
  require(ggmap)
  require(devtools)
  require(legendMap)

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

  ggsave("../figures/VBMap.png")

}


###########################################################################
# CROSS TABLES ------------------------------------------------------------



cross_tb <- function(dataset,x,y) {

require(tab)
require(tidyverse)

dataset <- as.data.frame(dataset)

  ct <- tabmulti(dataset, x, y,
                  cell = "n",
                  parenth = "col.percent",
                  p.include = FALSE,
                  listwise.deletion = TRUE,
                  n.headings = FALSE,
                  freq.text.label = "none")

  ct <- ct %>%
   as.tibble() %>%
   dplyr::select(" " = Variable, Chert, Quartz, Chalcedony, Total = Overall)

}


###########################################################################
# MEAN TABLE --------------------------------------------------------------


mean_tb <- function(dataset,x,y) {

  require(tab)
  require(tidyverse)

  mt <- tabmulti(dataset, x, y,
                 cell = "n",
                 parenth = "col.percent",
                 p.include = TRUE,
                 n.headings = FALSE,
                 freq.text.label = "none",
                 means.text.label = "none")

 # mt <- mt %>%
  #  as.tibble() %>%
   # dplyr::select(" " = Variable, Chert, Quartz, Chalcedony, Overall)

}



###########################################################################
# MEAN PLOTS --------------------------------------------------------------

mean_plot <- function(dataset, x, y, z){

require(ggpubr)

  ggbarplot(dataset, x = x, y = y,
            add = c("mean_sd", "jitter"), size = 1,
            color = z, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            position = position_dodge(0.8),
            xlab = "") +
  scale_x_discrete(limits=c("Gravettian", "Proto-Solutrean","Solutrean", "Magdalenian"))

}


###########################################################################
# BOXPLOT WITH JITTER -----------------------------------------------------

box_plot <- function(df, x_var, y_var, x_label, y_label){
  b_plot<-ggplot(df,aes_string(x= x_var, y=y_var)) +
    geom_boxplot(outlier.shape = NA, fill = "gray")

  b_plot <- b_plot +
    geom_jitter(position=position_jitter(width=.2, height=0), aes(color = RawMaterial), size = 4) +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Raw Material") +
    labs(x = x_label, y = y_label)+
    scale_fill_discrete(guide=FALSE) +
    theme(text = element_text(size = 20), axis.text = element_text(size = 20))

  return(b_plot)
}


###########################################################################
# INLINE PERCENTAGE -------------------------------------------------------


inline_perc <- function(dataset, x, y){

  x <- enquo(x)

    perc <- dataset %>%
      group_by(!!x) %>%
      tally() %>%
      na.omit()
    perc <- mutate(perc, n = (n/sum(n))*100)
    perc <- filter(perc, !!x == y)
    perc <- as.data.frame(perc)

    round(perc[1,2], digits = 1)

}


###########################################################################
# LIST OF VARIABLES TO COMPARE WITH STAT_COMPARE_MEANS --------------------

compare_list <- function(dataset,x){

  my_comparisons <- vector("list")
  un_values <- unique(dataset[[x]])
  un_values <- as.data.frame(combn(un_values,2))
  var_names <- colnames(un_values)

  for(i in var_names) {
    g <- assign(i, un_values[[i]])

    my_comparisons[[i]] <- g
    names(my_comparisons[i]) <- paste("ct", i, sep = "_")

  }

  list2env(my_comparisons, envir = .GlobalEnv)
}


###########################################################################
# MEAN PLOTS WITH STAT RESULTS ---------------------------------------------

mean_plot_stat_results <- function(dataset, x, y, z){

  compare_list(dataset, x)

  thickness_plot <- ggbarplot(techno_data, x = x, y = y,
                              add = c("mean_sd", "jitter"), size = 1,
                              color = z, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                              position = position_dodge(0.8),
                              xlab = "") +
    stat_compare_means(comparisons = my_comparisons) +
    #scale_x_discrete(limits=c("Gravettian", "Proto-Solutrean","Solutrean", "Magdalenian")) +
    theme_gray()
}



########################################################################################
# RECODE TO OTHER BASED ON 10% FREQUENCY -----------------------------------------------

condense_to_other <- function(df, x){

  m <- df %>%
    count_(x) %>%
    mutate(perc = n/sum(n)) %>%
    filter(perc < 0.10)

  z <- as.character(t(m[1]))

  for(i in z){

    df[[x]] <- ifelse(df[[x]] == i , "Other", df[[x]])

  }
  return(df)
}

########################################################################################
# RECODE TO NA BASED ON 10% FREQUENCY -----------------------------------------------

condense_to_NA <- function(df, x){

  m <- df %>%
    count_(x) %>%
    mutate(perc = n/sum(n)) %>%
    filter(perc < 0.10)

  z <- as.character(t(m[1]))

  for(i in z){

    df[[x]] <- ifelse(df[[x]] == i , NA, df[[x]])

  }
  return(df)
}


###########################################################################
# CHI_SQUARE --------------------------------------------------------------

CHI <- function(x, y) {

  test <- chisq.test(table(x, y))
  result <- c(test$p.value)

  return(result)

}


