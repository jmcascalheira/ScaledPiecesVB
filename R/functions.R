
###########################################################################
# READ DATA ----------------------------------------------------------------

read.data <- function(){
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

cross.tb <- function(dataset,x,y) {

require(tab)
require(tidyverse)

dataset <- dataset %>% mutate_if(is.character,as.factor)
dataset <- as.data.frame(dataset)

  ct <- tabmulti(dataset, x, y,
                  cell = "n",
                  parenth = "col.percent",
                  p.include = FALSE,
                  n.headings = FALSE,
                  freq.text.label = "none")

  #ct <- ct %>%
   #as.tibble() %>%
   #dplyr::select(" " = Variable, Chert, Quartz, Chalcedony, Total = Overall)

}

###########################################################################
# MEAN PLOTS --------------------------------------------------------------

mean.plot <- function(dataset, x, y, z){

  ggbarplot(dataset, x = x, y = y,
            add = c("mean_sd", "jitter"), size = 1,
            color = z, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            position = position_dodge(0.8),
            xlab = "") +
  scale_x_discrete(limits=c("Gravettian", "Proto-Solutrean","Solutrean", "Magdalenian"))

}


###########################################################################
# BOXPLOT WITH JITTER -----------------------------------------------------

box.plot <- function(df, x_var, y_var, x_label, y_label){
  b_plot<-ggplot(df,aes_string(x= x_var, y=y_var)) +
    geom_boxplot(outlier.shape = NA)

  b_plot <- b_plot +
    geom_jitter(position=position_jitter(width=.2, height=0), aes(color = RawMaterial), size = 2) +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Raw Material") +
    labs(x = x_label, y = y_label)+
    scale_fill_discrete(guide=FALSE)+
    theme_classic()

  return(b_plot)
}


###########################################################################
# INLINE PERCENTAGE -------------------------------------------------------


inline.perc <- function(dataset, x, y){

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

########################################################################################
# RECODE TO OTHER BASED ON 10% FREQUENCY -----------------------------------------------

condense.to.other <- function(df, x){

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

####### Alternative to all columns:

condense.to.other.all <- function(df){

  nm <- colnames(df)

  for (i in nm){
    m <- df %>%
      count_(i) %>%
      mutate(perc = n/sum(n)) %>%
      filter(perc < 0.10)

    z <- as.character(t(m[1]))

    for(x in z){

      df[[i]] <- ifelse(df[[i]] == x , "Other", df[[i]])

    }
  }
  return(df)
}


########################################################################################
# RECODE TO NA BASED ON 10% FREQUENCY -----------------------------------------------

condense.to.NA <- function(df, x){

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


####### Alternative to all columns:

condense.to.NA.all <- function(df){

  nm <- colnames(df)

  for (i in nm){
    m <- df %>%
      count_(i) %>%
      mutate(perc = n/sum(n)) %>%
      filter(perc < 0.10)

    z <- as.character(t(m[1]))

    for(x in z){

      df[[i]] <- ifelse(df[[i]] == x, NA, df[[i]])

    }
  }
  return(df)
}


###########################################################################
# CHI_SQUARE, EFFECT SIZE AND RATIO----------------------------------------


CHI <- function(x, y) {

  chi_test <- chisq.test(table(x, y))
  chi_size_effect <- ES.chisq.assoc(ct=table(x, y))

  return(list(chi_test=chi_test,
        chi_size_effect=chi_size_effect))

}


###########################################################################
# ANOVA AND COHENS TESTS --------------------------------------------------------

# ANOVA test

run.Anova<-function(df, y_var){
  frm<-as.formula(sprintf("%s~%s", y_var, "RawMaterial"))
  compare_aov<-aov(frm, data=df)
  return(compare_aov)
}


anova.test <- function(df, y_var){
  compare_aov = run.Anova(df, y_var)
  return(compare_aov)
}


# COHENS test

cohens.test <- function(df){
  compare_cohens <- cohens_f(df)
  return(compare_cohens)
}


###########################################################################
# JOIN AND RECODE OPPOSED PLATFORMS ---------------------------------------

opposed.plat.morpho <- function(){


  # Select Platforms A and B
  morpho_data_A_B <- morpho_data %>%
    filter(DamagedPlatforms != "1" & DamagedPlatforms != "3") %>%
    mutate(ScarDistribution = paste(ScarDistribution_PlatformA, ScarDistribution_PlatformB, sep = "/")) %>%
    mutate(ScarArrangement = paste(ScarArrangement_PlatformA, ScarArrangement_PlatformB, sep = "/")) %>%
    mutate(ScarExtension = paste(ScarExtention_PlatformA, ScarExtention_PlatformB, sep = "/")) %>%
    mutate(ScarEdgeDelineation = paste(ScarEdgeDelineation_PlatformA, ScarEdgeDelineation_PlatformB, sep = "/")) %>%
    mutate(ScarFaciality = paste(ScarFacialDistribution_PlatformA, ScarFacialDistribution_PlatformB, sep = "/")) %>%
    mutate(Angle = paste(Angle_PlatformA, Angle_PlatformB, sep = "/")) %>%
    select(RawMaterial,ScarDistribution:Angle)

  # Select Platforms C and D
  morpho_data_C_D <- morpho_data %>%
    filter(DamagedPlatforms == "4_or_more") %>%
    mutate(ScarDistribution = paste(ScarDistribution_PlatformC, ScarDistribution_PlatformD, sep = "/")) %>%
    mutate(ScarArrangement = paste(ScarArrangement_PlatformC, ScarArrangement_PlatformD, sep = "/")) %>%
    mutate(ScarExtension = paste(ScarExtention_PlatformC, ScarExtention_PlatformD, sep = "/")) %>%
    mutate(ScarEdgeDelineation = paste(ScarEdgeDelineation_PlatformC, ScarEdgeDelineation_PlatformD, sep = "/")) %>%
    mutate(ScarFaciality = paste(ScarFacialDistribution_PlatformC, ScarFacialDistribution_PlatformD, sep = "/")) %>%
    mutate(Angle = paste(Angle_PlatformC, Angle_PlatformD, sep = "/")) %>%
    select(RawMaterial,ScarDistribution:Angle)

  # Bind all opposed platforms
  morpho_data_opposed_platforms <- bind_rows(morpho_data_A_B,morpho_data_C_D)

  # Remove Chalcedony
  morpho_data_opposed_platforms <- filter(morpho_data_opposed_platforms, RawMaterial != "Chalcedony")


  # Recode repeated combinations (need function!!)
  morpho_data_opposed_platforms <- morpho_data_opposed_platforms %>%
    mutate(ScarDistribution = dplyr::recode(ScarDistribution, "Lateral_Central/Central" = "Central/Lateral_Central",
                                            "Lateral_Central/Total" = "Total/Lateral_Central",
                                            "Total/Central" = "Central/Total")) %>%
    mutate(ScarArrangement = dplyr::recode(ScarArrangement, "Overlapped/Isolated" = "Isolated/Overlapped", "Aligned_Overalapped/Overlapped" = "Overlapped/Aligned_Overlapped", "Overlapped/Aligned" = "Aligned/Overlapped", "Aligned_Overlapped/Aligned" = "Aligned/Aligned_Overlapped", "Aligned_Overlapped/Overlapped" = "Overlapped/Aligned_Overlapped", "Isolated/Aligned" = "Aligned/Isolated", "Aligned_Overlapped/Isolated" = "Isolated/Aligned_Overlapped")) %>%
    mutate(ScarExtension = dplyr::recode(ScarExtension, "Invasive/Marginal" = "Marginal/Invasive",
                                         "Mixed_Invasive/Marginal" = "Marginal/Mixed_Invasive",
                                         "Marginal/Mixed_Marginal" = "Mixed_Marginal/Marginal",
                                         "Invasive/Mixed_Invasive" = "Mixed_Invasive/Invasive",
                                         "Mixed_Invasive/Mixed_Marginal" = "Mixed_Marginal/Mixed_Invasive",
                                         "Mixed/Marginal" = "Marginal/Mixed",
                                         "Mixed/Invasive" = "Invasive/Mixed",
                                         "Mixed/Mixed_Invasive" = "Mixed_Invasive/Mixed",
                                         "Mixed/Mixed_Marginal" = "Mixed_Marginal/Mixed")) %>%
    mutate(ScarEdgeDelineation = dplyr::recode(ScarEdgeDelineation, "Concave/Pointed" = "Pointed/Concave",
                                               "Oblique/Straight" = "Straight/Oblique",
                                               "Pointed/Oblique" = "Oblique/Pointed",
                                               "Pointed/Straight" = "Straight/Pointed",
                                               "POinted/Irregular" = "Irregular/Pointed",
                                               "Pointed/Convex" = "Convex/Pointed",
                                               "Concave/Oblique" = "Oblique/Concave",
                                               "Concave/Straight" = "Straight/Concave",
                                               "Concave/Irregular" = "Irregular/Concave",
                                               "Concave/Convex" = "Convex/Concave",
                                               "Oblique/Irregular" = "Irregular/Oblique",
                                               "Oblique/Convex" = "Convex/Oblique",
                                               "Straight/Irregular" = "Irregular/Straight",
                                               "Irregular/Convex" = "Convex/Irregular")) %>%
    mutate(ScarFaciality = dplyr::recode(ScarFaciality, "Bifacial/Unifacial" = "Unifacial/Bifacial")) %>%
    mutate(Angle= dplyr::recode(Angle, "Platform/<45" = "<45/Platform", "Platform/>45" = ">45/Platform",
                                "<45/>45" = ">45/<45"))


  # Condense

  morpho_data_opposed_platforms <- condense.to.other.all(morpho_data_opposed_platforms)
  morpho_data_opposed_platforms <- condense.to.NA.all(morpho_data_opposed_platforms)

  return(morpho_data_opposed_platforms)

}

 # Plot all variables

plot.morpho.var <- function(x){

  dist_perc <- x %>%
    group_by(RawMaterial, ScarDistribution) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  dist <- ggbarplot(dist_perc, x = "ScarDistribution", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                    ylab = FALSE)



  arr_perc <- x %>%
    group_by(RawMaterial, ScarArrangement) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  arr <- ggbarplot(arr_perc, x = "ScarArrangement", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                   ylab = FALSE)


  ext_perc <- x %>%
    group_by(RawMaterial, ScarExtension) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  ext <- ggbarplot(ext_perc, x = "ScarExtension", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                   ylab = FALSE)


  deli_perc <- x %>%
    group_by(RawMaterial, ScarEdgeDelineation) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  deli <- ggbarplot(deli_perc, x = "ScarEdgeDelineation", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                    ylab = FALSE)


  facial_perc <- x %>%
    group_by(RawMaterial, ScarFaciality) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  facial <- ggbarplot(facial_perc, x = "ScarFaciality", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                    ylab = FALSE)

  angle_perc <- x %>%
    group_by(RawMaterial, Angle) %>%
    tally() %>%
    group_by(RawMaterial) %>%
    mutate(pct = (n / sum(n))*100)

  angle <- ggbarplot(angle_perc, x = "Angle", y = "pct",
                    position = position_dodge(0.9),
                    fill = "RawMaterial", color = "RawMaterial",
                    palette = c("#00AFBB", "#E7B800"),
                    orientation = "horiz",
                    xlab = FALSE,
                    ylab = FALSE)

  return(ggarrange(dist, arr, ext, deli, facial, angle, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom", align = "hv", labels="AUTO"))

}

###########################################################################
# MCA ---------------------------------------------------------------------

mca.scaled.pieces <- function(){


  morpho_data_mca <- MCA(morpho_data_opposed_platforms, quali.sup = 1, graph = FALSE)

  return(morpho_data_mca)

}

