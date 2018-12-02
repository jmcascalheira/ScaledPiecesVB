# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.4.4

# required
MAINTAINER João Cascalheira <jmcasca@gmail.com>

COPY . /scaledpiecesvb

# go into the repo directory
RUN . /etc/environment \

  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \

  # build this compendium package
  && R -e "devtools::install('/ScaledPiecesVB', dep=TRUE)" \

 # render the manuscript into a docx
  && R -e "rmarkdown::render('/ScaledPiecesVB/analysis/paper/paper.Rmd')"
