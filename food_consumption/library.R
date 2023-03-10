import_library <- function(){
  # Packages Required
  if(!require("tidyverse")) install.packages("tidyverse")
  if(!require("tmap")) install.packages("tmap")
  if(!require("rnaturalearth")) install.packages("rnaturalearth")
  if(!require("sf")) install.packages("sf")
  if(!require("ggtext")) install.packages("ggtext")
  if(!require("scales")) install.packages("scales")
  if(!require("ggdist")) install.packages("ggdist")
  if(!require("ggrepel")) install.packages("ggrepel")
  
  # Custom functions required
  source('scatter_dat.R')
  source('bar_dat.R')
  source('dotplot_fn.R')
  source('create_map.R')
  source('scatter_f.R')
  source('raincloud_plot.R')
  source('future_co2.R')
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
}