############################################################
#                                                          #
#                      LOAD PACKAGES                       #
#                                                          #
############################################################

library("dplyr")
library("geosphere")
library("ggmap")
library("ggplot2")
library("htmltools")
library("htmlwidgets")
library("httr")
library("leaflet")
library("lubridate")
library("maptools")
library("overpass")
library("purrr")
library("raster")
library("readr")
library("readxl")
library("rgdal")
library("rgeos")
library("sp")
library("tibble")
library("viridis")


############################################################
#                                                          #
#                  LOAD HELPER FUNCTIONS                   #
#                                                          #
############################################################


source("code/utils.R")
############################################################
#                                                          #
#                 Prepare mode share data                  #
#                                                          #
############################################################

source("code/data_preparation.R")

############################################################
#                                                          #
#                  Find cities boundaries                  #
#                                                          #
############################################################

source("code/add_boundaries.R")

############################################################
#                                                          #
#              Prepare Overpass query strings              #
#                                                          #
############################################################


source("code/make_query_strings.R")

############################################################
#                                                          #
#               Make queries for cycle paths               #
#                                                          #
############################################################

load("data/natalie_queries.RData")
source("code/utils.R")
load("data/cities.RData")
source("code/make_overpass_queries.R")


load("data/cities.RData")

############################################################
#                                                          #
#                 Make queries for streets                 #
#                                                          #
############################################################


load("data/natalie_streets_queries.RData")
source("code/make_overpass_streets_queries.R")

############################################################
#                                                          #
#                     Join all results                     #
#                                                          #
############################################################


source("code/bind_tables.R")

############################################################
#                                                          #
#                     Make flags plot                      #
#                                                          #
############################################################
source("code/natalie_growthmodel.R")
