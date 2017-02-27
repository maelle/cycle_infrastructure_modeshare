############################################################
#                                                          #
#                  data about mode share                   #
#                                                          #
############################################################


tems <- readr::read_csv("data/merged_share.csv")
############################################################
#                                                          #
#                          Spain                           #
#                                                          #
############################################################

# get boundaries for cities in Spain
adm <- getData("GADM", country = "ESP", level = 4)

# cities for which we have mode share data
cities_spain <- unique(tems$City[tems$Country == "Spain"])
cities_spain <- cities_spain[cities_spain %in% adm$NAME_4]

# data frame for storing data
data_spain <- tibble::tibble(city = cities_spain)

# not Murcia
data_spain <- filter(data_spain, city != "Murcia")

# add boundaries
data_spain <- data_spain %>%
  by_row(function(df){
    output <- adm[adm$NAME_4 == df$city,]
    if(df$city == "Valencia"){
      output <- adm[adm$NAME_3 == "Ciutat de Valencia",]
    }
    if(df$city == "Sevilla"){
      output <- adm[adm$ID_4 == 731,]
    }
    if(df$city == "Málaga"){
      output <- adm[adm$ID_4 == 667,]
    }
    if(nrow(output) != 1){print(df$city)}
    output
  }, .to = "sp")

file.remove("GADM_2.8_ESP_adm4.rds")
############################################################
#                                                          #
#                            UK                            #
#                                                          #
############################################################
# cities for which we have mode share data & easy to find
# No Newcastle bc I didn't know which one!
cities_uk <- c("Leeds", "Sheffield", "Portsmouth",
               "Bristol", "Nottingham", "Leicester",
               "Plymouth", "Stoke-on-Trent", "Southampton",
               "York", "Milton Keynes", "Swindon",
               "Southend-on-Sea", "Peterborough", "Thurrock",
               "Oxford", "Blackburn with Darwen", "Blackpool",
               "Norwich", "Ipswich", "Watford", "Exeter", "Preston", "London")

dsn <- paste0(getwd(), "/england_lad_2011/england_lad_2011")
adm <- raster::shapefile(dsn)
#adm <- adm[!(adm$KKOD == 303 & adm$NAMN1 == "Lund" & adm$ADAT == "2012-01-24 18:35"),]
# data frame for storing data
data_uk <- tibble::tibble(city = cities_uk)

london_boroughs <-  c("Barking and Dagenham",
                           "Barnet",
                           "Bexley",
                           "Brent",
                           "Bromley",
                           "Camden",
                           "London",
                           "Croydon",
                           "Ealing",
                           "Enfield",
                           "Greenwich",
                           "Hackney",
                           "Hammersmith and Fulham",
                           "Haringey",
                           "Harrow",
                           "Havering",
                           "Hillingdon",
                           "Hounslow",
                           "Islington",
                           "Kensington and Chelsea",
                           "Kingston upon Thames",
                           "Lambeth",
                           "Lewisham",
                           "Merton",
                           "Newham",
                           "Redbridge",
                           "Richmond upon Thames",
                           "Southwark",
                           "Sutton",
                           "Tower Hamlets",
                           "Waltham Forest",
                           "Wandsworth",
                           "Westminster")

# add boundaries
data_uk <- data_uk %>%
  by_row(function(df){
    if(df$city != "London"){
      if(df$city == "Bristol"){
        sp <- adm[adm$name == "Bristol, City of",]
      }else{
        sp <- adm[adm$name == df$city ,]
      }
      
      if(nrow(sp) != 1){print(df$city)}
      CRS.new <- CRS(proj4string(data_spain$sp[[1]])) 
      spTransform(sp, CRS.new)
    }
    }, .to = "sp")
    
############################################################

level <- 2
country <- "GBR"

adm <- getData("GADM", country = country, level = level)
data_uk[data_uk$city == "London",]$sp <- list(adm[adm$NAME_2 %in% london_boroughs,])
file.remove(paste0("GADM_2.8_", country,"_adm", level, ".rds"))


############################################################
#                                                          #
#                         Belgium                          #
#                                                          #
############################################################
# get boundaries for cities in the country
adm <- getData("GADM", country = "BEL", level = 4)

# cities for which we have mode share data
cities_belgium <- unique(tems$City[tems$Country == "Belgium"])
cities_belgium_easier <- cities_belgium
cities_belgium_easier <- gsub("Liege", "Liège", cities_belgium_easier)
cities_belgium_easier <- gsub("Brussels", "Brussel", cities_belgium_easier)
cities_belgium_easier <- gsub("Ghent", "Gent", cities_belgium_easier)
cities_belgium_easier <- gsub("Antwerp", "Antwerpen", cities_belgium_easier)
cities_belgium <- cities_belgium[cities_belgium_easier %in% adm$NAME_4]
cities_belgium_easier <- cities_belgium_easier[cities_belgium_easier %in% adm$NAME_4]

# data frame for storing data
data_belgium <- tibble::tibble(city = cities_belgium,
                               city_easier = cities_belgium_easier)

# add boundaries
data_belgium <- data_belgium %>%
  by_row(function(df){
    output <- adm[adm$NAME_4 == df$city_easier,]
    if(nrow(output) != 1){print(df$city_easier)}
    output
  }, .to = "sp")

file.remove("GADM_2.8_BEL_adm4.rds")
############################################################
#                                                          #
#                          Sweden                          #
#                                                          #
############################################################

# cities for which we have mode share data
cities_sweden <- unique(tems$City[tems$Country == "Sweden"])
cities_sweden_easier <- cities_sweden
cities_sweden_easier<- gsub("Gothenburg", "Göteborg", cities_sweden_easier)
cities_sweden_easier <- gsub("ö", ".", cities_sweden_easier)
cities_sweden_easier <- gsub("Ö", ".", cities_sweden_easier)
cities_sweden_easier <- gsub("ä", ".", cities_sweden_easier)
cities_sweden_easier <- gsub("å", ".", cities_sweden_easier)

dsn <- paste0(getwd(), "/Sweden_shapefile/ak_riks")
adm <- raster::shapefile(dsn)
#adm <- adm[!(adm$KKOD == 303 & adm$NAMN1 == "Lund" & adm$ADAT == "2012-01-24 18:35"),]
# data frame for storing data
data_sweden <- tibble::tibble(city = cities_sweden,
                               city_easier = cities_sweden_easier)

# add boundaries
data_sweden <- data_sweden %>%
  by_row(function(df){
    sp <- adm[grepl(df$city_easier, adm$KOMMUNNAMN),]
    CRS.new <- CRS(proj4string(data_belgium$sp[[1]])) 
    spTransform(sp, CRS.new)
  }, .to = "sp")



############################################################
#                                                          #
#                       Austria                            #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "AUT", level = 3)

# cities for which we have mode share data
cities_austria <- unique(tems$City[tems$Country == "Austria"])
cities_austria <- cities_austria[(cities_austria %in% adm$NAME_3)|(cities_austria %in% adm$NAME_2)]

# data frame for storing data
data_austria <- tibble::tibble(city = cities_austria)

# add boundaries
data_austria <- data_austria %>%
  by_row(function(df){
    if(df$city %in%adm$NAME_3){
      output <- adm[adm$NAME_3 == df$city,]
    }else{
      output <- adm[adm$NAME_2 == df$city,]
    }
    output
    
  }, .to = "sp")

file.remove("GADM_2.8_AUT_adm3.rds")


############################################################
#                                                          #
#                         Switzerland                      #
#                                                          #
############################################################

# get boundaries for cities in Spain
adm <- getData("GADM", country = "CHE", level = 3)

# cities for which we have mode share data
cities_suisse <- unique(tems$City[tems$Country == "Switzerland"])
cities_suisse <- cities_suisse[(cities_suisse %in% adm$NAME_3)|(cities_suisse %in% adm$NAME_2)]

# data frame for storing data
data_suisse <- tibble::tibble(city = cities_suisse)

# add boundaries
data_suisse <- data_suisse %>%
  by_row(function(df){
    if(df$city %in%adm$NAME_3){
      output <- adm[adm$NAME_3 == df$city,]
    }else{
      output <- adm[adm$NAME_2 == df$city,]
    }
    
    if(nrow(output) != 1){print(df$city)}
    
    output
  }, .to = "sp")

file.remove("GADM_2.8_CHE_adm3.rds")

############################################################
#                                                          #
#                          Italia                          #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "ITA", level = 3)

# cities for which we have mode share data
cities_italy <- unique(tems$City[tems$Country == "Italy"])

# data frame for storing data
cities_italy <- tibble::tibble(city = cities_italy,
                               city_easier = gsub("Rome", "Roma", cities_italy))

# add boundaries
data_italy <- cities_italy %>%
  by_row(function(df){
    if(df$city_easier == "Padua"){
      output <- adm[adm$NAME_3 == "Padova",]
    }
    else{
      if(df$city_easier == "Reggio Emilia"){
        output <- adm[adm$NAME_3 == "Reggio Nell' Emilia",]
      }else{
        if(df$city_easier %in%adm$NAME_3){
          output <- adm[adm$NAME_3 == df$city_easier,]
        }else{
          output <- adm[adm$NAME_2 == df$city_easier,]
        }
      }
      
      
      
      if(nrow(output) != 1){print(df$city)}
      output
    }
    
    
  }, .to = "sp")

file.remove(paste0("GADM_2.8_", "ITA","_adm", 3, ".rds"))

############################################################
#                                                          #
#                         Germany                          #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "DEU", level = 4)

# cities for which we have mode share data
cities_germany <- unique(tems$City[tems$Country == "Germany"])

# some cities cannot be included
cities_germany <- cities_germany[!cities_germany %in% c("Ulm/Neu-Ulm", "Fürth")]
cities_germany_easier <- cities_germany

# correct some names
cities_germany_easier <- gsub("Moenchengladbach", "Mönchengladbach", cities_germany_easier)
cities_germany_easier[cities_germany_easier == "Halle"] <- "Halle (Westf.)"
cities_germany_easier <- gsub("Oldenburg", "Oldenburg (Oldb)", cities_germany_easier)
cities_germany_easier <- gsub("Ludwigshafen", "Ludwigshafen am Rhein", cities_germany_easier)
cities_germany_easier <- gsub("Freiburg", "Freiburg im Breisgau", cities_germany_easier)
cities_germany_easier <- gsub("Frankfurt", "Frankfurt am Main", cities_germany_easier)



# data frame for storing data
data_germany <- tibble::tibble(city = cities_germany,
                               city_easier = cities_germany_easier)

# add boundaries
data_germany <- data_germany %>%
  by_row(function(df){
    
    if(df$city_easier == "Hamm"){
      output <- adm[adm$NAME_2 == "Hamm",]
    }else{
      
      if(df$city_easier == "Münster"){
        output <- adm[adm$NAME_2 == "Münster",]
      }else{
        
        if(df$city_easier == "Oberhausen"){
          output <- adm[adm$NAME_2 == "Oberhausen",]
        }else{
            output <- adm[adm$NAME_4 == df$city_easier,]
          
        }
        
        
      }
    }
    if(nrow(output) != 1){
      print(df$city_easier)
    }
    output
  }, .to = "sp")

file.remove(paste0("GADM_2.8_", "DEU","_adm", 4, ".rds"))

############################################################
#                                                          #
#                       Netherlands                        #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "NLD", level = 2)

# cities for which we have mode share data
cities_netherlands <- unique(tems$City[tems$Country == "Netherlands"])

# filter locations with boundaries not easy to find
ndl_exclude <- c("Region Nort-East Brabant", "Den Haag", "Parkstad Limburg", "Dordecht")
cities_netherlands <- cities_netherlands[! cities_netherlands %in% ndl_exclude]

# data frame for storing data
data_netherlands <- tibble::tibble(city = cities_netherlands)

# add boundaries
data_netherlands <- data_netherlands %>%
  by_row(function(df){
    output <- adm[adm$NAME_2 == df$city,]
    if(nrow(output) != 1){print(df$city)}
    output
  }, .to = "sp")

file.remove(paste0("GADM_2.8_", "NLD","_adm", 2, ".rds"))

############################################################
#                                                          #
#                         Denmark                          #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "DNK", level = 2)

# cities for which we have mode share data
cities_denmark <- unique(tems$City[tems$Country == "Denmark"])

# data frame for storing data
data_denmark <- tibble::tibble(city = cities_denmark)

# add boundaries
data_denmark <- data_denmark %>%
  by_row(function(df){
    output <- adm[adm$NAME_2 == df$city,]
    if(df$city == "Copenhagen"){
      output <- adm[adm$NAME_2 == "København",]
    }
    output
  }, .to = "sp")

file.remove(paste0("GADM_2.8_", "DNK","_adm", 2, ".rds"))




############################################################
#                                                          #
#                         France                          #
#                                                          #
############################################################
# get boundaries for cities in Spain
adm <- getData("GADM", country = "FRA", level = 4)

# cities for which we have mode share data
cities_france <- unique(tems$City[tems$Country == "France"])

# Not outside of Europe
cities_france <- cities_france[cities_france != "Pointe-à-Pitre"]

# cities only

cities_france <- cities_france[!cities_france %in% c("Grenoble Agglo", "Metropole Savoie", "Bas-Rhin",
                                                     "Bouches-du-Rhône", "Beaujolais", "Genevois Fancais", 
                                                     "Vienne")]

# if it's hard to find

cities_france <- cities_france[!cities_france %in% c("Saint Quentin en Yvelines")]

# data frame for storing data
data_france <- tibble::tibble(city = cities_france)

# add boundaries
data_france <- data_france %>%
  by_row(function(df){
    if(df$city %in% c("Lyon", "Marseille")){
      output <- adm[adm$NAME_3 == df$city,]
    }else{ 
      if(df$city == "Paris"){
        output <- adm[adm$NAME_2 == df$city,]
      }else{
        if(df$city == "Aix"){
          output <- adm[adm$NAME_4 == "Aix-en-Provence",]
        }else{
          output <- adm[adm$NAME_4 == df$city,]
        }
      }}
    
    
    output
  }, .to = "sp")

file.remove(paste0("GADM_2.8_", "FRA","_adm", 4, ".rds"))

############################################################
#                                                          #
#                   everything together                    #
#                                                          #
############################################################

cities <- bind_rows(mutate(data_uk, country = "UK"),
                    mutate(data_germany, country = "Germany"),
                    mutate(data_spain, country = "Spain"),
                    mutate(data_belgium, country = "Belgium"),
                    mutate(data_sweden, country = "Sweden"),
                    mutate(data_italy, country = "Italy"),
                    mutate(data_suisse, country = "Switzerland"),
                    mutate(data_austria, country = "Austria"),
                    mutate(data_netherlands, country = "Netherlands"),
                    mutate(data_denmark, country = "Denmark"),
                    mutate(data_france, country = "France"))

nrow_sp <- cities %>% by_row(function(x){nrow(x$sp[[1]])}) %>%
  tidyr::unnest(.out) %>%
  dplyr::select(.out) %>%
  .$.out

cities <- cities[nrow_sp > 0,]

save(cities, file = "data/cities.RData")

############################################################
#                                                          #
#                           maps                           #
#                                                          #
############################################################
plot_all(data_spain, name = paste0(getwd(), "/maps_for_checks/spain.html"))
plot_all(data_belgium, name = paste0(getwd(), "/maps_for_checks/belgium.html"))
plot_all(data_sweden, name = paste0(getwd(), "/maps_for_checks/sweden.html"))
plot_all(data_uk, name = paste0(getwd(), "/maps_for_checks/uk.html"))
plot_all(data_suisse, name = paste0(getwd(), "/maps_for_checks/suisse.html"))
plot_all(data_austria, name = paste0(getwd(), "/maps_for_checks/austria.html"))
plot_all(data_italy, name = paste0(getwd(), "/maps_for_checks/italy.html"))
plot_all(data_germany, name = paste0(getwd(), "/maps_for_checks/germany.html"))
plot_all(data_netherlands, name = paste0(getwd(), "/maps_for_checks/netherlands.html"))
plot_all(data_denmark, name = paste0(getwd(), "/maps_for_checks/denmark.html"))
plot_all(data_france, name = paste0(getwd(), "/maps_for_checks/france.html"))

