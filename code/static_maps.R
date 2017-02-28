
load("data/whatweget.RData")

load("data/cities.RData")

############################################################
#                                                          #
#                        Barcelona                         #
#                                                          #
############################################################

sweden <- dplyr::filter(cities, city == "Barcelona")
oerebro <- dplyr::filter(whatweget, city == "Barcelona")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")



p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                           min(watershedDF$lat) - 0.01,
                           max(watershedDF$long) + 0.01,
                           max(watershedDF$lat) + 0.01),
              maptype = "terrain",
              source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/barcelona.png", width = 6, height = 6)

############################################################
#                                                          #
#                         ZÃ¼rich                          #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "Zürich")
oerebro <- dplyr::filter(whatweget, city == "Zürich")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")



p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/zuerich.png", width = 6, height = 6)

############################################################
#                                                          #
#                         Ã–rebro                          #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "Antwerp")
oerebro <- dplyr::filter(whatweget, city == "Antwerp")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")



p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/antwerp.png", width = 6, height = 6)

############################################################
#                                                          #
#                         Antwerp                          #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "Örebro")
oerebro <- dplyr::filter(whatweget, city == "Örebro")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")


p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/oerebro.png", width = 6, height = 6)

############################################################
#                                                          #
#                          London                          #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "London")
oerebro <- dplyr::filter(whatweget, city == "London")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")

p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/london.png", width = 6, height = 6)


############################################################
#                                                          #
#                           Roma                           #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "Rome")
oerebro <- dplyr::filter(whatweget, city == "Rome")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")

p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)

ggsave(file = "maps/rom.png", width = 6, height = 6)

############################################################
#                                                          #
#                          Vienna                          #
#                                                          #
############################################################
sweden <- dplyr::filter(cities, city == "Wien")
oerebro <- dplyr::filter(whatweget, city == "Wien")
shp <- oerebro$cycleway[[1]]
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)
data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

shp_boundaries <- sweden$sp[[1]]
shp_boundaries@data$id <- rownames(shp_boundaries@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(shp_boundaries, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, shp_boundaries@data, by = "id")

p <- ggmap(get_map(location = c(min(watershedDF$long) - 0.01,
                                min(watershedDF$lat) - 0.01,
                                max(watershedDF$long) + 0.01,
                                max(watershedDF$lat) + 0.01),
                   maptype = "terrain",
                   source = "stamen"))
p +
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group),
            col = "red")+
  geom_path(data=watershedDF,size=1,
            aes(x=long,y=lat,group=group),
            col = "purple")+
  labs(x="",y="")+
  coord_cartesian() +
  theme_void() +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  scalebar(watershedDF, dist = 5, dd2km = TRUE, model = 'WGS84',
           location = "topleft", st.size = 3)+
  north(data_merged, scale = 0.2)
ggsave(file = "maps/wien.png", width = 6, height = 6)
