# function for keeping sp things only inside boundaries (sp2)
filter_sp <- function(sp, sp2){
  if(!is.null(sp)){
    crs(sp) <- crs(sp2)
    output <- try(list(sp[sp2,]))
    if(class(output) == "try-error"){
      output <- NULL
    }
    return(output)
  }
  else{
    return(NULL)
  }
  
}

# only ways, not things with only one point
onlyways <- function(sp){
  length_ways <- 1:nrow(sp) %>%
    map(function(x){
      nrow(sp@lines[[x]]@Lines[[1]]@coords)
    }) %>%
    unlist
  
  sp_better <- sp[length_ways > 1,]
  sp_better
}

# function for getting information for one city
get_info_city <- function(city_sp, natalie_queries){
  print(extent(city_sp))
  bbox <- paste0("(", extent(city_sp)[3], ",", extent(city_sp)[1], ",",
                 extent(city_sp)[4], ",", extent(city_sp)[2], ")")
  
  dplyr::group_by_(natalie_queries, ~category) %>%
    summarize_(query = lazyeval::interp(~gsub(", ", 
                                              paste(bbox, '; \n '),
                                              toString(query_string)))) %>%
    mutate_(query = lazyeval::interp(~paste0("[out:xml][timeout:100];\n (\n ",
                                             query,
                                             bbox,";\n\n );\n out body;\n >;\n out skel qt;"))) %>% 
    dplyr::select(query) %>%
    as.list() %>%
    unlist() %>%
    purrr::map(make_query) %>% 
    purrr::map(onlyways) %>%
   purrr::map(filter_sp, sp2 = city_sp) %>%
    setNames(c("cycleway"))
}

# streets
get_streets_city <- function(city_sp, natalie_streets_queries){
  print(extent(city_sp))
  # print("Sleeping for 5 minutes")
  # Sys.sleep(300)
  bbox <- paste0("(", extent(city_sp)[3], ",", extent(city_sp)[1], ",",
                 extent(city_sp)[4], ",", extent(city_sp)[2], ")")
  
  dplyr::group_by_(natalie_streets_queries, ~category) %>%
    summarize_(query = lazyeval::interp(~gsub(", ", 
                                              paste(bbox, '; \n '),
                                              toString(query_string)))) %>%
    mutate_(query = lazyeval::interp(~paste0("[out:xml][timeout:100];\n (\n ",
                                             query,
                                             bbox,";\n\n );\n out body;\n >;\n out skel qt;"))) %>% 
    dplyr::select(query) %>%
    as.list() %>%
    unlist() %>%
    purrr::map(make_query)  %>% 
    purrr::map(onlyways) %>% 
    purrr::map(filter_sp, sp2 = city_sp) %>%
    setNames("streets")
}

# function for querying Overpass, small break before
make_query <- function(query){

output <- try(overpass::overpass_query(query))
try <- 1
while(class(output) == "try-error" && try < 5){
  print(paste("try", try, "out of 15"))
  
  Sys.sleep(4^try)
  output <- try(overpass::overpass_query(query, wait = TRUE))
  try <- try + 1
}
return(output)

}



#d istances and sums
function_street <- function(df){
  df %>%
    by_row(function(x){
      fortify(x$streets[[1]]) %>%
        slice_rows("group") %>%
        by_slice(distance_function,
                 .collate = "rows",
                 .to = "distance")
    }, .to = "streets_d") %>%
    group_by(city) %>%
    mutate_(streets = lazyeval::interp(~sum(streets_d[[1]]$"distance")/1000))
}

function_cycleway <- function(df){
  df %>%
    by_row(function(x){
      fortify(x$cycleway[[1]]) %>%
        slice_rows("group") %>%
        by_slice(distance_function,
                 .collate = "rows",
                 .to = "distance")
      }, .to = "cycleway_d") %>%
    group_by_("city") %>%
    mutate_(cycleway = lazyeval::interp(~sum(cycleway_d[[1]]$"distance")/1000))
}


distance_function <- function(df){
  filtered <- dplyr::select(df, long, lat)
  filtered1 <- filtered[1:(nrow(filtered) - 1),]
  filtered2 <- filtered[2:nrow(filtered),]
  sum(distGeo(filtered1, filtered2))
}

# figures
make_maps <- function(sp){
  map <- get_map(location = c(extent(sp)[1],
                              extent(sp)[3],
                              extent(sp)[2],
                              extent(sp)[4]),
                 zoom = 11)
  gg <- ggmap(map)
  if(class(sp) == "SpatialLinesDataFrame"){
    df <- fortify(sp)
    gg <- gg + geom_path(data=df, 
                         aes(x=long, y=lat, group=group),
                         color="red", size=0.25)
  }else{
    df <- as.data.frame(coordinates(sp))
    gg <- gg + geom_point(data=df, 
                          aes(x=lon, y=lat),
                          color="red", size=0.25)
  }
  gg <- gg + coord_quickmap()
  gg <- gg + ggthemes::theme_map()
  gg
  
  
}

# plot all
plot_all <- function(df, name){
  sp <- df$sp[1][[1]]
  for (i in 2:nrow(df)){
    if(df$city[i] == "London"){
      df_faux <- df$sp[i][[1]][, c("OBJECTID", "ID_0", "ISO", "NAME_0")]
      names(df_faux) <- names(sp)
      sp <- rbind(sp, df_faux, makeUniqueIDs = TRUE)
    }else{
      sp <- rbind(sp, df$sp[i][[1]], makeUniqueIDs = TRUE)
    }
    
  }
  
  m <- leaflet(sp) %>% 
    addTiles() %>% 
    addPolygons()
  
  saveWidget(m, file = name)
}