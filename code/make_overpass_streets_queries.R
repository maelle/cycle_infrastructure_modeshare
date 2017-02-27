df <- NULL

cities_pasta <- filter(cities, 
                       city %in% c("Rome", "Barcelona",
                                   "London", "Antwerp",
                                   "Wien") |
                         grepl(".rebro", city) |
                         grepl("Z.rich", city))

  whatweget_streets <- as.list(cities_pasta$sp) %>%
    purrr::map(get_streets_city, natalie_streets_queries)
  
  save(whatweget_streets, file = "data/whatweget_streets_prov.RData")
  
  whatweget_streets <- dplyr::bind_rows(whatweget_streets)
   
  
  save(whatweget_streets, file = "data/whatweget_streets_", country, ".RData")
  
  whatweget_streets$city <- cities_pasta$city
  df <- whatweget_streets %>%
    function_street()

  

df %>%
  ungroup() %>%
  dplyr::select(streets, city) %>%
  write_csv(path = "tadaaa_streets.csv")