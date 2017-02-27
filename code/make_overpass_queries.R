whatweget <- as.list(cities$sp) %>%
  purrr::map(get_info_city, natalie_queries)

save(whatweget, file = "data/whatweget_prov.RData")


whatweget <- dplyr::bind_rows(whatweget)
whatweget <- mutate_(whatweget, city = lazyeval::interp(~cities$city)) 

save(whatweget, file = "data/whatweget.RData")

whatweget %>%
  function_cycleway() %>%
  dplyr::select(city, cycleway) %>%
  write_csv(path = "tadaaa.csv")

