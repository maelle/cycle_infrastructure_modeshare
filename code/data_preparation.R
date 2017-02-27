# PASTA data 
# other data from http://www.epomm.eu/tems/result_cities.phtml?more=1

tems <- read_excel("data/tems_modalsplit.xlsx")
pasta <- read_excel("data/tems_modalsplit_PASTA.xlsx")
pasta$City <- gsub("Vienna", "Wien", pasta$City)
# not very beautiful code
tems <- filter(tems,
                !City %in% pasta$City)
tems <- bind_rows(tems, pasta)
tems <- mutate(tems, City = ifelse(City == "Bern", "Berne", City))

tems <- filter(tems, Population > 100000)
tems <- filter(tems, Year >= 2006)
readr::write_csv(tems, path = "data/merged_share.csv")
