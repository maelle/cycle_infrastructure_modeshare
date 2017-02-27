tada <- read_csv("tadaaa.csv")
modeshare <- read_csv("data/merged_share.csv")
tada_streets <- read_csv("tadaaa_streets.csv")

allresults <- left_join(tada, modeshare,
                  by = c("city" = "City"))
# allresults <- left_join(tada, tada_streets,
#                   by = c("city" = "city"))

countries <- readxl::read_excel("data/Ggflag_countries.xlsx")

allresults <- left_join(allresults, countries, by = "Country")
allresults <- select(allresults, - Country)
allresults <- rename(allresults, country = Ggflag)

write_csv(allresults, path = "data/finaldata.csv")