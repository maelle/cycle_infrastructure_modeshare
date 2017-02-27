############################################################
#                                                          #
#                  Cycling infrastructure                  #
#                                                          #
############################################################


natalie_queries <- read_csv2("data/fietsen_natalie_tags.csv")
names(natalie_queries) <- c("fiets_category", "category", "query_string", "object_type", "wanted")
natalie_queries <- dplyr::filter(natalie_queries, wanted == 1)

# only cycleways
natalie_queries <- dplyr::filter(natalie_queries, category == "cycleway")

natalie_queries <- dplyr::select(natalie_queries, category, query_string, object_type)

# for queries with only ways we need to add a line with nodes
# because otherwise the overpass package will not be able to process them somehow
for (categori in c("cycleway")){
  subset <- dplyr::filter(natalie_queries, category == categori)
  newdata <- NULL
  for (j in 1:nrow(subset)){
    newdata <- rbind(newdata,
                     data.frame(category = categori,
                                query_string = gsub("way\\[", "node\\[", subset$query_string[j]),
                                object_type = "node"))
  }
  natalie_queries <- dplyr::bind_rows(natalie_queries, newdata)
}

natalie_queries <- dplyr::arrange(natalie_queries, category)
save(natalie_queries, file = "data/natalie_queries.RData")
rm(list = ls())

############################################################
#                                                          #
#                  Street infrastructure                   #
#                                                          #
############################################################
natalie_streets_queries <- read_csv2("data/roads_streets_labels.csv")
names(natalie_streets_queries) <- c("category", "label", "query_string", "object_type", "wanted")
natalie_streets_queries <- filter(natalie_streets_queries, wanted == 1)
natalie_streets_queries <- mutate(natalie_streets_queries,
                  query_string = paste0("way[\"highway\"=\"", label, "\"]"),
                  object_type = "way")

for (j in 1:nrow(natalie_streets_queries)){
  natalie_streets_queries <- bind_rows(natalie_streets_queries,
                   data.frame(category = "highway",
                              query_string = gsub("way\\[", "node\\[", natalie_streets_queries$query_string[j]),
                              object_type = "node"))
}

save(natalie_streets_queries, file = "data/natalie_streets_queries.RData")
rm(list = ls())
