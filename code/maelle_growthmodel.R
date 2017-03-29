library("dplyr")
library("tidyr")
library("readr")
library("ggplot2")
library("viridis")
library("ggrepel")
library("ggflags")
library("devtools")
library("ggflags")
library("purrr")
library("nlstools")
############################################################
#                                                          #
#                       prepare data                       #
#                                                          #
############################################################

Bike <- read.csv(file="data/finaldata.csv", header=TRUE, sep=",")
flags <- read_csv2("data/Ggflag_countries.csv")
Bike <- mutate(Bike, city = as.character(city))
Bike <- mutate(Bike, city = iconv(city, "UTF-8", "latin1"))
Bike <- mutate (Bike, norm_cycle =(cycleway*100000)/Population)
Bike <- mutate(Bike, pasta_city=city %in% c("Antwerp", "Barcelona", "London", 
                                            "Rome", "Wien") |
                 grepl("Z.rich", city) | grepl(".rebro", city))

############################################################
#                                                          #
#                          model                           #
#                                                          #
############################################################

model_growth <- function(Bikedata){
  m <- nls(Bike ~ L*exp(-b*(exp(-c*norm_cycle))), 
           data = Bikedata, 
           start = list(L = 20, 
                        b = 0.6,
                        c = 0.4))
  return(m)
}

set.seed(1)


model <- model_growth(Bikedata = Bike)
Bikedata <- Bike
boo <- nlsBoot(model, niter = 1000)

pred_modeshare <- function(L, b, c){
  L*exp(-b*(exp(-c*Bike$norm_cycle)))
}

L <- boo[[1]][1:1000]
b <- boo[[1]][1001:2000]
c <- boo[[1]][2001:3000]
pred <- pmap(list(L = L, b = b, c = c), 
            pred_modeshare)
boots <- data.frame(pred = unlist(pred))
boots <- mutate(boots, distance = rep(Bike$norm_cycle,
                                      1000))
boots <- group_by(boots, by = distance)
boots <- summarize(boots,
                   lower = quantile(pred, probs = 0.025),
                   upper = quantile(pred, probs = 0.975))
############################################################
#                                                          #
#                     Preds and plots                      #
#                                                          #
############################################################

Bike <- dplyr::select(Bike, -country)
Bike <- left_join(Bike, flags, by = "Country")
Bike <- dplyr::rename(Bike, country = Ggflag)

mpred <-predict(model)
Bike <- mutate(Bike,pred_share=mpred)
Bike <- arrange(Bike, norm_cycle)


arrange(Bike, norm_cycle) %>%
ggplot() + 
  geom_flag(aes(norm_cycle, Bike, country=country)) + 
  scale_country()+
  geom_ribbon(data = boots,
              aes(by, ymin = lower, 
                  ymax = upper),
              alpha = 0.2)+
  geom_line(aes(norm_cycle, pred_share), color="red")+
  geom_label_repel(data = dplyr::filter(Bike, pasta_city), 
                   aes(norm_cycle, Bike, label=city), size=4)+
    xlab("cycling km/100,000 persons")+
  ylab("cycling mode share (%)") 


ggsave(file = "figures/kitschflags.png",
       width = 8, height = 6)

