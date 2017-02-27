library("dplyr")
library("ggplot2")
library("viridis")
library("ggrepel")
library("ggflags")
library("devtools")
library("ggflags")

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

m <- nls(Bike ~ L*exp(-b*(exp(-c*norm_cycle))), data = Bike, start = list(L = 20, b = 0.6, c = 0.4))
plot (Bike$norm_cycle, Bike$Bike, main= "Association between cycling km/100,000 persons and cycling mode share", xlab="cycling km/100,000 persons", ylab="cycling mode share (%)")
l <- predict(m)
points(Bike$norm_cycle,l, col ="red")

L <- coefficients(m)[1]
b <- coefficients(m)[2]
c <- coefficients(m)[3]
norm_cycle <- 200

#maximal cycling mode share
x <- (-log((log(0.999))/-b))/c
x
norm_cycle <-x
L*exp(-b*(exp(-c*norm_cycle)))
L


# Model fit
# Residual sum of squares
(model_fit <- sum(residuals(m)^2))
# Total sum of squares
(TSS <- sum((Bike$Bike - mean(Bike$Bike))^2)) 
# R-squared measure
 

# Scenario 1
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona, London
norm_cycle <- 43.80
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 104.57
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 31.72
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 4.62
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 286.06
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 10.83
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 12.29
L*exp(-b*(exp(-c*norm_cycle)))

# Scenario 2
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona, London
norm_cycle <- 51.76 
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 123.59
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 37.49
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 5.47
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 338.07
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 12.80
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 14.53
L*exp(-b*(exp(-c*norm_cycle)))

# Scenario 3
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona, London
norm_cycle <- 59.72 
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 142.60
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 43.26
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 6.31
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 390.08
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 14.76
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 16.76
L*exp(-b*(exp(-c*norm_cycle)))

# Scenario 4
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona London
norm_cycle <- 79.63 
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 190.13
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 57.68
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 8.41
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 520.11
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 19.68
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 22.35
L*exp(-b*(exp(-c*norm_cycle)))

# Scenario 5
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona London
norm_cycle <- 260.05 
L*exp(-b*(exp(-c*norm_cycle)))

# Scenario 6
#Vienna, Antwerp, Zurich, Rome, Örebro, Barcelona London
norm_cycle <- 219.73 
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 335.25
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 289.99
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 287.55
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 2168.28
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 95.67
L*exp(-b*(exp(-c*norm_cycle)))
norm_cycle <- 191.35
L*exp(-b*(exp(-c*norm_cycle)))


############################################################
#                                                          #
#                     Preds and plots                      #
#                                                          #
############################################################

Bike <- dplyr::select(Bike, -country)
Bike <- left_join(Bike, flags, by = "Country")
Bike <- dplyr::rename(Bike, country = Ggflag)

mpred <-predict(m)
Bike <- mutate(Bike,pred_share=mpred)
Bike <- arrange(Bike, norm_cycle)


ggplot(Bike)+
  geom_point(aes(norm_cycle, Bike, label=city, col = Country, shape=pasta_city), size=3)+
  geom_label_repel(data = dplyr::filter(Bike, pasta_city), aes(norm_cycle, Bike, label=city), size=4)+
  geom_line(aes(norm_cycle, pred_share), color="red")+
  ggtitle("Association between cycling network distance and cycling mode share")+
  xlab("cycling km/100,000 persons")+
  ylab("cycling mode share (%)")+
  scale_color_viridis(discrete = TRUE)

ggplot(Bike)+
  geom_point(aes(norm_cycle, Bike, label=city, col = Country, shape=pasta_city), size=3)+
  geom_label_repel(data = dplyr::filter(Bike, pasta_city), aes(norm_cycle, Bike, label=city), size=4)+
  geom_line(aes(norm_cycle, pred_share), color="red")+
  ggtitle("Association between cycling network distance and cycling mode share")+
  xlab("cycling km/100,000 persons")+
  ylab("cycling mode share (%)")+
  scale_color_viridis(discrete = TRUE)




arrange(Bike, norm_cycle) %>%
ggplot() + 
  geom_flag(aes(norm_cycle, Bike, country=country)) + 
  scale_country()+
  geom_line(aes(norm_cycle, pred_share), color="red")+
  geom_label_repel(data = dplyr::filter(Bike, pasta_city), 
                   aes(norm_cycle, Bike, label=city), size=4)+
    xlab("cycling km/100,000 persons")+
  ylab("cycling mode share (%)")


ggsave(file = "figures/kitschflags.png")

