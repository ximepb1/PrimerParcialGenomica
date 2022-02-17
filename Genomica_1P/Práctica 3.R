####### PRACTICA 3 ###########
library(igraph)
a <- barabasi.game(100)
plot(a, vertex.label =NA)
### degree
d <- degree(a, mode =  "in")
hist(d)
# mayor número de nodos tienen menos conexiones

fit1 <- fit_power_law(d+1, 10)
fit2 <- fit_power_law(d+1, 10, implementation="R.mle")

help("fit_power_law")
## ajusta los valores a una ley de potencia
### dan la probabilidan de que cumplan la ley de potencia
#valor alfa 
fit1$alpha

stats4::coef(fit2)

fit1$logLik

stats4::logLik(fit2)


##### Cargar redes #######
g10<-barabasi.game(10,directed = FALSE)
g100<-barabasi.game(100,directed = FALSE)
g1K<-barabasi.game(1000,directed = FALSE)
g2<-random.graph.game(1000,0.20)
g3<-sample_smallworld(1,1000,p=0.2,nei=3)
g4<-make_graph("Zachary")

### trazar la distribución
d1 <- degree(g10, mode =  "in")
hist(d1)
d2 <- degree(g100, mode =  "in")
hist(d2)
d3 <- degree(g1K, mode =  "in")
hist(d3)
d4 <- degree(g2, mode =  "in")
hist(d4)
d5 <- degree(g3, mode =  "in")
hist(d5)
d6 <- degree(g4, mode =  "in")
hist(d6)

#####media
mean(d1)
mean(d2)
mean(d3)
mean(d4)
mean(d5)
mean(d6)

##### mediana
median(d1)
median(d2)
median(d3)
median(d4)
median(d5)
median(d6)

#####
boxplot(d1)
boxplot(d2)
boxplot(d3)
boxplot(d4)
boxplot(d5)
boxplot(d6)


#####
fitg1 <- fit_power_law(d1+1, 10)
fitg1$xmin
fitg2 <- fit_power_law(d2+1, 10)
fitg2$continuous


#####
karate <- make_graph("Zachary")
install.packages("igraphdata")

library(igraphdata)
data("yeast")

yeast
### UN = undirectional no dirigida
dy <- degree (yeast)
plot (yeast)


head(sort(dy,decreasing = TRUE),10)
sort(dy,decreasing = TRUE)[1:10]

hist(dy,100)
fit3<-fit_power_law(d1+1,10)

degree_distribution(yeast)->d4

plot(d4, log = "xy")### pone ambos ejes en escala logaritmica