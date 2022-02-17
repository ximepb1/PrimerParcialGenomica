library(igraph)
###### EJERCICIO UNO #####
##### ARMAR RED DIAPO #####
r2 <- make_empty_graph(n=7, directed = FALSE)
V(r2)$color = "purple"
V(r2)$shape = "sphere" #forma de los nodos
r2 <- add.edges(r2, c(1,2, 2,4, 2,5, 3,2, 4,5, 4,6, 4,7, 5,7))
plot(r2)
E(r2)$color = "green"
plot(r2)
#ruta más corta
shortest.paths(r2)
#ditancia
d1 <- distances(r2)
d1
distances(r2, mode= "out")
distances(r2, mode= "in")
#grafica las distancia
heatmap(d1)
#promedio de distancias
mean_distance(r2)
#diametro
diameter (r2)
#coeficiente de clousterización
t1 <- transitivity(r2)
transitivity(r2, vids="1", type="local")


######## EJERCICIO DOS #######
#### RED 1 ####
g1<-barabasi.game(100,directed = FALSE)
plot(g1)
#ruta más corta
shortest.paths(g1)
#ditancia
dg1 <- distances(g1)
distances(g1, mode= "out")
distances(g1, mode= "in")
#grafica las distancia
heatmap(dg1)
#promedio de distancias
mean_distance(g1)
#diametro
diameter (g1)
#coeficiente de clousterización
transitivity(g1)

##### RED 2 ####
g2<-random.graph.game(100,0.20)
plot(g2)
#ruta más corta
shortest.paths(g2)
#ditancia
dg2 <- distances(g2)
distances(g2, mode= "out")
distances(g2, mode= "in")
#grafica las distancia
heatmap(dg2)
#promedio de distancias
mean_distance(g2)
#diametro
diameter (g2)
#coeficiente de clousterización
transitivity(g2)

##### RED 3 #####
g3<-sample_smallworld(1,100,p=0.2,nei=3)
plot(g3)
#ruta más corta
shortest.paths(g3)
#ditancia
dg3 <- distances(g3)
distances(g3, mode= "out")
distances(g3, mode= "in")
#grafica las distancia
heatmap(dg3)
#promedio de distancias
mean_distance(g3)
#diametro
diameter (g3)
#coeficiente de clousterización
transitivity(g3)


g <- barabasi.game(100, power=1)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)
plot(g)
