library(igraph)
r2 <- make_empty_graph(n=10, directed = FALSE)
V(r2)$color = "purple"
V(r2)$shape = "sphere" #forma de los nodos
r2 <- add.edges(r2, c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 6,8, 6,9, 6,10))
plot(r2)
closeness(r2)
centr_clo(r2)
centr_degree(r2)
centralization.closeness(r2)
closeness.estimate(r2, mode=c("all"))


library(igraphdata)
data("yeast")
closeness(yeast) #valores más altos
centr_clo(yeast)
centr_degree(yeast)
centralization.closeness(yeast)
closeness.estimate(yeats, mode=c("all"))

####Cluster

data("karate")
eb <- cluster_edge_betweenness(karate, directed = FALSE)
table(membership(eb))
plot(karate)
plot(eb, karate)#separa y colorea los clusters

fg <-cluster_fast_greedy(karate)
membership(fg)
table (membership(fg))
plot(fg, karate)

####comparar los dos clusters

compare(eb, fg, method = "adjusted.rand")

cluster_label_prop()
cluster_fast_greedy()

mat <- matrix(1:9, ncol=3)
for (i in 1:3) {
 for(j in 1:3){
   print(mat[i,j])
 } 
}


#Calcula, usando R, el logaritmo de 10,000,100,000 y 1,000,000.
#Calcula, también, el logaritmo del logaritmo de los números anteriores.
log10(10000)
log10(100000)
log10(1000000)
log10(log10(10000))
log10(log10(100000))
log10(log10(1000000))


#En igraph genera una red de 10,000 nodos y 100,000 nodos con los modelos
#free-scale, aleatoria y small-world. (¡¡¡¡NO,NO y NO las grafiques !!!!!)
r1f <- barabasi.game(10000, directed=FALSE)
r2f <- barabasi.game(100000, directed = FALSE)
r1a <- random.graph.game(10000, 0.4)
r2a <- random.graph.game(100000, 0.4)
r1s <- smallworld(10000)
r2s <- smallworld(1000000)

#Usa igraph para calcular el diámetro y distancias promedio 
#de cada una de las redes anteriores.
diameter(r1f)
diameter (r2f)
diameter(r1a)
diameter(r2a)
diameter(r1s)
diameter(r2s)
mean_distance(r1f)
mean_distance(r2f)
mean_distance(r1a)
mean_distance(r2a)
mean_distance(r1s)
mean_distance(r2s)

#Usa la diapositiva anterior para calcular, usando una función, 
#las dos propiedades anteriores para cada red sin usar el objeto de una red

