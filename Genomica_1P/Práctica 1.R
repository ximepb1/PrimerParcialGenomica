install.packages("igraph")
install.packages("igraphdata")
library(igraph)

#Practica 1
r1 <- make_empty_graph(n=6, directed = TRUE)
#n= número de nodos
#dirigida = falso o verdadero
V(r1)$color = "purple" #color de los nodos
V(r1)$shape = "sphere" #forma de los nodos
#plot de la red sin conexiones
plot(r1)

#vamos a poner las conexiones
r1 <- add.edges(r1, c(1,2, 1,3, 2,4, 3,4, 4,5, 6,1))
plot(r1)

#Añadir nodos
r1 <- add.vertices(r1, 1, color="pink", shape="sphere")
plot(r1)

#añadir conexiones
r1 <- add.edges(r1, c(3,7, 7,2))
plot (r1)

#eliminar conexiones
r1 <- delete.edges(r1, c(1,3))
plot(r1)

#nombrar nodos
V(r1)$name <- LETTERS [1:7]
V(r1)

#nos imprima las conexiones entre nodos
E(r1)

