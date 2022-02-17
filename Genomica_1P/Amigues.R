  amigus <- read.csv("Red de amigues Genómica 2022 - Hoja 1.csv")
  amigus
row.names(amigus)<- amigus [,1] #poner los nombres de la primera columna como nombre de los renglones
amigus <- amigus [,-1] #eliminar la columna 1
amigus <- amigus [-2,-2] #eliminar columna y renglón dos de la persona que no contesto

amigus <- as.matrix(amigus)#convertir data frame en matriz

#cargar la matriz de adyacencia
library(igraph)

redamiguis <- graph_from_adjacency_matrix(amigus, mode ="directed")
redamiguis #nos da una descripción de la red

#cuantos considera amigos
ins <- degree(redamiguis, mode = "in")
sort(degree(redamiguis, mode = "in"), decreasing = TRUE)
#cuantos lo consideran amigo
out <- degree(redamiguis, mode = "out")
sort(degree(redamiguis, mode = "out"), decreasing = TRUE)

#promedio de conexiones por nodo
mean (degree(redamiguis, mode = "in"))
mean (degree(redamiguis, mode = "out"))

V(redamiguis)$size <- degree(redamiguis,mode = "in")*3
plot(redamiguis,edge.arrow.size=.4, edge.curved=.4)

posiciones <- which(amigus ["XIMENA",]==1)
posiciones
mean(posiciones)
posicionesi <- which(amigus [,"XIMENA"]==1)
posicionesi
mean(posicionesi)

wc <- cluster_walktrap(redamiguis)
members <- membership(wc)

plot(wc,redamiguis)

View( distances(redamiguis, mode = "in"))#nos da la matriz de distancias

