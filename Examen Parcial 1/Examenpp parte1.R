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
V(redamiguis)$color = "purple"
V(redamiguis)$shape = "sphere" #forma de los nodos
plot(redamiguis) #imprimos el grafico
# 2. Determine a las tres personas con más amigues
dos <- sort(degree(redamiguis, mode = "in"), decreasing = TRUE)[1:3] #seleccionar 3 nodos mas importantes
dos
# 3. Determine a las tres personas que consideran que tiene más amigues
tres <- sort(degree(redamiguis, mode = "out"), decreasing = TRUE)[1:3] #seleccionar 3 nodos mas importantes
tres
# 4. Las tres personas más importantes por tres medidas de centralidad
cuatro <- centr_degree(redamiguis)
cuatro
# 5. Clusteriza la red con al menos dos métodos y determine cuáles son los clústers.
eb <- cluster_edge_betweenness(redamiguis, directed = TRUE) #cluster método 1
  table(membership(eb)) #separamos por grupos
plot(eb, redamiguis) #imprimos vector con el plot

wc <- cluster_walktrap(redamiguis)
members <- membership(wc)
plot(wc,redamiguis)

#En general separa dos grupos
#PALOMA, ADRIAN, FERNANDA, MAYELA, ISABEL, ERICK Y XIMENA
#REBECA, CAROLINA, MARIA FERNANDA, MARIA, ANDREA, ERNESTO Y MITZI
#con el otro cluster se realizan tres grupos aunque en general los dos más grandes contienen casi a las mismas personas

# 6. Calcule el diámetro
diameter(redamiguis)
#3

# 7. La matriz de distancias y dibuje un heatmap
dmatriz <- View(distances(redamiguis, mode = "in"))
heatmap(dmatriz)    
