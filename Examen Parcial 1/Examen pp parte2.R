#cargamos las librerias que usaremos
library (igraph)
library (igraphdata)
data(karate)#cargamos la base de datos carate

ejer2 <- function(red) { #esta función sirve para cualquier red
tres <- sort(degree(red), decreasing = TRUE)[1:3] #seleccionar 3 nodos mas importantes
distribution <- degree.distribution(red) #sacamos la distribución de la res
print(hist(distribution, main = "Histograma de distribuciones")) #imprimos el histograma de la distribución
diametro <- diameter(red) #sacamos el diametro
print(paste("Diámetro de la red", diametro)) #imprimimos el diametro de la res
print (cluster.distribution(red)) #sacamos la distribución de conexiones de la res
#solo hay un actor con un coeficiente de 1, esto debe ser porque a las personas que el considera sus amigos, también el lo consideran amigos. 
#Son una amistad más cercana
total <- sum(degree(red)) #sumamos el total de conexiones en los nodos
promedio <- mean (degree(red)) #promedio de degree
porcentaje <- total/100*promedio #sacamos el porcentaje de conexiones respecto al total
print(paste ("El porcentaje de conexiones es", porcentaje)) #imprimos porcentaje
print(paste("El promedio de conectividades es", promedio)) #imprimos promedio
}
ejer2(karate) #corremos la función con la base de datos karate


#con la medida de centralidad comparamos a los actores (nodos) más importantes de la res
centr_eigen(karate) 
# Actor 1, actor 3 y actor 34
centr_betw(karate)
#Actor 1, actor 29 y actor 34
centr_degree(karate)
#Actor 1, actor 33 y acror 34


#función para realizar y comparar los clusters

clusterss <- function(red) {
eb <- cluster_edge_betweenness(red, directed = FALSE) #método de clusterización 1
table(membership(eb))
plot(eb, red, main = "Método Edge Betwenness")#separa, imprime y colorea los clusters

fg <-cluster_fast_greedy(red)  #método de clusterización 2
table (membership(fg))
plot(fg, red, main = "Método Fast Greedy")

lp <- cluster_label_prop(red)  #método de clusterización 3
table (membership(lp))
plot(lp, red, main = "Método Label Prop")

le <- cluster_leading_eigen(red) #método de clusterización 1
table(membership(le))
plot(le, red, main = "Método Leading Eigen")

}

clusterss(karate) #corremos la función con la red de karate

#Con los resultados podemos observar que los métodos de clusterización que más 
#se acercan a la realidad son el Método Fast Greedy y Método Label Prop que generan
#los dos grupos con más personas, pero el mejor es Fast Greedy con la excepción de dejar 5 
#actores en otro grupo
