ejer4 <- function(red) {
library(igraph)
library(igraphdata)
  distribution <- degree.distribution(red) #sacamos la distribución
  degr <- degree(red) #calculamos el degree
  dm <-degr[order(degr, decreasing = TRUE)] #ordenamos el degree de mayor a menor
  trayectoria <- path.length.hist(yeast)  #calculamos la trayectoria
  trayectoria <- is.vector(trayectoria) #convertimos los valores a vector
  t <- trayectoria[order(trayectoria, decreasing = TRUE)] #ordenamos de mayor a menor
  c1 <- centr_eigen(red) #valor de centralidad 1
  c1 <- is.vector(c1) #convertimos en vector
  c2 <- centr_degree(red) #valor de centralidad 2
  c2 <- is.vector(c2) #convertimos en vectore
  c3 <- centr_betw(red) #valor de centralidad 3
  c3 <- is.vector(c3) #convertimos en vector
  c4 <-c1[order(c1, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 1
  c5 <-c2[order(c2, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 2
  c6 <-c3[order(c3, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 3
  eb <- cluster_edge_betweenness(red, directed = FALSE) #cluster método 1
  table(membership(eb)) #separamos por grupos
  graf1<- plot(eb, red) #imprimos vector con el plot
  fg <-cluster_fast_greedy(red)  #cluster método 2
  table (membership(fg)) #separamos por grupos
  graf2 <- plot(fg, red) #imprimos vector con el plot
  comparacion <- compare(lp, fg, method = "adjusted.rand") #comparamos los plots
  comparacion <- comparacion[order(comparación, decreasing = TRUE)] #comparamos valores de la comparación de mayor a menor
  print(paste("Distrubución de probabilidades",distribution )) #imprimos la distribución de probabilidades
  print(hist(distribution)) #imprimimos el histograma de la distribución 
  print(boxplot(distribution)) #imprimimos el boxplot de la distribución
  print(paste("Degree máximo de la red", dm[1])) #Imprimimos el degree máximo
  print(diametro(red)) #imprimimos el diametro
  print(paste("La trayectoria más larga es", t[1])) #imprimos la trayectoria más larga
  print(paste("Centralidad 1", c4[1:10])) #imprimimos lo 10 valores más altos de la centralidad 1
  print(paste("Centralidad 2", c5[1:10])) #imprimimos lo 10 valores más altos de la centralidad 2
  print(paste("Centralidad 3", c6[1:10])) #imprimimos lo 10 valores más altos de la centralidad 3
  print(graf1) #imprimimos el cluster 1
  print(graf2) #imprimimos el cluster 2
  print(comparacion) #imprimimos la comparación
  print(paste("El cluster", comparacion, "es más grande"))
}


  ejer4(yeast)
  
  library(igraphdata)
  library(igraph) 
  
  ejer5 <- function(red) {
   distribution <- degree.distribution(red) #sacamos la distribución
  degr <- degree(red) #calculamos el degree
  dm <-degr[order(degr, decreasing = TRUE)] #ordenamos el degree de mayor a menor
  quince <- which(degree(red) >=15) #seleccionar los nodos con 15 o más conectividades
  dat15 <- length(quince) #cuantos datos tienen 15 o más conectividades
  longtotal <- length(degree(red)) #obtenemos la longitus de todos los datos
  por <- (dat15/longtotal)*100 #regla de tres para obtener el porcentaje de nodos con 15 o más conexiones
  trayectoria <- path.length.hist(red)  #calculamos la trayectoria
  trayectoria <- is.vector(trayectoria) #convertimos los valores a vector
  t <- trayectoria[order(trayectoria, decreasing = TRUE)] #ordenamos de mayor a menor
  nodos <- sort(degree(red), decreasing = TRUE)[1:10] #seleccionar 10 nodos mas importantes
  dia <- c() #vector vacio del diametro para añadir los nodos más conectados de la red a través de un ciclo for
  red2 <- red #renombrar la base de datos
  for(i in 1:10) { #indica que contara del elemento 1:10
    dia[i] <- diameter(red2) #sacar el diametro de los 10 nodos más importantes y añadir a el vector vacío de diametro
    red2 <- delete_vertices (red2, sample (nodos [i])) #eliminar los 10 nodos más importnates
  }
  c1 <- centr_eigen(red) #valor de centralidad 1
  c1 <- is.vector(c1) #convertimos en vector
  c2 <- centr_degree(red) #valor de centralidad 2
  c2 <- is.vector(c2) #convertimos en vectore
  c3 <- centr_betw(red) #valor de centralidad 3
  c3 <- is.vector(c3) #convertimos en vector
  c4 <-c1[order(c1, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 1
  c5 <-c2[order(c2, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 2
  c6 <-c3[order(c3, decreasing = TRUE)] #ordenamos de mayor a menor los valores de centralidad 3
  eb <- cluster_edge_betweenness(red, directed = FALSE) #cluster método 1
  table(membership(eb)) #separamos por grupos
  graf1<- plot(eb, red) #imprimos vector con el plot
  fg <-cluster_fast_greedy(red)  #cluster método 2
  table (membership(fg)) #separamos por grupos
  graf2 <- plot(fg, red) #imprimos vector con el plot
  comparacion <- compare(lp, fg, method = "adjusted.rand") #comparamos los plots
  comparacion <- comparacion[order(comparación, decreasing = TRUE)] #comparamos valores de la comparación de mayor a menor
  print(paste("A. Distrubución de probabilidades",distribution )) #imprimos la distribución de probabilidades
  print(hist(distribution)) #imprimimos el histograma de la distribución 
  print(boxplot(distribution)) #imprimimos el boxplot de la distribución
  print(paste("C. Nodos con más de 15 conexiones", por))
  print(paste("D. Degree máximo de la red", dm[1])) #Imprimimos el degree máximo
  print(paste ("E. El diametro de la red es", diametro(red))) #imprimimos el diametro
  print(paste("F. La trayectoria más larga es", t[1])) #imprimos la trayectoria más larga
  print(paste("G. Diamétro 10 nodos más importantes", dia))
  print(paste("Centralidad 1", c4[1:10])) #imprimimos lo 10 valores más altos de la centralidad 1
  print(paste("Centralidad 2", c5[1:10])) #imprimimos lo 10 valores más altos de la centralidad 2
  print(paste("Centralidad 3", c6[1:10])) #imprimimos lo 10 valores más altos de la centralidad 3
  print(graf1) #imprimimos el cluster 1
  print(graf2) #imprimimos el cluster 2
  print(comparacion) #imprimimos la comparación
  print(paste("El cluster", comparacion, "es más grande"))
  }

library(igraphdata)
data(yeast)
ejer5(yeast)  

degree(yeast)

quince <- which(degree(red) >=15) #seleccionar los nodos con 15 o más conectividades
dat15 <- length(quince) #cuantos datos tienen 15 o más conectividades
longtotal <- length(degree(red)) #obtenemos la longitus de todos los datos
por <- (dat15/longtotal)*100 #regla de tres para obtener el porcentaje de nodos con 15 o más conexiones