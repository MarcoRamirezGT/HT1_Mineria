datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)

library(tibbletime)
library(dplyr)
library(tidyverse)



#EJERCICIO 3
#para la variable id
datos[,'id']
id<-datos[,'id']
density <- dnorm(id)

ggplot(data.frame(x = id, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable id", y = "Densidad")

#para la variable budget
datos[,'budget']
budget<-datos[,'budget']
density <- dnorm(budget)

ggplot(data.frame(x = budget, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable budget", y = "Densidad")

#para la variable runtime
datos[,'runtime']
runtime<-datos[,'runtime']
density <- dnorm(runtime)

ggplot(data.frame(x = runtime, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable runtime", y = "Densidad")

#para la variable popularity
datos[,'popularity']
popularity<-datos[,'popularity']
density <- dnorm(popularity)

ggplot(data.frame(x = popularity, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable popularity", y = "Densidad")

#para la variable voteAvg
datos[,'voteAvg']
voteAvg<-datos[,'voteAvg']
density <- dnorm(voteAvg)

ggplot(data.frame(x = voteAvg, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable voteAvg", y = "Densidad")

#para la variable voteCount
datos[,'voteCount']
voteCount<-datos[,'voteCount']
density <- dnorm(voteCount)

ggplot(data.frame(x = voteCount, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable voteCount", y = "Densidad")

#para la variable genresAmount
datos[,'genresAmount']
genresAmount<-datos[,'genresAmount']
density <- dnorm(genresAmount)

ggplot(data.frame(x = genresAmount, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable genresAmount", y = "Densidad")

#para la variable productionCoAmount
datos[,'productionCoAmount']
productionCoAmount<-datos[,'productionCoAmount']
density <- dnorm(productionCoAmount)

ggplot(data.frame(x = productionCoAmount, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable productionCoAmount", y = "Densidad")

#para la variable productionCountriesAmount
datos[,'productionCountriesAmount']
productionCountriesAmount<-datos[,'productionCountriesAmount']
density <- dnorm(productionCountriesAmount)

ggplot(data.frame(x = productionCountriesAmount, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable productionCountriesAmount", y = "Densidad")

#para la variable actorsAmount
datos[,'actorsAmount']
actorsAmount<-datos[,'actorsAmount']
density <- dnorm(actorsAmount)

ggplot(data.frame(x = actorsAmount, y = density)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Variable actorsAmount", y = "Densidad")



#PREGUNTA 4.2
#Variables a utilizar
datos[,'id']
datos[,'revenue']
datos[,'originalTitle']

#asignación de las variables
id<-datos[,'id']
Pelicula<-datos[,'originalTitle']
ingresos<-datos[, ('revenue')]

#Creacion del dataframe
q2<-data.frame(Pelicula,ingresos)
ask2<-q2[order(-q2$ingresos),]
ask2f<-head(ask2,n=10)

#Creacion de un diagrama de barras para tener una representacion visual y analizar mejor los datos
ggplot(data=ask2f, aes(x=reorder(Pelicula,-ingresos) , y=ingresos,fill=Pelicula)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mas ingresos", x="Peliculas", y="Ingresos")



#PREGUNTA 4.8
#Variables a utilizar
datos[,'id']
datos[,'revenue']
datos[,'originalTitle']
datos[,'actorsAmount']

#Asignacion de las variables
id<-datos[,'id']
Pelicula<-datos[,'originalTitle']
ingresos<-datos[, 'revenue']
actores<-datos[, ('actorsAmount')]

#Creacion del dataframe para realizar analisis de tres variables
q8<-data.frame(ingresos,actores,Pelicula)
ask8<-q8[order(-q8$ingresos),]
ask8f<-head(ask8,n=10)

#Representacion grafica mediante un diagrama de barras para comparar los ingresos
#de las peliculas segun la cantidad de actores
ggplot(data=ask8f, aes(x=reorder(Pelicula,-ingresos) , y=ingresos,fill=actores)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mas actores y sus ingresos", x="Cantidad de Actores por Pelicula", y="Ingresos")



#PREGUNTA 4.14
#Lectura y guardado de los datos
datos<-read.csv('movies.csv')

#Creacion de un diagrama de dispersión para relacionar las variables voteAvg y revenue
#y asi correlacionar las calificaciones de las peliculas con su exito comercial
plot(x = datos$voteAvg, y = datos$revenue,
     main = "Calificaciones - Exito Comercial",
     xlab = "Promedio de Votos", ylab = "Ingresos")

ggplot(datos, aes(voteAvg, revenue)) + geom_point()
