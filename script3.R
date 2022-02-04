datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)

library(tibbletime)
library(dplyr)
library(tidyverse)


#Ejercicio 3
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