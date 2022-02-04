datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)


#Pregunta 4.1

#Llamamos a los paquetes necesarios para la ejecucion 
library(tibbletime)
library(dplyr)
library(tidyverse)

#Obtenemos las variables que necesitamos 
datos[,'id']
datos[,'budget']
datos[,'originalTitle']


id<-datos[,'id']
original_title<-datos[,'originalTitle']
presu<-datos[,'budget']


#Creamos el data frame del problema 
q3<-data.frame(original_title,presu)
View(q3)
#Ordenamos el frame por su cantidad de presupuesto
ask3<-q3[order(-q3$presu),]
#Limitamos la respuesta a 10.
ask3f<-head(ask3,n=10)
View(ask3f)


#Aca usamos ggplot para crear la representacion visual del frama, en este caso la usamos para hacer 
# un grafico de barras para demostar el frama
ggplot(data=ask3f, aes(x=reorder(original_title,-presu) , y=presu,fill=original_title)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mayor presupuesto", x="Peliculas", y="Presupuesto")





#Pregunta 4.3

#Llamada de las variables necesarias
datos[,'id']
datos[,'voteCount']
datos[,'originalTitle']


id<-datos[,'id']
Pelicula<-datos[,'originalTitle']
voteCount<-datos[('voteCount')]

#Creamos el frame para hacer la busqueda
q3<-data.frame(Pelicula,voteCount)
#Ordenamos los valores de los votos
ask3<-q3[order(q3$voteCount),]
#Limitamos la busqueda a 1, ya que solo nos pide la menos votada.
ask3f<-head(ask3,n=1)


#Aca usamos ggplot para crear la representacion visual del frama, en este caso la usamos para hacer 
# un grafico de barras para demostar el frama
ggplot(data=ask3f, aes(x=reorder(Pelicula,-voteCount) , y=voteCount,fill=Pelicula)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Pelicula con menos votos segun los usuarios", x="Peliculas", y="Votos")






#Pregunta 4.7

#Llamamos al paquete necesario para la pregunta
library(tidyverse)

#Llamamos a las variables que usaremos
datos<-read.csv('movies.csv')
ingresos<-datos[,'revenue']
genre<-datos[,'genres']

#Creamos el frame correspondiente
table12<-data.frame(ingresos,genre)
View(table12)

#Ordenamos los datos en base a los ingresos, es decir de mayor a menor
table12<-table12[order(-table12$ingresos),]
view(table12)




library(dplyr)
#Agrupamos y sacamos la media del genero, esto se hizo para comprobar que sea el promedio de generos
resut<-table12 %>% 
  group_by(genre) %>% 
  summarise_all(.funs = mean) 

#Condicionamos para que no hayan valores iguales a 0
result<-filter(resut,ingresos>0)
#Condicionamos que no hayan valores nulos.
result<-filter(result,genre!="")
View(result)
#Limitamos la vista a solo 10 resultados.
result<-head(result,n=10)
View(result)

#Creamos la grafica, en este caso una grafica de barras donde en el eje x se tienen el genero y el eje y la cantidad ingresada por genero.
ggplot(data=result, mapping=aes(x=reorder(genre, -ingresos), y=ingresos,fill=genre)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Las películas de qué genero principal obtuvieron mayores ganancias", x="Genero", y="Ingresos")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))






# Pregunta 4.10
#Llamamos al set de datos y las variables a usar.
datos<-read.csv('movies.csv')
directores<-datos[,'director']
calificacion<-datos[,'voteAvg']

#Creamos el data frame necesario
table12<-data.frame(directores,calificacion)
View(table12)

#Ordenamos de mayor a menor la califcacion.
table12<-table12[order(-table12$calificacion),]
view(table12)

#Filtrammos para que no hayan valores igual a 0
result<-filter(table12,calificacion>0)
#Filtramos para que no haya valores nulos.
result<-filter(table12,directores!="")
View(result)
#Limitamos la busqueda a 20, ya que eso es lo que se nos pide.
result<-head(result,n=20)
View(result)

#Creamos el grafico de barras.
ggplot(data=result, mapping=aes(x=reorder(directores, -calificacion), y=ingresos,fill=directores)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Quiénes son los directores que hicieron las 20 películas mejor calificadas", x="Directores", y="Calificacion")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))




# Pregunta 4.13

#Llamamos a las librerias correspondientes
library(tibbletime)
library(dplyr)
library(tidyverse)

#Llamamos las variables que utilizaremos
datos<-read.csv('movies.csv')
ingresos<-datos[,'revenue']
lanzamiento<-datos[,'releaseDate']
#Filtramos y llamamos por dia, mes y año 
lanzamiento<-as.Date(lanzamiento,"%Y-%m-%d")
ano<-format(lanzamiento,format="%Y")
mes<-format(lanzamiento,format="%m")
dia<-format(lanzamiento,format="%d")


#Creamos el frame que necesitamos 
table12<-data.frame(ingresos,mes)
View(table12)

#Ordenamos los valores en base al ingreso
table12<-table12[order(-table12$ingresos),]
View(table12)

#filtramos para que no hayan valores iguales a 0
result<-filter(table12,ingresos>0)
#Filtramos para que no hayan nulos.
result<-filter(table12,ingresos!="")
View(result)
#Limitamos a 3 la busqueda ya que si se deja el valor completo se vuelve en una busqueda repetida.
result<-head(result,n=3)
View(result)

#Creamos el grafico de barras.
ggplot(data=result, mapping=aes(x=reorder(ingresos, -mes), y=ingresos,fill=mes)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Meses con mejores ingresos", x="Mes", y="Ingresos")





