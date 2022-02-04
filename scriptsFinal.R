#Marco Ramirez 19588
#Pregunta 1
datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)





#Pregunta 3


datos[,'id']
datos[,'voteCount']
datos[,'originalTitle']

#Le asignamos a la variable id los valores de la columna id de la DB
id<-datos[,'id']
#Le asignamos a la variable pelicula los valores de la columna originalTitle de la DB
Pelicula<-datos[,'originalTitle']
#Le asignamos a la variable voteCount los valores de la columna voteCount de la DB
voteCount<-datos[('voteCount')]
#Al tener seleccionada las columnas con las cuales deseamos trabajar creamos un dataframe, el cual nos permitira observar de mejor manera los datos recolectados.

#Creamos el dataframe con las columnas que deseamos
q3<-data.frame(Pelicula,voteCount)
#Ordenamos el dataframe para que nos indique de manera descendente los valores de cantidad de votos
ask3<-q3[order(-q3$voteCount),]
#Le indicamos al dataframe que solo deseamos ver las primeros 5 filas. 
ask3f<-head(ask3,n=5)
#Creamos el grafico de barras que nos demostrara graficamente las 5 peliculas con mayor voto
ggplot(data=ask3f, aes(x=reorder(Pelicula,-voteCount) , y=voteCount,fill=Pelicula)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 5 peliculas con mas votos en IMDB", x="Peliculas", y="Votos")





#Pregunta 4.6

#Guardamos el valor de las columnas en una variable

library(tidyverse)
id<-datos[,'id']
original_title<-datos[,'originalTitle']
genres_amount<-datos[,'genresAmount']

#Nuevamente guardamos los datos de las columnas deseadas en nuevas variables. 

genre<-datos[,'genres']

release_date<-datos[,'releaseDate']

#Creamos el dataframe para poder ver mejor los datos
ask6<-data.frame(original_title,genre,release_date)
#Ordenamos la columna de release_date por fecha, para esto fue necesario cambiar el formato de la columna, ya que antoriormente
#se encontraba la columna como tipo char, por ello se cambio a formato date, facilitando asi el ordenamiento de la columna,
ask6order<-ask6[rev(order(as.Date(ask6$release_date,format="%Y-%m-%d"))),]
#Luego tambien filtramos la informacion de genero, ya que tenemos generos vacios, esto los descartamos para fines estadisticos
ans6<-ask6order[!ask6order$genre=="",]
#Le indicamos que solo deseamos ver las primeras 20 peliculas
ask6orderf<-head(ans6,n=20)
#Obteniendo asi las 20 peliculas mas recientes con su respectivo genero principal



data6<-ask6orderf
#Realizamos un group by para poder juntar todas las peliculas del mismo genero
datas<-data6 %>%
  group_by(genre) %>%
  tally()
#Ordenamos en base a la cantidad de repeticiones que hubieron
datas<-datas[order(-datas$n),]

View(datas)
genero<-datas$genre
#Realizamos el grafico correspondiente para poder mostrar el genero principal del las 20 peliculas. Donde se muestra que genero fue el que se repito mas recientemente.

ggplot(data=datas, aes(x=genre, y=n,fill=genero)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Genero principal de las 20 peliculas mas recientes", x="Genero", y="Cantidad")




#Pregunta 4.9

library(tidyverse)

#Le asignamos a las variables las columnas correspondientes para simplicidad

db<-read.csv('movies.csv')
#Ya que dentro de la DB la columna castMenAmount esta identificada como tipo char fue necesario cambiar la columna a tipo numerico

men_count<-as.numeric(db[,'castMenAmount'])
#Nuevamente la columna de castMenAmount estaba identificada como tipo char fue necesario cambiar a tipo numerico

women_count<-as.numeric(db[,'castWomenAmount'])
movie<-db[,'originalTitle']
popularity<-db[,'popularity']
ingresos<-db[,'revenue']

#Creamos el dataframe para recolectar los datos

tableAsk9<-data.frame(movie,men_count,women_count,popularity,ingresos)




#En este momento creamos dos sub tablas, la primera sera tableAsk9OrderFilterMen, la cual permitira tener almacenado solo las peliculas donde hayan mas hombres que mujeres
tableAsk9OrderFilterMen<-filter(tableAsk9,men_count > women_count)

# Y creamos la subtabla tableAsk9OrderFilterWomen la cual solo tendra las peliculas donde hallan mas mujeres que hombres. 
tableAsk9OrderFilterWomen<-filter(tableAsk9,men_count < women_count)

#Sumamos todos los ingresos de las peliculas donde hallan mas hombres
promedioHombres<-sum(tableAsk9OrderFilterMen$ingresos)

#Sumamos todos los ingresos de las peliculas donde hallan mas mujeres
promedioMujeres<-sum(tableAsk9OrderFilterWomen$ingresos)

#sumamos la popularidad de las peliculas donde hallan mas hombres que mujeres 
poMen<-sum(tableAsk9OrderFilterMen$popularity)
#sumamos la popularidad de las peliculas donde hallan mas mujeres que hombres
poWomen<-sum(tableAsk9OrderFilterWomen$popularity)
#Creamos la columna de la suma de popularidad de los hombres y mujeres
xPopu<-c(poMen,poWomen)
labels<-c("Hombres","Mujeres")
#Creamos la columna de la suma de ingresos de los hombres y mujeres
x<-c(promedioHombres,promedioMujeres)
labels<-c("Hombres","Mujeres")
#Sacamos el porcentaje de ingreso de cada sexo
piepercent<- round(100*x/sum(x), 1)
#Sacamos el porcent
piepercentPopu<- round(100*xPopu/sum(xPopu), 1)

#Generamos el grafico de PIE para observar graficamente el porcentaje que atribuye cada sexo con el tema de ganancias
pie(x, labels=piepercent, main = "Porcentaje de ganancias de las peliculas cuando hay un sexo predominante", col = rainbow(length(x)))
legend("topright", c("Hombres","Mujeres"), cex = 0.8,
       fill = rainbow(length(x)))
#Generamos el grafico de PIE para observar graficamente el porcentaje que atribuye cada sexo con el tema de popularidad
pie(xPopu, labels=piepercentPopu, main = "Porcentaje de popularidad de las peliculas cuando hay un sexo predominante", col = rainbow(length(x)))
legend("topright", c("Hombres","Mujeres"), cex = 0.8,
       fill = rainbow(length(xPopu)))



#Pregunta 4.12
#Llamamos a llamar las librerias necesarias.
library(tibbletime)
library(dplyr)
library(tidyverse)
#Guardamos en variables los datos de las columnas que necesitamos 
datos<-read.csv('movies.csv')
ingresos<-datos[,'revenue']
lanzamiento<-datos[,'releaseDate']
#Cambiamos el formato de la columna, ya que dentro de la BD era tipo char por ende se cambio a formato de fecha
lanzamiento<-as.Date(lanzamiento,"%Y-%m-%d")
#Separamos la fecha por 3 columnas, una para dia, otra para mes y otra para ano, para mayor facilidad
ano<-format(lanzamiento,format="%Y")
mes<-format(lanzamiento,format="%m")
dia<-format(lanzamiento,format="%d")


#Creamos el dataframe que guarde el ingreso de cada pelicula y su respectivo ingreso que obtuvo
table12<-data.frame(ingresos,mes)


#Creamos la grafica de barras la cual promedia el ingreso de cada mes, obteniendo el siguiente resultado.
pregunta4.12<-ggplot(data=table12, mapping=aes(x=mes, y=ingresos,fill=mes)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Porcentaje de ingresos de las peliculas segun su mes de lanzamiento", x="Mes", y="Ingresos")
#Muestra la grafica
pregunta4.12

#Pregunta 4.15
#Llamamos a llamar las librerias necesarias.
library(tibbletime)
library(dplyr)
library(tidyverse)
#Guardamos en variables los datos de las columnas que necesitamos 
db<-read.csv('movies.csv')
str(db)
genero<-db[,'genres']
peli<-db[,'originalTitle']
duracion<-db[,'runtime']
#Creamos el dataframe con las columnas necesarias.
result15<-data.frame(peli,genero,duracion)
#Ordenamos el dataframe segun el tiempo de duracion de cada pelicula
result15<-result15[order(-result15$duracion),]
#Le indicamos que solo deseamos ver las 7 peliculas con mayor duracion
result15<-head(result15,n=7)
r1<-head(result15,n=7)

#Debido a que la pelicula posee un nombre extremadamente largo se cambio el nombre para mostrar de mejor manera la grafica. 
result15[1,1]<-'Pelicula con mayor duracion'

#Creamos el grafico de barras.
pregunta4.15<-ggplot(data=result15, aes(x=genero, y=duracion,fill=peli)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 7 peliculas con mayor duracion y su genero principal", x="Genero", y="Duracion")
#Muestra la grafica
pregunta4.15




