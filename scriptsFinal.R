#Marco Ramirez 19588
#Alfredo Quezada 191002
#Estuardo Hernandez 19202
#Pregunta 1
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



#Pregunta 4.2
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





#Pregunta 4.3


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




#Pregunta 4.4

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




#Pregunta 4.5
# ¿Cuántas  películas  se  hicieron  en  cada  año?  ¿En  qué  año  se  hicieron  más películas? Haga un gráfico de barras 
library(tibbletime)
library(dplyr)
library(tidyverse)

datos<-read.csv('movies.csv')

pelicula<-datos[,'originalTitle']
lanzamiento<-datos[,'releaseDate']
#Cambiamos el formato de la columna, ya que dentro de la BD era tipo char por ende se cambio a formato de fecha
lanzamiento<-as.Date(lanzamiento,"%Y-%m-%d")
#Separamos la fecha por 3 columnas, una para dia, otra para mes y otra para ano, para mayor facilidad
ano<-format(lanzamiento,format="%Y")
mes<-format(lanzamiento,format="%m")
dia<-format(lanzamiento,format="%d")


#Creamos el dataframe
ask4_5<-data.frame(pelicula,ano)

resultado4_5<-ask4_5 %>%
  group_by(ano) %>%
  tally()

rsa<-resultado4_5[order(-resultado4_5$n),]

res45<-head(rsa,n=7)


pregunta4_5<-ggplot(data=res45, aes(x=reorder(ano,-n) , y=n,fill=ano)) +
  geom_bar(stat="identity")+
  # scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1))+
  labs(title="Los 7 anos con mayor lanzamiento de peliculas", x="", y="Cantidad de peliculas")


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



#Pregunta 4.8
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


#Pregunta 4.11


library(tibbletime)
library(dplyr)
library(tidyverse)
#Guardamos en variables los datos de las columnas que necesitamos 
datos<-read.csv('movies.csv')
ingresos<-datos[,'revenue']
presupuesto<-datos[,'budget']
peliculas<-datos[,'originalTitle']

rt411<-data.frame(ingresos,presupuesto)

grupo <- as.factor(ifelse(ingresos < presupuesto , "Grupo 1", "Grupo 2"))

plot(x=presupuesto, y=ingresos, pch = as.numeric(grupo), col = grupo)


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
ggplot(data=result, mapping=aes(x=mes, y=ingresos,fill=mes)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Meses con mejores ingresos", x="Mes", y="Ingresos")




#Pregunta 4.14
#Variables a utilizar
datos[,'id']
datos[,'popularity']
datos[,'originalTitle']

#Asignacion de variables
id<-datos[,'id']
Pelicula<-datos[,'originalTitle']
popularidad<-datos[, ('popularity')]

#Creacion del dataframe para comparar dos variables
q2<-data.frame(Pelicula,popularidad)
ask2<-q2[order(-q2$popularidad),]
ask2f<-head(ask2,n=10)

#Representacion grafica para el analisis del top 10 de peliculas mejores calificadas
ggplot(data=ask2f, aes(x=reorder(Pelicula,-popularidad) , y=popularidad,fill=Pelicula)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mayor indice de popularidad", x="Peliculas", y="Popularidad")



#Pregunta 4.15
#Llamamos a llamar las librerias necesarias.
library(tibbletime)
library(dplyr)
library(tidyverse)
#Guardamos en variables los datos de las columnas que necesitamos 
db<-read.csv('movies.csv')

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


#Preguntas extras

#Las 7 compa?ias productoras con mas peliculas lanzadas. 
#Llamamos a las librerias
library(tibbletime)
library(dplyr)
library(tidyverse)
db<-read.csv('movies.csv')

#Asignamos en variables las variables que necesitamos
productora<-db[,'productionCompany']
pelicula<-db[,'originalTitle']
#Creamos el dataframe
extra<-data.frame(pelicula,productora)

#Realizamos un group by de las peliculas en base a su productora
resultado<-extra %>%
  group_by(productora) %>%
  tally()
#Filtramos para evitar tener productoras vacias
res1<-filter(resultado,productora!='')
#Ordenamos para obtener la mayor cantidad de peliculas
res2<-res1[order(-res1$n),]
#Mostramos solo las 7 productoras con mayor lanzamiento de peliculas 
res3<-head(res2,n=7)

View(res3)
#Mostramos la grafica
ggplot(data=res3, aes(x=reorder(productora,-n), y=n,fill=productora)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 7 productoras con mayor lanzamiento de peliculas", x="Pelicula", y="Cantidad")


#Pregunta extra 2
#Las 3 productoras con mejores ingresos 
#Llamamos a las librerias
library(tibbletime)
library(dplyr)
library(tidyverse)
db<-read.csv('movies.csv')

#Asignamos en variables las variables que necesitamos
productora<-db[,'productionCompany']
ingresos<-db[,'revenue']
#Creamos el dataframe
extra2<-data.frame(productora,ingresos)
View(extra2)


resut<-extra2 %>% 
  group_by(productora) %>% 
  summarise_all(.funs = mean) 

#Filtramos para evitar tener productoras vacias
resut<-filter(resut,productora!='')
#Ordenamos para obtener la mayor cantidad de ingresos
resut<-resut[order(-resut$ingresos),]
#Mostramos solo las 3 productoras con mayores ingresos
resut<-head(resut,n=3)

#Creamos la grafica
preguntaEx2<-ggplot(data=resut, aes(x=reorder(productora,-ingresos), y=ingresos)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(vjust = 1, hjust=1))+
  labs(title="Las 3 productoras con mejores ingresos", x="", y="Ingresos")
#Mostramos la grafica
preguntaEx2


#Pregunta extra 3
#Paises donde se llevaron a cabo mas peliculas

library(tibbletime)
library(dplyr)
library(tidyverse)
db<-read.csv('movies.csv')

#Asignamos en variables las variables que necesitamos
pais<-db[,'productionCountry']
pelicula<-db[,'originalTitle']
#Creamos el dataframe
resEx3<-data.frame(pelicula,pais)

#Realizamos un group by de las peliculas en base a su pais
resultado<-resEx3 %>%
  group_by(pais) %>%
  tally()


#Filtramos para evitar tener productoras vacias
resut<-filter(resultado,pais!='')
#Ordenamos para obtener la mayor cantidad de ingresos
resut<-resut[order(-resut$n),]


#Mostramos solo las 3 productoras con mayores ingresos
resut<-head(resut,n=7)


#Creamos la grafica
preguntaEx3<-ggplot(data=resut, aes(x=reorder(pais,-n), y=n,fill=pais)) +
  geom_bar(stat="identity")+
 # scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1))+
  labs(title="Los 7 paises donde mas peliculas se realizaron", x="", y="Cantidad")
#MOstramos la grafica
preguntaEx3


#Pregunta extra 4
#?Cu?ntas pel?culas se han rodado en Guatemala?

library(tibbletime)
library(dplyr)
library(tidyverse)
db<-read.csv('movies.csv')

#Asignamos en variables las variables que necesitamos
pais<-db[,'productionCountry']
pelicula<-db[,'originalTitle']
#Creamos el dataframe
resEx4<-data.frame(pelicula,pais)
#Filtramos el dataframe para que solo muestre las peliculas que se rodaron en Guatemala
resultadoEx4<-filter(resEx4,pais=="Guatemala")

#Creamos la grafica
preguntaEx4<-ggplot(data=resultadoEx4, aes(x=pais, y=nrow(resultadoEx4),fill=pais)) +
  geom_bar(stat="identity")+
  # scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1))+
  labs(title="Cantidad de peliculas rodadas en Guatemala", x="", y="Cantidad")
#Mostramos la grafica
preguntaEx4


#Pregunta extra 5

#?Cu?les son las pel?culas m?s viejas?
#Llamamos a llamar las librerias necesarias.
library(tibbletime)
library(dplyr)
library(tidyverse)
#Guardamos en variables los datos de las columnas que necesitamos 
datos<-read.csv('movies.csv')
pelicula<-db[,'originalTitle']
lanzamiento<-datos[,'releaseDate']
#Cambiamos el formato de la columna, ya que dentro de la BD era tipo char por ende se cambio a formato de fecha
lanzamiento<-as.Date(lanzamiento,"%Y-%m-%d")
#Creamos el dataframe
resEx5<-data.frame(pelicula,lanzamiento)
View(resEx5)
#Ordenamos el dataframe por orden de lanzamiento
resutadoEx5<-resEx5[order(resEx5$lanzamiento),]
resutadoEx5<-head(resutadoEx5,n=5)


#Creamos la grafica
preguntaEx5<-ggplot(data=resutadoEx5, aes(x=reorder(pelicula,lanzamiento) , y=lanzamiento,fill=pelicula)) +
  geom_bar(stat="identity")+
  # scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1))+
  labs(title="Las 7 peliculas mas viejas guardadas en la base de datos", x="", y="Fecha de lanzamiento")
#Mostramos la grafica
preguntaEx5


##Pregunta extra 6
#?Cu?les son las pel?culas con mayores pa?ses de rodaje?

library(tibbletime)
library(dplyr)
library(tidyverse)
db<-read.csv('movies.csv')

#Asignamos en variables las variables que necesitamos
cantidadPaises<-db[,'productionCountriesAmount']
pelicula<-db[,'originalTitle']
#Creamos el dataframe

resEx6<-data.frame(pelicula,cantidadPaises)
#Ordenamos el dataframe por cantidad de paises
r1<-resEx6[order(-resEx6$cantidadPaises),]
#Solicitamos solo las 7 primeras filas
r2<-head(r1,n=7)
#Creamos la grafica
preguntaEx6<-ggplot(data=r2, aes(x=reorder(pelicula,-cantidadPaises) , y=cantidadPaises,fill=pelicula)) +
  geom_bar(stat="identity")+
  # scale_y_continuous(labels=scales::dollar) + 
  theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1))+
  labs(title="Las 7 peliculas con mayor cantidad de paises de rodaje", x="", y="Cantidad de paises")
#Mostramos la grafica
preguntaEx6
