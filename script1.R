#Pregunta
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
women_count<-as.numeric(db[,'castMenAmount'])
movie<-db[,'originalTitle']
popularity<-db[,'popularity']
ingresos<-db[,'revenue']

#Creamos el dataframe para recolectar los datos
tableAsk9<-data.frame(movie,men_count,women_count,popularity,ingresos)

View(tableAsk9)
str(tableAsk9)


#En este momento creamos dos sub tablas, la primera sera tableAsk9OrderFilterMen, la cual permitira tener almacenado solo las peliculas donde hayan mas hombres que mujeres
tableAsk9OrderFilterMen<-filter(tableAsk9,men_count > women_count)
# Y creamos la subtabla tableAsk9OrderFilterWomen la cual solo tendra las peliculas donde hallan mas mujeres que hombres. 
tableAsk9OrderFilterWomen<-filter(tableAsk9,men_count < women_count)
View(tableAsk9OrderFilterMen)
View(tableAsk9OrderFilterWomen)

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

str(piepercent)

piepercent[1:1]
#Sacamos el porcent
piepercentPopu<- round(100*xPopu/sum(xPopu), 1)



pie(x, labels=piepercent, main = "Porcentaje de ganancias de las peliculas cuando hay un sexo predominante", col = rainbow(length(x)))
legend("topright", c("Hombres","Mujeres"), cex = 0.8,
       fill = rainbow(length(x)))

pie(xPopu, labels=piepercentPopu, main = "Porcentaje de popularidad de las peliculas cuando hay un sexo predominante", col = rainbow(length(x)))
legend("topright", c("Hombres","Mujeres"), cex = 0.8,
       fill = rainbow(length(xPopu)))



#Pregunta 4.12


library(tibbletime)
library(dplyr)
library(tidyverse)
datos<-read.csv('movies.csv')
ingresos<-datos[,'revenue']
lanzamiento<-datos[,'releaseDate']
lanzamiento<-as.Date(lanzamiento,"%Y-%m-%d")
ano<-format(lanzamiento,format="%Y")
mes<-format(lanzamiento,format="%m")
dia<-format(lanzamiento,format="%d")



table12<-data.frame(ingresos,mes)
View(table12)

table12<-table12[order(-table12$ingresos),]
View(table12)


ggplot(data=table12, mapping=aes(x=mes, y=ingresos,fill=mes)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Porcentaje de ingresos de las peliculas segun su mes de lanzamiento", x="Mes", y="Ingresos")

  

#Pregunta 4.15


library(tibbletime)
library(dplyr)
library(tidyverse)

db<-read.csv('movies.csv')
str(db)
genero<-db[,'genres']
peli<-db[,'originalTitle']
duracion<-db[,'runtime']


result15<-data.frame(peli,genero,duracion)
result15<-result15[order(-result15$duracion),]
result15<-head(result15,n=5)
View(result15)

result15[1,1]<-'Pelicula con mayor duracion'

ggplot(data=result15, aes(x=genero, y=duracion,fill=peli)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mayor presupuesto", x="Peliculas", y="Presupuesto")


tinytex::install_tinytex()
# to uninstall TinyTeX, run
# tinytex::uninstall_tinytex()

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
