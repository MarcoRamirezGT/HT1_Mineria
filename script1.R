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


id<-datos[,'id']
original_title<-datos[,'originalTitle']
voteCount<-datos[('voteCount')]


q3<-data.frame(original_title,voteCount)
ask3<-q3[order(-q3$voteCount),]
ask3f<-head(ask3,n=5)
View(ask3f)



barplot(height = ask3f$voteCount,names=ask3f$original_title,
        col=c('red','green','purple','blue','yellow'),
        main = 'Top 5 peliculas con mas votos en IMDB')


#Pregunta 4.6

#Guardamos el valor de las columnas en una variable

library(tidyverse)
id<-datos[,'id']
original_title<-datos[,'originalTitle']
genres_amount<-datos[,'genresAmount']


genre<-datos[,'genres']

release_date<-datos[,'releaseDate']


ask6<-data.frame(original_title,genre,release_date)
ask6order<-ask6[rev(order(as.Date(ask6$release_date,format="%Y-%m-%d"))),]
ans6<-ask6order[!ask6order$genre=="",]
ask6orderf<-head(ans6,n=20)

View(ask6orderf)


data6<-ask6orderf
datas<-data6 %>%
  group_by(genre) %>%
  tally()

View(datas)
barplot(height = datas$n,names=datas$genre,
        col=rainbow(10),
        main = 'Generos principales de las 20 peliculas mas recientes',
        xlab = "Genero principal",
        ylab = 'Cantidad')


#Pregunta 4.9
library(tidyverse)
db<-read.csv('movies.csv')
men_count<-as.numeric(db[,'castMenAmount'])
women_count<-as.numeric(db[,'castWomenAmount'])
movie<-db[,'originalTitle']
popularity<-db[,'popularity']
ingresos<-db[,'revenue']


tableAsk9<-data.frame(movie,men_count,women_count,popularity,ingresos)

View(tableAsk9)
str(tableAsk9)



tableAsk9OrderFilterMen<-filter(tableAsk9,men_count > women_count)
View(tableAsk9OrderFilterMen)

promedioHombres<-sum(tableAsk9OrderFilterMen$ingresos)
promedioMujeres<-sum(tableAsk9OrderFilterWomen$ingresos)

x<-c(promedioHombres,promedioMujeres)
labels<-c("Hombres","Mujeres")

piepercent<- round(100*x/sum(x), 1)

pie(x, labels=piepercent, main = "Porcentaje de ganancias de cada sexo", col = rainbow(length(x)))
legend("topright", c("Hombres","Mujeres"), cex = 0.8,
       fill = rainbow(length(x)))

barplot(height = x,names=labels,
        col=rainbow(10),
        main = 'Ganancia de las peliculas con mayor cantidad de actores o actrices',
        xlab = "Sexo",
        ylab = 'Promedio de ingreso')





