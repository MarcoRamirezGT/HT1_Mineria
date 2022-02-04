datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)


#Pregunta 4.1



library(tibbletime)
library(dplyr)
library(tidyverse)

datos[,'id']
datos[,'budget']
datos[,'originalTitle']


id<-datos[,'id']
original_title<-datos[,'originalTitle']
presu<-datos[,'budget']



q3<-data.frame(original_title,presu)
View(q3)
ask3<-q3[order(-q3$presu),]
ask3f<-head(ask3,n=10)
View(ask3f)



ggplot(data=ask3f, aes(x=reorder(original_title,-presu) , y=presu,fill=original_title)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mayor presupuesto", x="Peliculas", y="Presupuesto")





#Pregunta 4.3

datos[,'id']
datos[,'voteCount']
datos[,'originalTitle']


id<-datos[,'id']
Pelicula<-datos[,'originalTitle']
voteCount<-datos[('voteCount')]


q3<-data.frame(Pelicula,voteCount)
ask3<-q3[order(q3$voteCount),]
ask3f<-head(ask3,n=1)



ggplot(data=ask3f, aes(x=reorder(Pelicula,-voteCount) , y=voteCount,fill=Pelicula)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Pelicula con menos votos segun los usuarios", x="Peliculas", y="Votos")





#Pregunta 4.7

library(tidyverse)
id<-datos[,'id']
original_title<-datos[,'originalTitle']
genres_amount<-datos[,'genresAmount']

genre<-datos[,'genres']
ingresos<-datos[,'revenue']


ask6<-data.frame(original_title,genre,ingresos)
ask6order<-ask6[rev(order(ask6$ingresos)),]
ans6<-ask6order[!ask6order$ingresos=="",]
ask6orderf<-head(ans6,n=5)

data6<-ask6orderf
datas<-data6 %>%
  group_by(genre) %>%
  tally()

datas<-datas[order(-datas$n),]

View(datas)
genero<-datas$genre
ggplot(data=datas, aes(x=genre, y=n,fill=genero)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Las películas de qué genero principal obtuvieron mayores ganancias", x="Genero", y="Ganancias")

# 
# barplot(height = ask3f$presu,names=ask3f$original_title,
#         col=c('red','green','purple','blue','yellow'),
#         main = 'Top 10 peliculas con mayor presupuesto',
#         scale_y_continuous(labels=scales::dollar))
# 
# 
# ggplot(data=ask3f, mapping=aes(x=original_title, y=names,fill=original_title)) + 
#   stat_summary(fun.data=mean_sdl, geom="bar") + 
#   scale_y_continuous(labels=scales::dollar) + 
#   labs(title="Porcentaje de ingresos de las peliculas segun su mes de lanzamiento", x="Mes", y="Ingresos")


