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


