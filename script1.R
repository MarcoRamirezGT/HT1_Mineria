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
View(data6)
#Creamos un dataframe con las columnas necesarias.



