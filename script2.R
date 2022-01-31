datos<-read.csv('movies.csv')
summary(datos) #Resumen de los datos
nrow(datos) #Cantidad de columnas
ncol(datos) #Cantidad de columnas
str(datos) #Estructura de los datos
names(datos)


#Pregunta 4.1


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



barplot(height = ask3f$voteCount,names=ask3f$original_title,
        col=c('red','green','purple','blue','yellow'),
        main = 'Top 10 peliculas con mayor presupuesto')