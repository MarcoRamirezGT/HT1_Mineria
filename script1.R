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


q3<-data.frame(id,original_title,voteCount)
View(q3)
