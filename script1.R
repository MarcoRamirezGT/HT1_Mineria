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

barplot(prop.table(table(ask3f$original_title)))

barplot(height = ask3f$voteCount,names=ask3f$original_title,
        col=c('red','green','purple','blue','yellow'))


