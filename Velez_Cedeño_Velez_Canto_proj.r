#librerías a utilizar

library("cba")
library("clusteval")
library("ggplot2")
library("MASS")
library("klaR")

#Funciones

graficar<-function(id_provincia){
  provincia<-id_provincia
  etiquetas<-c("muy bueno","regular","malo","bueno","muy malo")
  etiquetas<-paste(etiquetas,"\n",conocimiento[provincia,],"%")
  pie((conocimiento[provincia,]),main = provincias[provincia],labels = etiquetas,col = rainbow(length(conocimiento[24,])))
}

nombre_a_idprov <- function(nombre,p){
  for(i in 1: 24 ){
    if(dimnames(p)[[1]][i]==nombre){
      return(i)
    }
  }
}

leer_dataset<-function(ruta){
  return(read.csv2(ruta, header=TRUE))
}

#importación base de datos
dataset_original <- leer_dataset("C:/Users/XavierVelez/Desktop/BASE_RAS_2018.csv")
dataset <- dataset_original
# variables a utilizar
#Se selecciona la columna 12 a la 18
dataset<-(dataset_original[,12:18])

#Se agregan las columnas con nombre k15k14, k1520...k1534
dataset<- cbind(dataset, c(dataset_original$k1514),c(dataset_original$k1516), c(dataset_original$k1520), c(dataset_original$k1522), c(dataset_original$k1529), c(dataset_original$k1530), c(dataset_original$k1532), c(dataset_original$k1534),c(dataset_original$k1536))


k = 5

#Agrupamiento
set.seed(1987)

  #Agrupamiento 2
agrupamiento1 <-kmodes(dataset, k, iter.max = 10, weighted = FALSE)

#Matriz de partición
matriz_particion<-data.matrix(agrupamiento1$cluster)


#Determinar el conocimiento que tienen las provincias acerca del control 
#de desechos solidos en base al cluster que pertenece

provincias<-c("Azuay","Bolívar", "Cañar", "Carchi", "Cotopaxi", "Chimborazo", "El Oro", "Esmeraldas", "Guayas", "Imbabura","Loja","Los Rios","Manabí","Morona Santiago","Napo","Pastaza","Pichincha","Tungurahua","Zamora Chinchipe", "Galápagos","Sucumbíos","Orellana","Santo Domingo","Santa Elena")

conocimiento<-matrix(0,nrow = 24, ncol = k, dim = list(provincias, c(1:k) ))

#le asignamos un grupo  al i-nesimo registro en la matriz conocimiento
for(i in 1: length(agrupamiento1$cluster)){
  grupo<- agrupamiento1$cluster[i]
  n_prov <- dataset_original$prov_ubi[i]
  conocimiento[n_prov,grupo]<-conocimiento[n_prov,grupo]+1
}

#Determinamos el promedio
for(i in 1:nrow(conocimiento)){
  n <- sum(conocimiento[i,])
  for(j in 1: ncol(conocimiento) ){
    conocimiento[i, j] <- round((conocimiento[i, j] * 100 /n),2)
  }
}

conocimiento

#En base a los observaciones determinamos cuantos 1 y 0 habían
#así determinamos el porcentaje total de si y el porcentaje total de no
#de cada cluster
cono <-matrix(0,nrow = k, ncol = 4, dim = list(c(), c("no", "sí", "%no", "%sí") ))

for(i in 1: length(agrupamiento1$cluster)){
  for(j in 1: ncol(dataset) ){
    cono[agrupamiento1$cluster[i], (dataset[i, j]+1)] <- cono[agrupamiento1$cluster[i], (dataset[i, j]+1)] + 1
  }
}

for(i in 1: nrow(cono)){
  cono[i,3]<- round((cono[i,1] * 100 /sum(cono[i,c(1,2)])),2)
  cono[i,4]<- round((cono[i,2] * 100 /sum(cono[i,c(1,2)])),2)
}

#Total de conocimiento de cada cluster
grups<-matrix(0,nrow = k, ncol = 2, dim = list(c("muy bueno","bueno","regular","malo","muy malo"), c("Nº", "%sí")))
grups[,2]<- cono[order(cono[,4]),4]
grups[,1]<- order(cono[,4])


#Graficiamos el conocimiento sobre los desechos solidos en establecimiento de salud para cada provincia

mejores<-data.frame((conocimiento[order(conocimiento[,1]),1]))

graficar(nombre_a_idprov(dimnames(mejores)[[1]][24],conocimiento))
graficar(nombre_a_idprov(dimnames(mejores)[[1]][23],conocimiento))
graficar(nombre_a_idprov(dimnames(mejores)[[1]][22],conocimiento))



buenos<-data.frame((conocimiento[order(conocimiento[,4]),4]))

graficar(nombre_a_idprov(dimnames(buenos)[[1]][24],conocimiento))
graficar(nombre_a_idprov(dimnames(buenos)[[1]][23],conocimiento))
graficar(nombre_a_idprov(dimnames(buenos)[[1]][22],conocimiento))

regulares<-data.frame((conocimiento[order(conocimiento[,2]),2]))

graficar(nombre_a_idprov(dimnames(regulares)[[1]][24],conocimiento))
graficar(nombre_a_idprov(dimnames(regulares)[[1]][23],conocimiento))
graficar(nombre_a_idprov(dimnames(regulares)[[1]][22],conocimiento))




malos<-data.frame((conocimiento[order(conocimiento[,3]),3]))

graficar(nombre_a_idprov(dimnames(malos)[[1]][24],conocimiento))
graficar(nombre_a_idprov(dimnames(malos)[[1]][23],conocimiento))
graficar(nombre_a_idprov(dimnames(malos)[[1]][22],conocimiento))




peores<-data.frame((conocimiento[order(conocimiento[,5]),5]))

graficar(nombre_a_idprov(dimnames(peores)[[1]][24],conocimiento))
graficar(nombre_a_idprov(dimnames(peores)[[1]][23],conocimiento))
graficar(nombre_a_idprov(dimnames(peores)[[1]][22],conocimiento))


