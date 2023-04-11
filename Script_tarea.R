# Cargo las funciones que voy a utilizar despu?s
source("FuncionesRosa.R")

# Cargo las librerias 
library(questionr)

library(psych)

library(car)

library(corrplot)

library(caret)

library(ggplot2)

library(lmSupport)

library(unmarked)

library(VGAM)



library(pROC)

library(glmnet)

#APARTADO 1)
# Cargo los datos 
library(readxl)
datos <- read_excel("C:/Users/uxiat/Desktop/MasterBD/Mineria/Documentación minería de datos y modelización predictiva - Rosa Espinola-20221111/Tarea/DatosImpuestos_Tarea.xlsx")
View(datos)
# Voy a tomar como variables objetivo ActEconom_Cuali y ActEconom
datos <- datos[,-(5:9)]
datos
# APARTADO 2)
#Comprobamos el tipo de variable asignado a cada una
str(datos) 
#No todas las categoricas estan como factores

# Indico los factores (ActEconom_Cuali, Actividad_Municipio y Tamaño)
datos[,c(4,22,23)] <- lapply(datos[,c(4,22,23)], factor)
str(datos) 
summary(datos)

# Ver el reparto de las categorias de las variables cualitativas
freq(datos$Actividad_Municipio)
freq(datos$Tamano) # tiene una categoria rara "?"
freq(datos$CCAA) # voy a juntar ceuta y meilla porque tienen un 0% de representacion
# Cuento el numero de valores diferentes para las variables numericas para ver si puedo categorizar alguna
# todas tienen un valor elevadisimo de valores distintos asi que no puedo categorizar nada.
sapply(Filter(is.numeric, datos),function(x) length(unique(x))) 
freq(datos$ActEconom_Cuali)
freq(datos$DeudaFinanc)
freq(datos$Habit_Vivienda)
freq(datos$Vivienda_Grande)
View(freq(datos$Distrito))

#Si detecto alguna variable adicional, uso el codigo de antes

# Para otros estadisticos mas avanzados que los que muestra freq():
describe(Filter(is.numeric, datos)) #hay otro describe en otra libreria
#variables muy asimetricas
View(freq(datos$Vivienda_Grande))
freq(datos$Ganaderia)
freq(datos$Hosteleria)
freq(datos$Comercios)
freq(datos$Agricultura)

# las voy a pasar a variable categorica binaria...

# APARTADO 3) Corregir los errores del apartado 2)

# Missings no declarados variables cualitativas (NSNC, ?) los pasamos a na (no declarados)
datos$Tamano<-recode.na(datos$Tamano,"?")
summary(datos)
# Missings no declarados variables cuantitativas (-1, 99999)
datos$Locales<-replace(datos$Locales,which(datos$Locales==99999),NA)


# Valores fuera de rango los pasamos a a valores perdidos (se hace para los outliers)
datos$DeudaSaldada<-replace(datos$DeudaSaldada, which((datos$DeudaSaldada < 0)|(datos$DeudaSaldada>100)), NA)
datos$Empresario<-replace(datos$Empresario, which((datos$Empresario < 0)|(datos$Empresario>100)), NA)
summary(datos)

#arreglar errores en las variables categoricas (ecritura y/o poca representacion)
datos$Actividad_Municipio<-recode(datos$Actividad_Municipio, "c('Agricultura','Ganaderia')='Campo'")
freq(datos$Actividad_Municipio)
datos$CCAA <- factor(datos$CCAA)
datos$CCAA<-recode(datos$CCAA, "c('Ceuta','Melilla')='Ceuta y Melilla'")
freq(datos$CCAA)
# No paso a factor las CCAA porque me parece importante tenerlas por separado 


#Variables cualitativas con categorias poco representadas (MERGEAR CATEGORIAS POCO REPRESENTADAS)
datos$Ganaderia <- factor(replace(datos$Ganaderia, which(datos$Ganaderia>0),1))
datos$Hosteleria<- factor(replace(datos$Hosteleria, which(datos$Hosteleria>0),1))
datos$Comercios <- factor(replace(datos$Comercios, which(datos$Comercios>0),1))
datos$Agricultura <- factor(replace(datos$Agricultura, which(datos$Agricultura>0),1))
datos$Vivienda_Grande <- factor(replace(datos$Vivienda_Grande, which(datos$Vivienda_Grande>0),1))



#Indico la variableObj, el ID y las Input (los atipicos y los missings se gestionan solo de las input)
# dividimos el dataset en varables input y variables objetivo(en este caso tenermos 2 objetivo)
#var. objetivo
varObjCont<-datos$ActEconom
varObjBin<-datos$ActEconom_Cuali
#variables input (todas menos las objetivo)
# para seleccionarlas quitamos las columnas asociadas a las var. objetivo del dataset

input<-as.data.frame(datos[,-(3:4)])
View(input)

# tratamiento de valores atipicos. que será pasarlos a missing
# Cuento el porcentaje de atIpicos de cada variable.
# atipicosAmissing cuenta cuantos atipicos hay en cada variable [1 arg] y me los convierte a valores missing [2 arg]
sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[2]])/nrow(input) #valores bajos de atipicos
summary(input)
# Modifico los atipicos como missings
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[1]])
summary(input)


## MISSINGS buscar correlaciones
#Busco si existe algun patron en los missings, que me pueda ayudar a entenderlos. RELACION ENTRE LAS VARIABLES CON VALORES PERDIDOS
corrplot(cor(is.na(input[colnames(input)[colSums(is.na(input))>0]])),method = "ellipse",type = "upper") #No se aprecia ningun patron
summary(input)
# el color de las elipses determina el grado de correlacion de las varaibles


#Proporcion de missings por variable y observacion 
input$prop_missings<-apply(is.na(input),1,mean) # por filas (observaciones)
#me crea la nueva variable prop_missings

summary(input$prop_missings) # veo el summary de mi nueva variable que tiene por filas la proporcion de valores perdididos por variable
#max = 0.30 me dice que al menos hay una varaible con el 30% de las varaibles perdidas
#es menor del 50% por tanto no podemos eliminar esa varibale

(prop_missingsVars<-apply(is.na(input),2,mean)) #por columnas (varaiables)
#me salen todasa con menos de un 50% de valores missings por tanto no puedo eliminar ninguna variable
#elimino las observaciones y las variables con mas de la mitad de datos missings (No hay ninguna, no ejecuto este codigo)
#este codigo se usa e caso de que que tuvieramos que eliminar alguna variable u observacion
input <- subset(input, prop_missings< 0.5, select=names(prop_missingsVars)[prop_missingsVars<0.5])
varObjBin<-varObjBin[input$prop_missings<0.5] #Actualizar las observaciones de las variables objetivo
varObjCont<-varObjCont[input$prop_missings<0.5]


#para las VARIABLES CATEGORICAS
#Recategorizo categoricas con "suficientes" observaciones missings
#Solo la variable Clasificacion que es la que tiene un 26% missing
#Se considera una categoria mas los missing.

## RECATEGORIZAR (cREAR NUEVA CATEOGRIA) -> cuando el % de NA'S de una var categorica es sufucientemente elevado, es decir tienee buena representcion
freq(datos$Tamano)
freq(datos$Ganaderia)
freq(datos$Hosteleria)
freq(datos$Comercios)
freq(datos$Agricultura)
freq(datos$Vivienda_Grande)

# No recategorizamos Tamano xq el % de representacion de los Nan es del 1.1% por tanto es mejor inputarla
summary(input)
summary(datos) #para actividad municipio no haria falta xq no tiene valores peridos
 

## IMPUTACIONES -> cuando los valores NA's tienen poca representacion
## INPUTACIONES PARA CUANTITATIVAS
# Imputo todas las cuantitativas, seleccionar el tipo de imputaci?n: media, mediana o aleatorio
summary(datos)
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio")) #ejecutar 2 veces
summary(input) # aqui no deberian aparecer NA's en las cuantitativas

#me pasa algo muy chungo, que a la primera tirada no se me eliminan todos los Nan, solo ocurre a la 3 vez que ejecuto inputar

#INPUTACIONES PARA CUALITATIVAS
# Imputo todas las cualitativas, seleccionar el tipo de imputaci?n: moda o aleatorio
# Si solo se quiere imputar una, variable<-ImputacionCuali(variable,"moda")
summary(datos)
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) ImputacionCuali(x,"aleatorio"))
# A veces se cambia el tipo de factor a character al imputar, asi que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

# Reviso que no queden datos missings
summary(input) #aqui no deberain aparecer ninguna con NA's


#GUARDAMOS NUESTRA BASE DE DATOS DEPURADA PARA HACER EL SIGUIENTE PASO QUE ES CONTRUIR EL MODELO
# Una vez finalizado este proceso, se puede considerar que los datos estan depurados. Los guardamos
saveRDS(cbind(varObjBin,varObjCont,input),"datosTareaDep")
#Se va a llamar datosVinoDep
# este dataset CONTIENE LAS VARIABLES EXPLICATIVAS + LAS VARIABLES OBJETIVO que si recordamos nos las hemos cargado al inicio de este codigo para no usarlas en esta fase de la mineria

# Ver el directorio de trabajo y cambiarlo
getwd() # ver mi directorio actual
setwd("C:\\Users\\uxiat\\Desktop\\MasterBD\\Mineria\\Documentación minería de datos y modelización predictiva - Rosa Espinola-20221111\\codigos_señora_buenos") # por si queremos cambiar el directorio de trabajo
