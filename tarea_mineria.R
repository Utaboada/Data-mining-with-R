# Cargo las funciones que voy a utilizar despu?s
source("FuncionesRosa.R")

# Cargo las librerias que me van a hacer falta sino las tengo ya cargadas
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

library(glmnet)# Cargo las librerias 

library(questionr)

library(readxl)



# 1. Introduccion al bojetivo del problema y las variables implicadas
# 2. Importacion del conjunto de datos y asignacion de los tipos de variables
datos <- read_excel("C:\\Users\\uxiat\\Desktop\\MasterBD\\Mineria\\Documentación minería de datos y modelización predictiva - Rosa Espinola-20221111\\Tarea\\DatosImpuestos_Tarea.xlsx")
# Voy a tomar como variables objetivo ActEconom_Cuali y ActEconom

datos <- datos[,-(5:9)]


# 3. Analisis descriptivo del conjunto de datos
#Comprobamos el tipo de variable asignado a cada una
str(datos) 
#No todas las categoricas estan como factores

# Indico los factores (ActEconom_Cuali, Actividad_Municipio y TamaÃ±o)
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
freq(datos$Vivienda_Grande)
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

# tratamiento de valores atipicos. que serÃ¡ pasarlos a missing
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
setwd("C:\\Users\\uxiat\\Desktop\\MasterBD\\Mineria\\Documentación minería de datos y modelización predictiva - Rosa Espinola-20221111\\Tarea\\datosTareaDep") # por si queremos cambiar el directorio de trabajo


################################################  REGRESION LINEAL #############################3
# Parto de los datos sin at?picos ni ausentes guardados
datos<-readRDS("C:\\Users\\uxiat\\Desktop\\MasterBD\\Mineria\\Documentación minería de datos y modelización predictiva - Rosa Espinola-20221111\\Tarea\\datosTareaDep")
varObjCont<-datos$varObjCont
varObjBin<-datos$varObjBin
View(datos)
input<-datos[,-(1:2)]

View(input)

#Obtengo la importancia de las variables. Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes
# Los gr.Kramer nos dicen la correlacion entre las variables y las variables objetivo
graficoVcramer(input,varObjBin)
graficoVcramer(input,varObjCont)

#Veo gr?ficamente el efecto de dos variables cualitativas sobre la binaria
mosaico_targetbinaria(input$Actividad_Municipio,varObjBin,"Actividad_municipio") #esta no influye

mosaico_targetbinaria(input$Tamano,varObjBin,"TamaÃ±o") #esta s? influye


barras_targetbinaria(input$Actividad_Municipio,varObjBin,"Actividad_municipio")
barras_targetbinaria(input$Tamano,varObjBin,"TamaÃ±o")

#Vemos graficamente el efecto de dos variables cuantitativas sobre la binaria
summary(input)
boxplot_targetbinaria(input$Edad_18_65,varObjBin,"Edad_18_65")
boxplot_targetbinaria(input$Locales,varObjBin,"Local")

hist_targetbinaria(input$Edad_18_65,varObjBin,"Edad_18_65")
hist_targetbinaria(input$Acidez,varObjBin,"Acidez")


#Todas las variables num?ricas frente a la objetivo continua
graficoCorrelacion(varObjCont,input) #Nos fijamos en la forma de las l?neas rojas (si hay muchas variables num?ricas, tarda un poco)
corrplot(cor(cbind(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")

#Busco las mejores transformaciones para las variables num?ricas con respesto a los dos tipos de variables
input_cont<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjCont))
input_bin<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjBin))

saveRDS(data.frame(input_bin,varObjBin),"todo_bin")

saveRDS(data.frame(input_cont,varObjCont),"todo_cont")


## Comienzo con la regresi?n lineal
todo<-data.frame(input,varObjCont)

#Obtengo la partici?n
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) #En test hay bastante diferencia, seguramente sobren variables

# Nos fijamos en la importancia de las variables. Podemos sacar un gr?fico que muestra lo que se pierde en R2 en train al quitarlas del modelo
modelEffectSizes(modelo1)
barplot(sort(modelEffectSizes(modelo1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

#Pruebo un modelo con menos variables. Recuerdo el grafico de Cramer
graficoVcramer(todo,varObjCont) #Pruebo con las mas importantes
modelo2<-lm(varObjCont~CCAA+DeudaSaldada+Locales+Vivienda_Grande+Tamano+Actividad_Municipio+Mayor_65+Salario_4+Salario_1+DeudaFinanc,data=data_train)
modelEffectSizes(modelo2)
summary(modelo2)
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 

modelEffectSizes(modelo2)
barplot(sort(modelEffectSizes(modelo2)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

#Modelo solo con las 3 variabales mas correlacionadas del modelo 1 (R2 muy baja 0.34)
modelox<-lm(varObjCont~CCAA+Vivienda_Grande+DeudaSaldada,data=data_train)
summary(modelox)
Rsq(modelox,"varObjCont",data_train)
Rsq(modelox,"varObjCont",data_test)

#Pruebo un modelo con menos variables, basandome en la importancia de las variables
# le quitamos variables al modelo 2 para ver si puede mejorar o mantenerse la r2 pero con menos variables
modelo3<-lm(varObjCont~CCAA+DeudaSaldada+Locales+Vivienda_Grande+Tamano+Actividad_Municipio+Habit_Vivienda,data=data_train)
summary(modelo3)
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 

modelEffectSizes(modelo3)
barplot(sort(modelEffectSizes(modelo3)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

# las interacciones causabam un numero muy elevado de facotores y no me parecia buen modelo
modelo4<-lm(varObjCont~CCAA+DeudaSaldada+Locales+Vivienda_Grande+Tamano,data=data_train)
summary(modelo4)
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test) 

modelo5<-lm(varObjCont~CCAA+Vivienda_Grande+Actividad_Municipio+DeudaSaldada,data=data_train)
summary(modelo5)
Rsq(modelo5,"varObjCont",data_train)
Rsq(modelo5,"varObjCont",data_test) 


#Validaciones cruzadas para ver que modelo es mejor
modelo1VC <- train(formula(modelo1),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo2VC <- train(formula(modelo2),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo3VC <- train(formula(modelo3),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo4VC <- train(formula(modelo4),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo5VC <- train(formula(modelo5),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)
#de toda la info solo nos quedamos con resample que guarda el error de todos los cruces de todos los tests de cada modelo
results<-data.frame(rbind(modelo1VC$resample,modelo2VC$resample,modelo3VC$resample,modelo4VC$resample,modelo5VC$resample),modelo=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100)))
boxplot(Rsquared~modelo,data=results)
aggregate(Rsquared~modelo, data = results, mean) 
aggregate(Rsquared~modelo, data = results, sd) 

#numero de parametros de cada modelo

length(coef(modelo1));length(coef(modelo2));length(coef(modelo3));length(coef(modelo4));length(coef(modelo5))

# ELECCION DE MODELO
  #si bien es cierto que el modelo 1 tiene mayor r2 y teniendo en cuenta aue  la variabilidad de todos los modelos es
#muy similar, nos fijamos en el numero de parametros para tomar la decision final.
#para este caso el modelo 5 parece el mejor ya que tiene un 4factores menos que el modelo 3 (segundo mejor) y un r2 muy similar
# el modelo 4 ya tiene un 4 r2 considerablemente mas bajo que el 4 y con el mismo numero de factores


# Vemos los coeficientes del modelo ganador
coef(modelo3)
coef(modelo5)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 


Rsq(modelo5,"varObjCont",data_train)
Rsq(modelo5,"varObjCont",data_test) 


modelEffectSizes(modelo3)
modelEffectSizes(modelo5)

summary(modelo5)

barplot(sort(modelEffectSizes(modelo3)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")
#getwd() # ver mi directorio actual
#setwd("C:\\Users\\uxiat\\Desktop\\MasterBD\\Mineria\\DocumentaciÃ³n minerÃ­a de datos y modelizaciÃ³n predictiva - Rosa Espinola-20221111\\Tarea") # por si queremos cambiar el directorio de trabajo











###################### .................REGRESION LOGISTICA................ ############################
###################### .....................................................#############################

todo<-readRDS("todo_bin")

#veo el reparto original (sin transformaciones ni interacciones). Compruebo que la variable objetivo tome valor 1 para el evento y 0 para el no evento
freq(todo$varObjBin) 

#Hago la particion para sweparar los datos originales de entrenamiento y test
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train <- todo[trainIndex,c(1:23,39)]#quito las variables transformadas por el momento
data_test <- todo[-trainIndex,c(1:23,39)]#quedo con las variables sin trasnformar y la objetivo

#pruebo un primer modelo sin las transformadas
#hacemos la regresion logistica para los datos de entrenamiento
modeloInicial<-glm(varObjBin~.,data=data_train,family=binomial)

summary(modeloInicial)
#hay variables que no son significativas y por tanto no van a influir en la variable respuesta
# con el summary no nos sale el valor de r2 por tanto no podemos estimar la calidad del modelo con estos datos
# interpretar un parametro asociado a var.continua y otro a una var. categorica
    # var.continua




pseudoR2(modeloInicial,data_train,"varObjBin") #pseudor2 similar a 0.2 es equivalente a un r2 de 0.7 asi que es un buen pseudor2
pseudoR2(modeloInicial,data_test,"varObjBin")
#es normal que el pseudor2 baje un poco en los datos de prueba (justo lo contrario que en la lineal)
#  es normal xq porque estamos midiendo el error con datos que no se han usado para estimar el valor.
modeloInicial$rank #numero de aprametros de este modelo
# 43 no nos interesa tener tantisimos ademas vimos que hay variables no significativas

#fijandome en la significaci?n de las variables,el modelo con las variables m?s significativas queda
modelo2<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Vivienda_Grande+Tamano+Salario_2+Actividad_Municipio+Habit_Vivienda,data=data_train,family=binomial)

summary(modelo2)
pseudoR2(modelo2,data_train,"varObjBin")#es un poquito peor que el anterior, pero el n. de parametros es casi la mitad
pseudoR2(modelo2,data_test,"varObjBin")
modelo2$rank

impVariablesLog(modelo2,"varObjBin") #quizas sobren las 2 ultimas salario_2 y vivienda_grande

#Miro el gr?fico V de Cramer para ver las variables m?s importantes
graficoVcramer(todo[,c(1:23,39)],todo$varObjBin) 

modelo3<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Tamano+Actividad_Municipio+Habit_Vivienda,data=data_train,family=binomial)
summary(modelo3)
pseudoR2(modelo3,data_train,"varObjBin")#mejor que el 2 porque tiene menos aprametros y no disminuye pseudor2
pseudoR2(modelo3,data_test,"varObjBin")
modelo3$rank

#Eliminamos las variables que no son significativas del modelo anterior. que perece ser la actividad municipio
#parece que actividad municipio no es significativa

modelo3_bis<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Tamano+Habit_Vivienda,data=data_train,family=binomial)
summary(modelo3_bis)
pseudoR2(modelo3_bis,data_train,"varObjBin")#baja un poco r2 pero al menos todos los parametros son significativos
pseudoR2(modelo3_bis,data_test,"varObjBin")
modelo3_bis$rank  

#Pruebo alguna interaccion sobre el modelo 3_bis que parece el mejor
modelo4<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Tamano+Habit_Vivienda+Tamano:Habit_Vivienda,data=data_train,family=binomial)
summary(modelo4)
pseudoR2(modelo4,data_train,"varObjBin")
pseudoR2(modelo4,data_test,"varObjBin")
modelo4$rank  #no es muy viable xq la pseudor2 es parecida al 4 pero aumentan hasta 25 los factores


modelo5<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Tamano+Habit_Vivienda+CCAA:Locales,data=data_train,family=binomial)
pseudoR2(modelo5,data_train,"varObjBin")
pseudoR2(modelo5,data_test,"varObjBin")
modelo5$rank  #si es cierto que aumenta un poco pseudor2 pero se van a 40 los facores
summary(modelo5) # por no mencionar que hay muchos fatores muy poco significativos


modelo6<-glm(varObjBin~CCAA+DeudaSaldada+Locales+Tamano+Habit_Vivienda+Tamano:DeudaSaldada,data=data_train,family=binomial)
# No es significativa esta interacci?n
summary(modelo6)
pseudoR2(modelo6,data_train,"varObjBin")
pseudoR2(modelo6,data_test,"varObjBin")
modelo6$rank #aumenta pseudor2 un poco pero hay 25 factores (aunque la mayoria significativos)

#Validacion cruzada repetida para elegir entre todos
auxVarObj<-todo$varObjBin
todo$varObjBin<-make.names(todo$varObjBin) #formateo la variable objetivo para que funcione el codigo
total<-c()
modelos<-sapply(list(modeloInicial,modelo2,modelo3,modelo3_bis,modelo4,modelo5,modelo6),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = todo,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
boxplot(roc~modelo,data=total,main="Area bajo la curva ROC") #el 3 es peor, los otros parecidos
aggregate(roc~modelo, data = total, mean) # media 
aggregate(roc~modelo, data = total, sd) #desviacion tipica de todos los modelos

# en vista solo de estos valores, todos los modelos presentan la misma area bajo la curva roc
# podriamos descartar 4,5 y 6 porque es ligeramente menor (pero veamos primero el numero de parametros )
#y todos presentan una variabilidad muy similar tambien (desviacion tipica)
# por tanato para concluir con que modelo es mejor, debemos ver el numero de parametros
#recupero la variable objetivo en su formato
todo$varObjBin<-auxVarObj

#miro el numero de parametros

modelo1$rank
modelo2$rank 
modelo3$rank
modelo4$rank #Tiene menos par?metros y funciona pr?cticamente igual
modelo5$rank
modelo6$rank
#modelos 1 y 5 descartados xq tienen muchismos factores
 
# el modelo 4 tiene una roc 0.03 veces menor que el siguiente modelo con mejor roc y parametros
# que es el 2. El 2 tiene tiene un factor mas que el 4 pero mejor roc
# entre 4 y 6 estan empatados en factores pero ambos un roc menor que el 2
# asi que voy a escoger el modelo 2 como ganador
## BUscamos el mejor punto de corte


# COMPROBAMOS LAS MEDIDAS DE ERROR
# PARA disntintos puntos de corte para ver cual es mejor
# los puntos de corte son necesarios para crear la matriz de confusion
#probamos dos
sensEspCorte(modelo2,data_test,"varObjBin",0.5,"1")
sensEspCorte(modelo2,data_test,"varObjBin",0.75,"1")
# en este caso de ejemplo seria mejor el 0.75 
## generamos una rejilla de puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo2,data_test,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
#representamos el indice de Youden para cada punto de corte
plot(rejilla$posiblesCortes,rejilla$Youden)
# nos quedaremos con el punto de corte que maximice el indice de Youden
# lo mismo pero para Accuracy
plot(rejilla$posiblesCortes,rejilla$Accuracy)
# cogeremos punto de corte que maximice la accuracy
# lo vemos numericamente
rejilla$posiblesCortes[which.max(rejilla$Youden)]  # 0.34
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]  # 0.48
# son 2 puntos bastante diferentes por lo que vamos a evaluar las medidas en ambos puntos de corte y ver cual nos interesa mas

#El resultado es 0.34 para youden y 0.48 para Accuracy
#Los comparamos
sensEspCorte(modelo2,data_test,"varObjBin",0.34,"1")
sensEspCorte(modelo2,data_test,"varObjBin",0.48,"1")
# viendo ambos, elegiriamos el punto 0.48 porque nos maximiza la Accuracy
# tambien nos interesa mas que acierte en los unos que en los ceros (specificity)
# aunque acierta mas "unos"  (sensitivity) el 0.34
# por ello, en vista de que la accuracy es muy similar, y que en el 0.48 acierta mas unos 
#me quedo con el 0.34

# Vemos las variables m?s importantes del modelo ganador
impVariablesLog(modelo2,"varObjBin") 

# Vemos los coeficientes del modelo ganador
coef(modelo2)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo2,data_train,"varObjBin")
pseudoR2(modelo2,data_test,"varObjBin") #baja pseudor2 en test que es bueni
#es poca la diferencia, por lo que el modelo se puede considerar robusto

roc(data_train$varObjBin, predict(modelo5,data_train,type = "response"))
roc(data_test$varObjBin, predict(modelo5,data_test,type = "response"))
#area bajo la curva roc no exageradamente buena pero es bastante proxima a 1
# tambi?n es poca la diferencia en el ?rea bajo la curva roc y para el punto de corte por tanto parece un modelo robusto

# por ultimo comprobamos que los datos de entramiento y de los tests no hayan cambiado mucho
sensEspCorte(modelo2,data_train,"varObjBin",0.34,"1") 
sensEspCorte(modelo2,data_test,"varObjBin",0.34,"1")

summary(modelo5)






