#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 13, desarrollo de sala 4
#
#            Integrantes:                        RUT:              
#        - Carlos Castillo                   20.201.274-4
#        - Gonzalo Cuevas                    19.721.859-2
#        - Daniel Jara                       20.113.716-0
#-------------------------------------------------------------------------------
library(pROC) 
library(tidyr) 
library (dplyr) 
library (readr) 
library ( ggpubr ) 
library (leaps) 
library ( car )  
library ( caret )

#-------------------------------------------------------------------------------
# Se obtienen los datos del ejercicio anterior
datos <- read.csv(file.choose(), head = TRUE, sep=";", encoding = "UTF-8")

# Obtención de la variable IMC
IMC <- datos$Weight/((datos$Height*10^-2)^2)

# Obtención de la variable dicotómica EN dentro de datos
EN <- ifelse ( IMC >= 25 , 1, 0)
datos <- cbind ( EN , datos )

# Una vez obtenido la variable dicotómica, ahora se realizan los siguientes
# pasos.
# 1. Recordar las ocho posibles variables predictoras seleccionadas de forma 
#    aleatoria en el ejercicio anterior.

# 2. Seleccionar, de las otras variables, una que el equipo considere que podría 
#    ser útil para predecir la clase EN, justificando bien esta selección.

# 3. Usando el entorno R, construir un modelo de regresión logística con el 
#    predictor seleccionado en el paso anterior.

# 4. Usando herramientas para la exploración de modelos del entorno R, buscar 
#    entre dos y cinco predictores de entre las variables seleccionadas al azar, 
#    recordadas en el punto 3, para agregar al modelo obtenido en el paso. 

# 5. Evaluar los modelos y "arreglarlos" en caso de que tengan algún problema 
#    con las condiciones que deben cumplir.

# 6. Evaluar el poder predictivo de los modelos en datos no utilizados para 
#    construirlo (o utilizando validación). 
#-------------------------------------------------------------------------------                                                                                       cruzada) y revisar las respectivas curvas ROC.
cat("\n----------------------------------------------------------------------")
cat("\n                            DESARROLLO") 

cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 1") 
cat("\n----------------------------------------------------------------------\n")
# Las 8 variables predictores escogidas en el ejercicio anterior son los 
# siguientes:

cat("\n Variables predictoras escogidas:")
cat("\n 1- Chest.depth")
cat("\n 2- Hip.Girth")
cat("\n 3- Navel.Girth")
cat("\n 4- Shoulder.Girth")
cat("\n 5- Chest.Girth")
cat("\n 6- Bicep.Girth")
cat("\n 7- Chest.diameter")
cat("\n 8- Thigh.Girth")

cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 2") 
cat("\n----------------------------------------------------------------------\n")
# La variable predictora escogida es la variable Waist.Girth 
# (grosor a la altura de la cintura), ya que se considera que el grosor de 
# la cintura puede ser la variable mas representativa de las restantes, 
# pues esta medida es comunmente utilizada para determinar el porcentaje 
# de grasa en las personas ademas de utilizarse como un indicador para la 
# dimensión corporal de las mismas.
# Se puede verificar dicho candidato al comparar los modelos logísticos ajustados 
# completo y nulo, obtenidos a continuación mediante la función add1(), 
# efectivamente presenta el coeficiente AIC de Waist.Girth es menor que el 
# de las demás variables. 

#Se establece semilla utilizada en el EP anterior.
set.seed(1274)
# Separaración de conjuntos de entrenamiento y prueba .
datos$EN <- factor ( datos$EN )
n <- nrow ( datos )
n_entrenamiento <- floor (0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajuste de modelo nulo .
nulo <- glm( EN ~ 1 , family = binomial ( link = "logit") , data = entrenamiento )

# Ajuste de modelo completo .
completo <- glm( EN ~ . , family = binomial ( link = "logit") ,
                 data = entrenamiento )

cat("\n Evaluación de modelos completo y nulo mediante el AIC:\n")
print ( add1 ( nulo , scope = completo ) )
cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 3") 
cat("\n----------------------------------------------------------------------\n")

# Modelo de regresíon Logística con la variable Waist.Girth
modelo_WaistGirth <- glm( EN ~  Waist.Girth, family = binomial ( link = "logit") ,
                 data = entrenamiento )
cat("\n Modelo regresíon Logística con la variable Waist.Girth")
print ( summary ( modelo_WaistGirth ) )

cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 4") 
cat("\n----------------------------------------------------------------------\n")
#Pruebas de modelos 
# Predictores: Waist.Girth + Hip.Girth
modelo2 <- glm( EN~ Waist.Girth + Hip.Girth , family = binomial ( link = "logit") ,
     data = entrenamiento )

AIC1 <- AIC ( modelo_WaistGirth)
AIC2 <- AIC ( modelo2)
comparacion <- anova ( modelo_WaistGirth , modelo2, test = "LRT")
cat("\nComparación modelo 1 con modelo 2:\n")
print(comparacion)

# Se tiene que el AIC del modelo 2 es menor que el del modelo 1. al evaluar las 
# diferencias significativas con ANOVA, se obtiene un p = 1.722e-13, inferior a 
# 0.05, por lo que se concluye con un 95% de confianza, que el modelo2 es mejor 
# que el modelo1.

# Predictores: Waist.Girth + Hip.Girth + Thigh.Girth

modelo3 <- update( modelo2 , . ~ . + Thigh.Girth )
AIC3 <- AIC ( modelo3)
comparacion <- anova ( modelo2 , modelo3, test = "LRT")
cat("\nComparación modelo 2 con modelo 3:\n")
print(comparacion)

# Similar al caso anterior, AIC del modelo 3 es menor que el del modelo 2, con 
# ANOVA, p = 1.752e-10, por ende, se tiene un 95% de confianza en que el modelo 
# 3 es mejor que el modelo 2.

# Predictores: Waist.Girth + Hip.Girth + Thigh.Girth + Bicep.Girth
modelo4 <- update( modelo3 , . ~ . + Bicep.Girth )
AIC4 <- AIC ( modelo4)
comparacion <- anova ( modelo3 , modelo4)
cat("\nComparación modelo 3 con modelo 4:\n")
print(comparacion)

# Similar al caso anterior, AIC del modelo 4 tambien es menor que el del modelo 3, con 
# ANOVA, p = 0.02446, por ende, se tiene un 95% de confianza en que el modelo 
# 4 es mejor que el modelo 3.

cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 5") 
cat("\n----------------------------------------------------------------------\n")
#Requsisitos:

#Verificación de Condiciones para el modelo Final: 

# Generación de data.Frame con los estadísticos para comprobar 
# la influencia de cada observación 
observaciones <- data.frame ( respuesta_predicha = fitted ( modelo4 ) ) 
observaciones [[" residuos_estandarizados "]] <- rstandard ( modelo4 ) 
observaciones [[" residuos_estudiantizados "]] <- rstudent ( modelo4 ) 
observaciones [[" distancia_Cook "]] <- cooks.distance ( modelo4 ) 
observaciones [[" dfbeta "]] <- dfbeta ( modelo4 ) 
observaciones [[" dffit "]] <- dffits ( modelo4 ) 
observaciones [[" apalancamiento "]] <- hatvalues ( modelo4 ) 
observaciones [[" covratio "]] <- covratio ( modelo4 ) 

cat (" Identificación de valores atípicos :\n") 
# Observaciones con residuos estandarizados fuera del 95 % esperado . 
atipicos <- which ( abs(observaciones [[" residuos_estandarizados "]]) > 1.96) 

cat ("- Residuos estandarizados fuera del 95 % esperado :",atipicos , "\n") 

# Observaciones con distancia de Cook mayor a uno. 
distanciaCook <- which ( observaciones [[" distancia_Cook "]] > 1) 
cat ("- Residuos con una distancia de Cook alta :",distanciaCook , "\n") 

# Observaciones con apalancamiento mayor igual al doble del 
# apalancamiento promedio . 
apal_medio <- ( ncol ( datos ) + 1) / nrow ( datos ) 
apalancamiento <- which ( observaciones [[" apalancamiento "]] > 2 * apal_medio ) 
cat ("- Residuos con apalancamiento fuera de rango :",apalancamiento , "\n") 


# Observaciones con DFBeta mayor o igual a 1. 
dfBeta <- which ( apply ( observaciones [[" dfbeta "]] >= 1, 1 , any) ) 
names( dfBeta ) <- NULL 
cat ("- Residuos con DFBeta >= 1:",dfBeta , "\n") 



#cat ("- Residuos con razón de covarianza fuera de rango :",covarianza , "\n") 

# Resumen de valores sospechosos . 
sospechosos <- c( atipicos , distanciaCook , apalancamiento ,dfBeta) 
sospechosos <- sort ( unique ( sospechosos ) ) 
cat ("\ nResumen de valores sospechosos :\n") 
cat (" Apalancamiento promedio :", apal_medio , "\n") 

#cat (" Intervalo razón de covarianza : [", inferior , "; ",superior , "]\n\n", sep = "") 

print ( round ( observaciones [ sospechosos , c(" distancia_Cook ", " apalancamiento ") ], 2) ) 

# La mayoría de los datos detectados como conflictivos presentan ciertas 
# condiciones que permiten no eliminarlos de la lista, pues en alguno con 
# con alto apalancamiento, su distancia de cook es baja o su nivel de 
# covarianza se encuentra dentro del rango óptimo. 

#Verificación de las condiciones 
# Comprobar independencia de los residuos . 
cat (" \nPrueba de Durbin - Watson para autocorrelaciones ") 
cat (" entre errores :\n") 
print ( durbinWatsonTest ( modelo4 ) ) 

# Con la prueba de Durbin-Watson se obtiene un p = 0.068, superior al nivel de significancia 
# lo que nos permite afirmar con un 95% de confianza que las residuos no estan correlacionados 


# Comprobar la multicolinealidad . 
vifs <- vif ( modelo4 ) 
cat ("\n Verificar la multicolinealidad :\n") 
cat ("- VIFs :\n") 
print ( vifs ) 
cat ("- Tolerancias :\n") 
print (1 / vifs ) 
cat ("- VIF medio :", mean ( vifs ) , "\n") 

# Cumple con  el criterio, todos los VIFS son menores a 5 y ademas, las toleracias 
# son superiores a 0.2 


cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 6") 
cat("\n----------------------------------------------------------------------\n")

# Evaluar el modelo 4 con el conjunto de entrenamiento
cat ("\n Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict ( modelo4 , entrenamiento , type = "response")

umbral <- 0.5
preds_e <- sapply ( probs_e , function (p) ifelse ( p >= umbral , "1", "0") )
preds_e <- factor ( preds_e , levels = levels ( datos[["EN"]]) )

# Se obtiene el modelo roc del modelo 4 con el conjunto de entrenamiento
ROC_e <- roc ( entrenamiento[["EN"]] , probs_e )
plot(ROC_e)

# Matriz de confusión del conjunto entrenamiento
matriz_e <- confusionMatrix ( preds_e , entrenamiento[["EN"]])
print ( matriz_e )

# Evaluar el modelo 4 con el conjunto de prueba .
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict ( modelo4 , prueba , type = "response")

preds_p <- sapply ( probs_p , function ( p ) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor ( preds_p , levels = levels ( datos [["EN"]]) )

# Se obtiene el modelo roc del modelo 4 con el conjunto de prueba
ROC_p <- roc ( prueba[["EN"]] , probs_p )
plot ( ROC_p )

# Matriz de confusión del conjunto de prueba
matriz_p <- confusionMatrix ( preds_p , prueba[["EN"]])
print ( matriz_p )

# Conclusión:
# La curva de ROC vista en los datos de entrenamiento y de prueba se aleja bastante
# de la diagonal, indicando que existe una gran precisión, causando que este sea un 
# buen modelo.
# Del mismo modo, la matriz de confusión para el conjunto de entrenamiento y de 
# puebas nos indica que, estos poseen un nivel de exactitud del 91% y 82% respectivamente,
# a su vez, la sensitividad de cada uno es del 95% y 92% y su especifidad del 80% y 60%
# indicando que  existe la posibilidad de que el conjunto de pruebas sea demasiado pequeño
# para poder comprobar los valores correctamente, pues se detecta que existe una diferencia
# en la exactitud de ambos conjuntos.