#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 12, desarrollo de sala 4
#
#            Integrantes:                        RUT:              
#        - Carlos Castillo                   20.201.274-4
#        - Gonzalo Cuevas                    19.721.859-2
#        - Daniel Jara                       20.113.716-0
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Librerías a utilizar
library(tidyr)
library (dplyr)
library (readr)
library ( ggpubr )
library (leaps)
library ( car ) 

#-------------------------------------------------------------------------------
#                                 Pregunta 
#-------------------------------------------------------------------------------
# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres
# (Heinz et al., 2003). Estas mediciones están disponibles en el archivo
# Body-EP12.csv que acompaña a este enunciado. El estudio incluyó nueve 
# mediciones del esqueleto (ocho diámetros y una profundidad de hueso a
# hueso) y doce mediciones de grosor(circunferencias) que incluyen el tejido




#Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso, de acuerdo con las 
#siguientes instrucciones:
  
#1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito 
#   verificador) del integrante de menor edad del equipo.

#2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar).

#3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

#4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
#   Peso, justificando bien esta selección.

#5. Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el paso 
#   anterior.

#6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de 
#   entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple 
#   obtenido en el paso 5.

#7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben 
#   cumplir.

#8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación 
#   cruzada).

datos <- read.csv(file.choose(), head = TRUE, sep=";", encoding = "UTF-8")


#-------------------------------------------------------------------------------
# Respuesta:  
cat("\n----------------------------------------------------------------------")
cat("\n                            DESARROLLO") 
cat("\n----------------------------------------------------------------------\n")

cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 1 Y 2") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 1 y 2 #######
# Semilla a utilizar
set.seed(1274)
muestra <- filter(datos, Gender == "0") %>% sample_n(., 50) 

cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 3") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 3 #######
# Se escogen 8 posibles variables predictoras de forma aleatoria
#Variables al azar
# 1- Chest.depth
# 2- Hip.Girth
# 3- Navel.Girth
# 4- Shoulder.Girth
# 5- Chest.Girth
# 6- Bicep.Girth
# 7- Chest.diameter
# 8- Thigh.Girth

cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 4") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 4 #######
# Variable predictora Waist.Girth (grosor a la altura de la cintura):
# Se selecciona este predictor pues se considera que el grosor de la cintura
# puede ser la variable mas representativa de las restantes, pues esta medida
# es comunmente utilizada para determinar el porcentaje de grasa en las personas
# ademas de utilizarse como un indicador para la dimensión corporal
# de las mismas.
# Si adicionalmente probamos con la funcion add1(), esta también nos indica que 
# el mejor candidato como predictor, de los restantes es Waist.Girth
nulo <- lm( Weight ~ 1 , data = muestra )
completo <- lm( Weight ~ . , data = muestra )
print ( add1 ( nulo , scope = completo ) )


cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 5") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 5 #######
# Se construye un modelo de regresión lineal simple con el predictor seleccionado anteriormente.
modelo1 <- lm( Weight~Waist.Girth , data = muestra )
print ( summary ( modelo1 ) )

#Comparación muestra y modelo.
p <- ggscatter (muestra ,
                x = "Waist.Girth",
                y = "Weight",
                color = "blue", 
                fill = "blue",
                xlab = " Grosor Cintura [cm] ",
                ylab = " Peso [Kg]")
p <- p + geom_smooth( method = lm , se = FALSE , color = "orange")

cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 6") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 6 #######
#Pruebas de modelos 
# Predictores: Waist.Girth + Hip.Girth
modelo2 <- lm( Weight~ Waist.Girth + Hip.Girth , data = muestra )

AIC1 <- AIC ( modelo1)
AIC2 <- AIC ( modelo2)
comparacion <- anova ( modelo1 , modelo2)
print(comparacion)

# Se tiene que el AIC del modelo 2 es menor que el del modelo 1. al evaluar las 
# diferencias significativas con ANOVA, se obtiene un p = 3.843e-07, inferior a 
# 0.05, por lo que se concluye con un 95% de confianza, que el modelo2 es mejor 
# que el modelo1.

# Predictores: Waist.Girth + Hip.Girth + Shoulder.Girth
modelo3 <- update( modelo2 , . ~ . + Shoulder.Girth )
AIC3 <- AIC ( modelo3)
comparacion <- anova ( modelo2 , modelo3)
print(comparacion)

# Similar al caso anterior, AIC del modelo 3 es menor que el del modelo 2, con 
# ANOVA, p = 0.02063, por ende, se tiene un 95% de confianza en que el modelo 
# 3 es mejor que el modelo 2

# Predictores: Waist.Girth + Hip.Girth + Shoulder.Girth + Navel.Girth
modelo4 <- update( modelo3 , . ~ . + Navel.Girth )
AIC4 <- AIC ( modelo4)
comparacion <- anova ( modelo3 , modelo4)
print(comparacion)

# AIC del modelo 4 es ligeramente mayor que el del modelo 3, al utilizar
# ANOVA, nos entrega un p = 0.3371, por lo que se tiene un 95% de confianza
# en que no hay diferencias entre el modelo 3 y 4, por lo que el modelo 3
# sigue siendo mejor.

#
cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 7") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 7 #######
#Verificación de Condiciones para el modelo Final:

# Reducción de los datos, contemplando solo los predictores
predictores <- names ( coef ( modelo3 ) ) [ -1]
datosAux <-muestra[, c( predictores , "Weight")]

# Generación de data.Frame con los estadísticos para comprobar
# la influencia de cada observación
observaciones <- data.frame ( respuesta_predicha = fitted ( modelo3 ) )
observaciones [[" residuos_estandarizados "]] <- rstandard ( modelo3 )
observaciones [[" residuos_estudiantizados "]] <- rstudent ( modelo3 )
observaciones [[" distancia_Cook "]] <- cooks.distance ( modelo3 )
observaciones [[" dfbeta "]] <- dfbeta ( modelo3 )
observaciones [[" dffit "]] <- dffits ( modelo3 )
observaciones [[" apalancamiento "]] <- hatvalues ( modelo3 )
observaciones [[" covratio "]] <- covratio ( modelo3 )

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

# Observaciones con razón de covarianza fuera de rango .
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3 * apal_medio
covarianza <- which ( observaciones [[" covratio "]] < inferior | observaciones [[" covratio "]] > superior )

cat ("- Residuos con razón de covarianza fuera de rango :",covarianza , "\n")

# Resumen de valores sospechosos .
sospechosos <- c( atipicos , distanciaCook , apalancamiento ,dfBeta , covarianza )
sospechosos <- sort ( unique ( sospechosos ) )
cat ("\ nResumen de valores sospechosos :\n")
cat (" Apalancamiento promedio :", apal_medio , "\n")

cat (" Intervalo razón de covarianza : [", inferior , "; ",superior , "]\n\n", sep = "")

print ( round ( observaciones [ sospechosos , c(" distancia_Cook ", " apalancamiento "," covratio ") ], 3) )

# La mayoría de los datos detectados como conflictivos presentan ciertas
# condiciones que permiten no eliminarlos de la lista, pues en alguno con
# con alto apalancamiento, su distancia de cook es baja o su nivel de
# covarianza se encuentra dentro del rango óptimo.

#Verificación de las condiciones
# Comprobar independencia de los residuos .
cat (" Prueba de Durbin - Watson para autocorrelaciones ")
cat (" entre errores :\n")
print ( durbinWatsonTest ( modelo3 ) )

# Con la prueba de Durbin-Watson se obtiene un p = 0.938, superior al nivel de significancia
# lo que nos permite afirmar con un 95% de confianza que las residuos no estan correlacionados


# Comprobar normalidad de los residuos .
cat ("\ nPrueba de normalidad para los residuos :\n")
print ( shapiro.test( modelo3$residuals) )
# Con la prueba de Shapiro se obtiene un p = 0.1663, superior al nivel de signifiancia
# lo que nos permite afirmar con un 95% de confianza que los residuos siguen una distribucion normal.


# Comprobar homocedasticidad de los residuos .
cat (" Prueba de homocedasticidad para los residuos :\n")
print ( ncvTest ( modelo3 ) )
# Con la prueba de homocedasticidad se obtiene un p = 0.12583, superior al nivel de significancia,
# lo que nos permite afirmar con un 95% de confianza que los residuos cumplen con el supuesto de Homocedasticidad.


# Comprobar la multicolinealidad .
vifs <- vif ( modelo3 )
cat ("\ nVerificar la multicolinealidad :\n")
cat ("- VIFs :\n")
print ( vifs )
cat ("- Tolerancias :\n")
print (1 / vifs )
cat ("- VIF medio :", mean ( vifs ) , "\n")

# Cumple con  el criterio, todos los VIFS son menores a 5 y ademas, las toleracias
# son superiores a 0.2


cat("\n----------------------------------------------------------------------")
cat("\n                            PASO 8") 
cat("\n----------------------------------------------------------------------\n")
# ####### Paso 8 #######

#Validación cruzada
n <- 50
n_entrenamiento <- floor(0.8 * n )
muestraCruzada <- sample.int( n = n, size = n_entrenamiento , replace = FALSE )
entrenamiento <- muestra [ muestraCruzada , ]
prueba <- muestra [ - muestraCruzada , ]

modeloEntrenamiento <- lm( Weight~ Waist.Girth + Hip.Girth + Shoulder.Girth , data = entrenamiento )

mse_entrenamiento <- mean ( modeloEntrenamiento$residuals**2)
predicciones <- predict ( modeloEntrenamiento , prueba )
error <- prueba[["Weight"]] - predicciones
mse_prueba <- mean( error ** 2)

print(mse_prueba)
print(mse_entrenamiento)

# Conclusión:
# El error cuadrático medio para el entrenamiento y la prueba son distintos, sin embargo,
# el modelo cumple con todos los estadísticos para ser considerado uno válido, por lo que
# este puede ser generalizable.
# Debido a lo anterior, sería necesario intentar con otra semilla que genere una nueva
# muestra para volver a comprobar los datos.


