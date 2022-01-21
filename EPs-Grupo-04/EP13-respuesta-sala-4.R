#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 13, desarrollo de sala 4
#
#            Integrantes:                        RUT:              
#        - Carlos Castillo                   20.201.274-4
#        - Gonzalo Cuevas                    19.721.859-2
#        - Daniel Jara                       20.113.716-0
#-------------------------------------------------------------------------------
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

# Separaración de conjuntos de entrenamiento y prueba .
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

# Modelo de regresión logística con la variable Waist.Girth
modelo_WaistGirth <- glm( EN ~  Waist.Girth, family = binomial ( link = "logit") ,
                 data = entrenamiento )
cat("\n Modelo regresión logística con la variable Waist.Girth")
print ( summary ( modelo_WaistGirth ) )

cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 4") 
cat("\n----------------------------------------------------------------------\n")
#Pruebas de modelos 
# Predictores: Waist.Girth + Hip.Girth
modelo2 <- glm( EM~ Waist.Girth + Hip.Girth , data = muestra )

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


cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 5") 
cat("\n----------------------------------------------------------------------\n")
cat("\n----------------------------------------------------------------------")
cat("\n                            Paso 6") 
cat("\n----------------------------------------------------------------------\n")