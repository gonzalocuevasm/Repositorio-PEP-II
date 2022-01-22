library(boot)
library(ggpubr)
library(ez)
library(tidyverse)

#PREGUNTA 1
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
# capitán, comandante y general) califican a los stormtroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presentan diferencias.


datos <- read.csv2(file.choose(),head=TRUE ,sep=";", stringsAsFactors = TRUE  )



#Dado que el Lord Smith nos solicita verificar si existen semejanzas o diferencias en las calificaciones, por lo que
#se considera pertinente una prueba de ANOVA para muestras correlacionadas, puesto que se piden evaluar las medias
#de distintas muestras sobre un individuo.
 
#Hipótesis
#H0: Se establece que las medias para todas las evaluaciones según cada evaluador son iguales.
#HA: Al menos una media  de las evaluaciones por parte de un evaluador es distinta.


alfa <- 0.05


set.seed(123)
datos <- datos %>% select(eval_instructor,eval_capitan,eval_comandante,eval_general)
instancia <- factor(1:nrow(datos))
datos <- datos %>% sample_n(., 50) 
# Llevar data frame a formato largo .
datos <- data.frame(datos,instancia)
datos <- datos %>% pivot_longer (c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general") ,
                                 names_to = "oficiales",values_to = "evaluacion")
datos[["oficiales"]] <- factor(datos[["oficiales"]])



# Comprobción de normalidad .
g <- ggqqplot ( datos , x = "evaluacion", y = "oficiales", color = "oficiales")
g <- g + facet_wrap (~ oficiales )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )


#Para aplicar la prueba ANOVA se deben verificar sus condiciones:
#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
#iguales.
#Como las evaluaciones son numéricas y además definidas en un rango de valores, equivalente a intervalos iguales.
#2. Las mediciones son independientes al interior de cada grupo.
#Se asume que los soldados imperiales (stormtroopers), fueron escogidos aleatoriamente, dada la situación inicial
#en la que se obtuvieron (secuestrados).
#3. Se puede suponer razonablemente que la(s) poblaci n(es) de origen tiende(n) a una distribuci n normal.
#Mediante los gráficos Q-Q se puede visualizar que las 4 muestras poseen una distribución normal.
#4. La matriz de varianzas-covarianzas es esf rica. Como explica Horn (2008, p. 1), esta condici n establece
#que las varianzas entre los diferentes niveles de las medidas repetidas deben ser iguales.
#Esta condición será verificada a continuación junto a la prueba de ANOVA.



cat ("\n\ Resultado de ANOVA para muestras correlacionadas \n\n")

anova <- ezANOVA( datos, dv = evaluacion , within = oficiales ,
                  wid = instancia , return_aov = TRUE )

print(anova)
#El Test de ANOVA  nos da como resultado un p menor al alfa estipulado, por lo que se rechaza la hipótesis
#nula en favor de la hipótesis alternativa, lo cual significa que existe al menos una evaluación dictada por un evaluador
#con media distinta al resto.
 
#Dado que se rechazó la hipótesis nula, se va a implementar una prueba post-hoc, sin considerar que no se cumple
#la cuarta condición.
# Procedimiento post-hoc de Holm.

holm <- pairwise.t.test(datos[["evaluacion"]],datos[["oficiales"]] , p.adj = "holm", paired = TRUE )
cat("\n\Corrección de Holm \n")
print(holm)
#Los resultados de la prueba de holm nos permiten inferir que según la evaluación comendada por el general
#es la que presenta mayor diferencias.
#Finalmente, se concluye que si existe al menos una evaluación con media distinta generada por un alto mando.





#PREGUNTA:
datos <- read.csv2(file.choose(),head=TRUE ,sep=";", stringsAsFactors = TRUE  )
#Justificación de uso de método:
#Como nos solicitan incluir entre 2 a 5 variables predictoras se rechaza la opción de utilizar una regresión lineal,
#y la variable a evaluar corresponde a una dicotómica, se decide implementar una regresión logística.
#Variables:

set.seed (3501)

clones <- factor (datos$es_clon)

# Crear una variable indicadora para clones llamada tipo , con valor 0
# para los que no son clonesy 1 , para los que son clones .
tipo <- rep(1,length(clones))
tipo[ clones == "N"] <- 0

# Reemplazar la variable tipo por la variable indicadora .
datos <- cbind ( datos , tipo )
datos[["es_clon"]] <- NULL

# Cargar los datos .
tipo <- factor(datos$tipo)
datos$tipo <- NULL
datos <- cbind(tipo,datos)

# Crear una variable indicadora para tipo , con valor 0
# para los que no son clones y 1 , para los que son clones .
# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( datos )
n_entrenamiento <- floor (0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo nulo .
nulo <- glm( tipo ~ 1 , family = binomial ( link = "logit") , data = entrenamiento )

# Ajustar modelo completo .
cat ("\n\n")
completo <- glm( tipo ~ . , family = binomial ( link = "logit") ,data = entrenamiento )

# Ajustar modelo con regresión escalonada .
cat (" Modelo con regresión escalonada \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
mejor <- step ( nulo , scope = list ( lower = nulo , upper = completo ) , direction = "both", trace = 0)
print ( summary ( mejor ) )

# Ajustar modelo con la resistencia como predictor .
modelo1 <- glm( tipo ~ resistencia , family = binomial ( link = "logit") , data = entrenamiento )
print ( summary ( modelo1) )
aic_1 <- AIC ( modelo1)


# Ajustar modelo con el resistencia y velocidad como predictor .
modelo2 <- glm( tipo ~ resistencia + velocidad, family = binomial ( link = "logit") , data = entrenamiento )
print ( summary ( modelo2) )
aic_2 <- AIC ( modelo2)

comparacion <- anova ( modelo1 , modelo2, test = "LRT")
cat("\nComparación modelo 1 con modelo 2:\n")
print(comparacion)

#Como AIC del modelo 2 es menor que AIC del mdoelo 1 y p = 2.2e-16 se afirma con un 95% de confianza que 
# el modelo 2 es mejor que el 1

# Ajustar modelo con la resistencia, velocidad y fuerza como predictor .
modelo3 <- glm( tipo ~ resistencia + velocidad + fuerza, family = binomial ( link = "logit") , data = entrenamiento )
print ( summary ( modelo3) )
aic_3 <- AIC ( modelo3)

comparacion <- anova ( modelo2 , modelo3, test = "LRT")
cat("\nComparación modelo 2 con modelo 3:\n")
print(comparacion)

#Como AIC del modelo 3 es menor que AIC del mdoelo 2 y p = 1.11e-05, inferior
# que 0.05 se afirma con un 95% de confianza que el modelo 3 es mejor que el 2


# Generación de data.Frame con los estadísticos para comprobar 
# la influencia de cada observación 
observaciones <- data.frame ( respuesta_predicha = fitted( modelo3 ) ) 
observaciones[[" residuos_estandarizados "]] <- rstandard( modelo3 ) 
observaciones[[" residuos_estudiantizados "]] <- rstudent( modelo3 ) 
observaciones[[" distancia_Cook "]] <- cooks.distance( modelo3 ) 
observaciones[[" dfbeta "]] <- dfbeta( modelo3 ) 
observaciones[[" dffit "]] <- dffits( modelo3 ) 
observaciones[[" apalancamiento "]] <- hatvalues( modelo3 ) 
observaciones[[" covratio "]] <- covratio( modelo3 ) 

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


# Resumen de valores sospechosos . 
sospechosos <- c( atipicos , distanciaCook , apalancamiento ,dfBeta) 
sospechosos <- sort ( unique ( sospechosos ) ) 
cat ("\ nResumen de valores sospechosos :\n") 
cat (" Apalancamiento promedio :", apal_medio , "\n") 

#cat (" Intervalo razón de covarianza : [", inferior , "; ",superior , "]\n\n", sep = "") 

print ( round ( observaciones [ sospechosos , c(" distancia_Cook ", " apalancamiento ") ], 2) ) 


#Verificación de las condiciones 
# Comprobar independencia de los residuos . 
cat (" \nPrueba de Durbin - Watson para autocorrelaciones ") 
cat (" entre errores :\n") 
print ( durbinWatsonTest ( modelo3 ) ) 



# Comprobar la multicolinealidad . 
vifs <- vif ( modelo3 ) 
cat ("\n Verificar la multicolinealidad :\n") 
cat ("- VIFs :\n") 
print ( vifs ) 
cat ("- Tolerancias :\n") 
print (1 / vifs ) 
cat ("- VIF medio :", mean ( vifs ) , "\n") 

# Cumple con  el criterio, todos los VIFS son menores a 5 y ademas, las toleracias 
# son superiores a 0.2 

# Evaluar el modelo 4 con el conjunto de entrenamiento
cat ("\n Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict ( modelo3 , entrenamiento , type = "response")

umbral <- 0.5
preds_e <- sapply ( probs_e , function (p) ifelse ( p >= umbral , "1", "0") )
preds_e <- factor ( preds_e , levels = levels ( datos[["tipo"]]) )

# Se obtiene el modelo roc del modelo 4 con el conjunto de entrenamiento
ROC_e <- roc ( entrenamiento[["tipo"]] , probs_e )
plot(ROC_e)

# Matriz de confusión del conjunto entrenamiento
matriz_e <- confusionMatrix ( preds_e , entrenamiento[["tipo"]])
print ( matriz_e )

# Evaluar el modelo 4 con el conjunto de prueba .
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict ( modelo3 , prueba , type = "response")

preds_p <- sapply( probs_p , function ( p ) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor( preds_p , levels = levels ( datos [["tipo"]]) )

# Se obtiene el modelo roc del modelo 4 con el conjunto de prueba
ROC_p <- roc( prueba[["tipo"]] , probs_p)
plot(ROC_p)

# Matriz de confusión del conjunto de prueba
matriz_p <- confusionMatrix ( preds_p , prueba[["tipo"]])
print(matriz_p)

#Conclusión
#La curva de ROC vista en los datos de entrenamiento y de prueba se aleja bastante
# de la diagonal, indicando que existe una gran precisión, causando que este sea un 
# buen modelo.

#Luego de revisar los datos del modelo, este cumple con las condiciones solicitadas
# adicionalmente, mediante la curva de ROC, se verifica que el modelo es óptimo, pues
# su curva se aleja bastante de la diagonal.
# Además, su matriz de confusión nos indica que existe un 96% de exactitud, superior al
# valor solicitado. Por otro lado, ningun dato presenta una distancia de cook que lo 
# marque como un valor preocupante, como tampoco estos presentan una multicolinealidad 
# severa, pues son inferiores a tres veces el apalancamiento promedio.


#PREGUNTA 3:
#Ejemplo:
#La valoración del gabinete estipulado por el nuevo mandato y el gobierno anterior por parte de la población,
# donde 0 significa que no valora ningún personaje, 1 valora a una persona en el gabinete y así sucesivamente,
#por lo que posee un rango de 0-23, siendo 23 la cantidad de ministerios,
#luego este se divide por la cantidad total (23),
#obteniendo el promedio de valoración por participante para cada gabinete.
#H0: No hay diferencia en la valoración del gabinete escogido entre los dos gobiernos.
#HA: Existe diferencia en la valoración del gabinete actual con el gabinete anterior.

#variables
#   Participante | Gabinete Anterior | Gabinete Actual
#       1                 2/23             13/23
#       .                 .                .
#       .                 .                .
#       n                 i/23             j/23
