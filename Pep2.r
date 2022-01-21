library ( boot )
library ( ggpubr )
library (ez)
library ( tidyverse )

#PREGUNTA 1
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
# capitán, comandante y general) califican a los shoretroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.

datos <- read.csv2(file.choose(),head=TRUE ,sep=";", stringsAsFactors = TRUE  )



#Dado que el Lord Smith nos solicita verificar si existen semejanzas o diferencias en las calificaciones, por lo que 
#se considera pertinente una prueba de ANOVA para muestras correlacionadas, puesto que se piden evaluar las medias 
#de distintas muestras sobre un individuo.

#Hipótesis
#H0: Se establece que las medias para todas las evaluaciones segun cada evaluador son iguales.
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
#Como las evaluaciones son NUMERICAS y ademas definidas en un rango de valores, equivalente a intervalos iguales. 
#2. 2. Las mediciones son independientes al interior de cada grupo.
#Se asume que los soldados imperiales (stormtroopers), fueron escogidos aleatoriamente, dada la situacio4n inicial
#en la que se obtuvieron (secuestrados).
#3. Se puede suponer razonablemente que la(s) poblaci n(es) de origen tiende(n) a una distribuci n normal.
#Mediante los graficos Q-Q se puede visualizar que las 4 muestras poseen una distribucion normal.
#4. La matriz de varianzas-covarianzas es esf rica. Como explica Horn (2008, p. 1), esta condici n establece
#que las varianzas entre los diferentes niveles de las medidas repetidas deben ser iguales.
#Esta condicio4n sera verificada a continuacion junto a la prueba de ANOVA.


cat ("\n\ Resultado de ANOVA para muestras correlacionadas \n\n")

anova <- ezANOVA( datos, dv = evaluacion , within = oficiales ,
                  wid = instancia , return_aov = TRUE )

print(anova)
#El Test de ANOVA  nos da como resultado un p menor al alfa estipulado, por lo que se rechaza la hipotesis
#nula en favor de la hipotesis alternativa, lo cual significa que existe al menos una evaluacion dictada por un evaluador 
#con media distinta al resto.

#Dado que se rechazo la hipotesis nula, se va a implementar una prueba post-hoc, sin considerar que no se cumple 
#la cuarta condicion. 
# Procedimiento post-hoc de Holm .
holm <- pairwise.t.test(datos[["evaluacion"]],datos[["oficiales"]] , p.adj = "holm", paired = TRUE )
cat("\n\Corrección de Holm \n")
print(holm)
#Los resultados de la prueba de holm nos permiten inferir que segun la evaluacion comendada por el general
#es la que presenra mayor diferencias.
#Finalmente, se concluye que si existe al menos una evualuacion con media distinta generada por un alto mando.

