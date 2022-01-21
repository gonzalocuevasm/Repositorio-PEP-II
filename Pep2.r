library ( tidyverse )
library ( ggpubr )
library ( ez )

#PREGUNTA 1
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores (instructor,
# capitán, comandante y general) califican a los shoretroopers son similares, por lo que le ha solicitado estudiar si existen
# diferencias significativas en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten diferencias.

datos <- read.csv(file.choose(), head = TRUE, sep=";", encoding = "UTF-8")



#Dado que el Lord Smith nos solicita verificar si existen semejanzas o diferencias en las calificaciones, por lo que 
#se considera pertinente una prueba de ANOVA para muestras correlacionadas, puesto que se piden evaluar las medias 
#de distintas muestras sobre un individuo.




datos <- datos %>% select(eval_instructor,eval_capitan,eval_comandante,eval_general)
muestra <- datos %>% sample_n(., 50) 

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer (c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general") ,
                                    names_to = "oficiales",values_to = "evaluacion")
datos [["oficiales"]] <- factor(datos[["oficiales"]])
datos [["instancia"]] <- factor(1: nrow ( datos ) )


set.seed(1234)



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
#3. Se puede suponer razonablemente que la(s) poblaci n(es) de origen sigue(n) una distribuci n normal.

#4. La matriz de varianzas-covarianzas es esf rica. Como explica Horn (2008, p. 1), esta condici n establece
#que las varianzas entre los diferentes niveles de las medidas repetidas deben ser iguales.
#Esta condicio4n sera verificada a continuacion junto a la prueba de ANOVA.
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")

prueba2 <- ezANOVA ( data = datos , dv = tiempo , within = algoritmo ,wid = instancia , return _ aov = TRUE )