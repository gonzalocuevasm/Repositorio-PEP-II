#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 10, desarrollo de sala 4
#
#            Integrantes:                        RUT:              
#        - Carlos Castillo                   20.201.274-4
#        - Gonzalo Cuevas                    19.721.859-2 
#        - Felipe Gomez			     19.914.686-6
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library ( ggpubr )

#-------------------------------------------------------------------------------
#                                 Pregunta 1
#-------------------------------------------------------------------------------
# Dos art�culos reportan el porcentaje de acierto alcanzado por dos algoritmos 
# de clasificaci�n, espec�ficamente el Bayes ingenuo (C4) y el Bayes ingenuo 
# oculto (C2), en diferentes conjuntos de prueba disponibles en el UCI Machine 
# Learning Repository. �Es uno de los algoritmo mejor que el otro?

texto <-("
 Dataset C2 Dataset C4
 'tae' 43.82 'credit' 85.67
 'anneal' 97.44 'monks' 61.68
 'pasture-production' 85.27 'soybean' 91.52
 'contact-lenses' 67.77 'segment' 90.74
 'primary-tumor' 47.52 'squash-unstored' 61.11
 'kr-s-kp' 91.90 'mushroom' 95.27
 'solar-flare-C' 87.68 'page-blocks' 92.95
 'monks1' 99.44 'grub-damage' 47.23
 'white-clover' 78.73 'cmc' 50.49
 'ecoli' 79.48 'waveform' 79.30
 'nursery' 93.72 'postoperatie' 66.11
 'squash-stored' 57.44 -- --
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")
#-------------------------------------------------------------------------------
# Respuesta:  
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 1") 
cat("\n----------------------------------------------------------------------\n")

# Para responder dicha pregunta, se debe diferenciar los datos entregados.

# En primera instancia, se puede observar que los datos son independientes,  
# cumpliendo la condicion de independencia para poder usar la prueba t-student.
# Pero no se puede usar dicha prueba, debido a dos razones.
# - El primero es que no se podr�a asumir que la escala que presenta los datos 
#   sean de intervalos iguales.
# - Y el segundo es que las distribucion de los datos no se asemejan a la normal,
#   en la cual se puede verificarse en los siguientes hist�gramas.

# Histograma del algoritmo Bayes ingenuo oculto (C2)
g1 <- gghistogram ( datos ,
                    x = "C2",
                    bins = 10,
                    add = "mean",
                    xlab = "Porcentaje de acierto",
                    ylab = "Frecuencia",
                    title = "Histograma del algoritmo C2",
                    color = "black",
                    fill = "blue")
print(g1)

# Histograma del algoritmo Bayes ingenuo (C4)
g2 <- gghistogram ( datos ,
                    x = "C4",
                    bins = 10,
                    add = "mean",
                    xlab = "Porcentaje de acierto",
                    ylab = "Frecuencia",
                    title = "Histograma del algoritmo C4",
                    color = "black",
                    fill = "blue")
print(g2)

# Debido a lo anterior, no nos queda opci�n que usar la prueba no param�trica 
# de Wilcoxon-Mann-Whitney, considerando las siguientes hip�tesis:

# H0: No hay diferencia en el porcentaje de acierto en ambos algoritmos.
# HA: Si hay diferencia en el porcentaje de acierto en ambos algoritmos.
        
# Entonces, con un nivel de significaci�n del 1%, se har� la prueba mediante la
# funci�n wilcox.test de R:

alfa <- 0.01
prueba <- wilcox.test(datos$C2 , 
                      datos$C4 , 
                      alternative = "two.sided", 
                      conf.level = 1 - alfa )
cat("\nPrueba de suma de rangos de Wilcoxon mediante wilcox.test:\n")
print ( prueba )

# Entonces, de acuerdo a los resultados, el valor de p obtenido es 0.6947, 
# siendo demasiado alto para el nivel de significancia definido (alfa = 0.01),
# por lo que se falla al rechazar la hip�tesis nula a favor de la alternativa.
# Por lo tanto, con un 99% de confianza, se concluye que no existe diferencia 
# en el porcentaje de acierto en ambos algoritmos, por lo que ning�n algoritmo
# es mejor que otro.

#------------------------------------------------------------------------------
#                                 Pregunta 2
#------------------------------------------------------------------------------
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las 
# lecturas dadas) en donde un estudio o experimento, relacionado con el alza 
# que han experimentado las tasas de inter�s de los cr�ditos en Chile, necesite 
# utilizar una prueba de los rangos con signo de Wilcoxon, debido a problemas 
# con la escala de la variable dependiente en estudio. Indiqu� cu�les ser�an 
# las variables/niveles involucrados en su ejemplo y las hip�tesis nula y 
# alternativa a contrastar. 
#------------------------------------------------------------------------------
# Respuesta:
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 2") 
cat("\n----------------------------------------------------------------------\n")




#------------------------------------------------------------------------------
#                                 Pregunta 3
#------------------------------------------------------------------------------
# El siguiente texto muestra porcentaje de acierto alcanzado por tres 
# algoritmos de clasificaci�n en diferentes conjuntos de prueba disponibles 
# en el UCI Machine Learning Repository. Los algoritmos corresponden a 
# C3: averaged one-dependence estimator (AODE), 
# C6: locally weighted naive-Bayes y 
# C7: random forest. 
# �Existe un algoritmo mejor o peor que los otros?

texto <- ("
 Dataset C3 C6 C7
 'eucalyptus' 58.15 58.96 58.84
 'pendigits' 97.26 94.25 95.11
 'primary-tumor' 46.93 48.99 37.75
 'iris' 92.11 91.44 92.77
 'optdigits' 96.34 93.64 91.24
 'waveform' 84.36 83.06 79.12
 'yeast' 57.18 56.92 55.70
 'glass' 73.27 75.13 72.77
 'solar-flare-X' 97.28 93.85 95.43
 'sonar' 80.70 80.23 77.80
 'hepatitis' 83.23 81.94 80.69
 'page-blocks' 96.39 93.59 96.41
 'solar-flare-C' 87.98 87.36 85.49
 'pima-diabetes' 74.45 74.19 72.11
 'credit' 84.51 84.66 82.77
 'solar-flare-m' 87.36 86.43 84.90
")
datos <- read.table(textConnection(texto), header = TRUE)
#------------------------------------------------------------------------------
# Respuesta:
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 3") 
cat("\n----------------------------------------------------------------------\n")





#------------------------------------------------------------------------------
#                                 Pregunta 4
#------------------------------------------------------------------------------
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las 
# lecturas dadas) en donde un estudio o experimento, relacionado con el alza 
# que han experimentado las tasas de inter�s de los cr�ditos en Chile, 
# necesite utilizar una prueba de Kruskal-Wallis, debido a problemas con la 
# normalidad de los datos. Indiqu� cu�les ser�an las variables/niveles 
# involucrados en su ejemplo y las hip�tesis nula y alternativa a contrastar. 
#------------------------------------------------------------------------------
# Respuesta:
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 4") 
cat("\n----------------------------------------------------------------------\n")
