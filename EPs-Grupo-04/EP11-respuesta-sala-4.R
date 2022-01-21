#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                     Enunciado 11, desarrollo de sala 4
#
#            Integrantes:                        RUT:              
#        - Carlos Castillo                   20.201.274-4
#        - Gonzalo Cuevas                    19.721.859-2
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggpubr)
library(ggplot2)

# Carga de los datos
poblacion <- read_excel(file.choose())

#-------------------------------------------------------------------------------
#                       FUNCIONES A CONSIDERAR
#-------------------------------------------------------------------------------
# funcion para calcular la diferencia de medias .
# Argumentos :
# - muestra_1 , muestra_2: vectores num�ricos con las muestras a comparar .
# - FUN: funcion del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.
calcular_diferencia <- function ( muestra_1 , muestra_2 , FUN ) {
  diferencia <- FUN ( muestra_1) - FUN ( muestra_2)
  return ( diferencia )
}


# funcion para hacer una permutaci�n y calcular el estadístico
# de inter�s.
# Argumentos :
# - muestra_1 , muestra_2: vectores num�ricos con las muestras a comparar .
# - FUN: funcion del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.
permutar <- function ( muestra_1 , muestra_2 , FUN ) {
  n_1 <- length ( muestra_1)
  n_2 <- length ( muestra_2)
  
  # Hacer la permutaci�n.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2 ,
                         replace = FALSE )
  
  # Asignar elementos a los dos grupos .
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [ n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias .
  return ( calcular_diferencia ( permutacion_1 , permutacion_2 , FUN ) )
}


# funcion para calcular el valor p.
# Argumentos :
# - distribucion : distribuci�n nula del estadístico de inter�s.
# - valor_observado : valor del estadístico de inter�s para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two.sided " para
# hipótesis bilateral , " greater " o " less " para hipótesis unilaterales .
# Valor :
# - el valor p calculado .
calcular_valor_p <- function ( distribucion , valor_observado ,
                               repeticiones , alternative ) {
  if( alternative == "two.sided") {
    numerador <- sum(abs( distribucion ) > abs ( valor_observado ) ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if( alternative == "greater") {
    numerador <- sum( distribucion > valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum( distribucion < valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return ( valor_p )
}


# funcion para graficar una distribuci�n.
# Argumentos :
# - distribucion : distribuci�n nula del estadístico de inter�s.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function ( distribucion , ...) {
  observaciones <- data.frame ( distribucion )
  
  histograma <- gghistogram ( observaciones , x = "distribucion",
                              xlab = " estadístico de inter�s",
                              ylab = "Frecuencia", ...)
  
  qq <- ggqqplot ( observaciones , x = "distribucion", ...)
  
  # Crear una �nica figura con todos los gr�ficos de dispersi�n.
  figura <- ggarrange ( histograma , qq , ncol = 2 , nrow = 1)
  print ( figura )
}


# funcion para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores num�ricos con las muestras a comparar .

# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : funcion del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o " less " para hipótesis unilaterales .
# - plot : si es TRUE , construye el gr�fico de la distribuci�n generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion .
contrastar_hipotesis_permutaciones <- function ( muestra_1 , muestra_2 ,
                                                 repeticiones , FUN ,
                                                 alternative , plot , ...) {
  cat ("Prueba de permutaciones \n\n")
  cat ("hipótesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia ( muestra_1 , muestra_2 , FUN )
  cat ("Valor observado :", observado , "\n")
  
  distribucion <- rep( NA , repeticiones )
  
  for ( i in 1: repeticiones ) {
    distribucion [ i ] <- permutar ( muestra_1 , muestra_2 , FUN )
  }
  
  if( plot ) {
    graficar_distribucion ( distribucion , ...)
  }
  
  valor_p <- calcular_valor_p ( distribucion , observado , repeticiones ,
                                "two.sided")
  
  cat ("Valor p:", valor_p , "\n\n")
}

#-------------------------------------------------------------------------------
#                                 Pregunta 1
#-------------------------------------------------------------------------------
# Propongan una pregunta de investigacion original, que involucre la comparaci�n 
# de las medias de dos grupos independientes (m�s abajo se dan unos ejemplos). 
# Fijando una semilla propia, seleccionen una muestra aleatoria de hogares 
# (250 < n < 500) y respondan la pregunta propuesta utilizando una simulaci�n
# Monte Carlo.

#-------------------------------------------------------------------------------
# Respuesta:  
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 1") 
cat("\n----------------------------------------------------------------------\n")
# Semilla a utilizar
set.seed(300)

# Una investigacion realizada por el Banco Central de Chile, pretende determinar
# si existen diferencias en el ingreso per cápita (ytotcorh / numper) de las
# personas solteras (ecivil) de una region del norte (Región de Tarapacá) 
# y una region del sur (Región de la Araucanía).

# Para ello, se obtuvo el ingreso de todas las personas solteras, 
# ya sea hombre o mujer, que pertenezcan a ambas regiones.
personas_solteras <- poblacion %>% filter(ecivil == "Soltero(a)")

tarapaca <- personas_solteras %>% filter(region == "Región de Tarapacá")
araucania <- personas_solteras %>% filter(region == "Región de La Araucanía")  

ingreso_s_t <- tarapaca %>% select(ytotcorh) 
ingreso_s_a <- araucania %>% select(ytotcorh) 

# Despu�s, se obtuvo una muestra aleatoria con los ingresos de 50 personas 
# solteras en la Region de Tarapacá y 45 personas solteras en la 
# Región la Araucanía.
muestra_ingreso_s_t <- sample(ingreso_s_t$ytotcorh, 50)
muestra_ingreso_s_a <- sample(ingreso_s_a$ytotcorh, 45)

# Para el respectivo estudio del Banco Central, se compar� la media de los ingresos
# de ambas regiones usando una prueba de permutaci�nes con P = 4999 repeticiones,
# y un nivel de significacion alfa de 0.05.

# Las hipótesis a contrastar son las siguientes:
# H0: No existen diferencias en el promedio del ingreso per cápita de las personas
#     solteras entre las Regiones de Tarapacá (u_a) y la Araucanía (u_b).
# H1: Si existen diferencias en el promedio del ingreso per cápita de las personas
#     solteras entre las Regiones de Tarapacá (u_a) y la Araucanía (u_b).

cat("\nhipótesis bilateral contrastada:")
cat("\nu_a-u_b = 0")
cat("\nu_a-u_b > 0  U  u_a-u_b < 0\n\n")

# Una vez contrastadas las hipótesis, se realiza la prueba de permutaciones
# mediante el procedimiento de la simulaci�n de Monte Carlo para la media.
R = 4999
contrastar_hipotesis_permutaciones (muestra_ingreso_s_t, 
                                    muestra_ingreso_s_a, 
                                    repeticiones = R, 
                                    FUN = mean,
                                    alternative = "two.sided", 
                                    plot = TRUE ,
                                    color = "blue", 
                                    fill = "blue")

# -De acuerdo a los resultados, en primera instancia se observa que el hist�grama
# obtenido se asemeja a la distribuci�n normal, con una leve asimetr�a en la 
# distribuci�n de los datos.
# -En segunda instancia, se obtuvo una diferencia observada de 560979.1 
# (siendo la diferencia de las media muestrales de los ingresos entre ambas 
# regiones), por lo que se sugiere que la region de Tarapacá posee mejores 
# ingresos per cápita en personas solteras.
# -Finalmente, el valor p obtenido es 0.0002, la cual es menor al nivel de 
# significancia obtenido, por lo que se rechaza la hipótesis nula a favor de la
# alternativa.
# Por lo tanto, con un 95% de confianza, se concluye que el promedio de los
# ingresos per cápita para las personas solteras var�a tanto en la Región de 
# Tarapacá como en la Región de la Araucanía, teniendo un mayor ingreso en las
# personas solteras que viven en la Región de Tarapacá que en la Región de la
# Araucanía.
#------------------------------------------------------------------------------
#                                 Pregunta 2
#------------------------------------------------------------------------------
# Propongan una pregunta de investigacion original, que involucre la comparaci�n 
# de las medias de m�s de dos grupos independientes (m�s abajo se dan unos 
# ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra 
# aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio acad�mico, aplique un an�lisis 
# post-hoc con bootstrapping aunque este no sea necesario
#------------------------------------------------------------------------------
# Respuesta:
cat("\n----------------------------------------------------------------------")
cat("\n                            PREGUNTA 2") 
cat("\n----------------------------------------------------------------------\n")
#edicion
#edicion2

library (simpleboot)
library (boot)

# Un estudio pretende comparar la cantidad de personas casadas en tres de las 
# regiones del sur de chile,(Region del maule,Bio-Bio, Chile) con el fin de 
# determinar si existe alguna region que contenga mas personas casadas que otra.
# La cantidad de personas casadas en la region del maule, bio bio y nuble es similar
#

# Un estudio pretende determianr si existe diferencia entre los sueldos de las
# mujeres casadas de entre 20 a 45 años en cuatro regiones de chile,siendo estas: la region de 
# antofagasta, valparaiso, metropolitana y del maule

set.seed(450)



mujeres <- poblacion %>% filter(sexo == "Mujer"& ecivil == "Casado(a)" & edad >= 20 & edad <=50)
antofagasta <- mujeres %>% filter(region == "Región de Antofagasta")
valparaiso <- mujeres %>% filter(region == "Región de Valparaíso")
metropolitana <- mujeres %>% filter(region == "Región Metropolitana de Santiago")
maule <- mujeres %>% filter(region == "Región del Maule")

ingresoAnt <- sample(antofagas4$ytotcorh, 51)
ingresoValp <- sample(valparaiso$ytotcorh, 45)
ingresoMet <- sample(metropolitana$ytotcorh, 60)
ingresoMaul <- sample(maule$ytotcorh, 38)

