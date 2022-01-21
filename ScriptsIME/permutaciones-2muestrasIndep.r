library ( ggpubr )

set.seed (432)
# Función para calcular la diferencia de medias .
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .
# - FUN: funci ón del estad í stico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
calcular_diferencia <- function ( muestra_1 , muestra_2 , FUN ) {
  diferencia <- FUN ( muestra_1) - FUN ( muestra_2)
  return ( diferencia )
}

# Funci ón para hacer una permutaci ón y calcular el estad í stico
# de inter és.
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .
# - FUN: funci ón del estad í stico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function ( muestra_1 , muestra_2 , FUN ) {
  n_1 <- length ( muestra_1)
  n_2 <- length ( muestra_2)
  
  # Hacer la permutaci ón.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2 ,replace = FALSE )
  
  # Asignar elementos a los dos grupos .
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [ n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias .
  return ( calcular_diferencia ( permutacion_1 , permutacion_2 , FUN ) )}

# Funci ón para calcular el valor p.
# Argumentos :
# - distribucion : distribuci ón nula del estad í stico de inter és.
# - valor _ observado : valor del estad í stico de inter és para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipó tesis alternativa . "two. sided " para
# hipó tesis bilateral , " greater " o " less " para hip ó tesis unilaterales .
# Valor :
# - el valorp calculado

calcular_valor_p <- function ( distribucion , valor_observado ,repeticiones , alternative ) {
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

# Funci ón para graficar una distribuci ón.
# Argumentos :
# - distribucion : distribuci ón nula del estad í stico de inter és.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function ( distribucion , ...) {
  observaciones <- data.frame ( distribucion )
  
  histograma <- gghistogram ( observaciones , x = "distribucion",
                              xlab = " Estad í stico de inter és",
                              ylab = " Frecuencia ", ...)
  qq <- ggqqplot ( observaciones , x = "distribucion", ...)
  
  # Crear una ú nica figura con todos los grá ficos de dispersi ón.
  figura <- ggarrange ( histograma , qq , ncol = 2 , nrow = 1)
  print ( figura )
}
# Funci ón para hacer la prueba de permutaciones .
# Argumentos :
# - muestra _1 , muestra _2: vectores num é ricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : funci ón del estad í stico E para el que se calcula la diferencia .
# - alternative : tipo de hipó tesis alternativa . "two. sided " para
# hip ó tesis bilateral , " greater " o " less " para hip ó tesis unilaterales .
# - plot : si es TRUE , construye el grá fico de la distribuci ón generada .
# - ...: otros argumentos a ser entregados a graficar _ distribucion .
contrastar_hipotesis_permutaciones <- function ( muestra_1 , muestra_2 ,repeticiones , FUN ,alternative , plot , ...) {
  cat (" Prueba de permutaciones \n\n")
  cat (" Hipó tesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia ( muestra_1 , muestra_2 , FUN )
  cat (" Valor observado :", observado , "\n")
  distribucion <- rep( NA , repeticiones )
  
  for ( i in 1: repeticiones ) {
    distribucion [ i ] <- permutar ( muestra_1 , muestra_2 , FUN )
  }
  
  if( plot ) {
    graficar_distribucion ( distribucion , ...)
  }
  valor_p <- calcular_valor_p ( distribucion , observado , repeticiones ,"two.sided")
  
  cat ("Valor p:", valor_p , "\n\n")
}

# Crear muestras iniciales .
a <- c(5.4 , 4.7 , 6.3 , 2.9 , 5.9 , 5.1 , 2.1 , 6.2 , 1.6 , 6.7 , 3.0 , 3.3 ,5.0 , 4.1 , 3.3 , 3.4 , 1.2 , 3.8 , 5.8 , 4.2)

b <- c(4.0 , 4.1 , 4.3 , 4.3 , 4.3 , 4.2 , 4.3 , 4.3 , 4.4 , 4.1 , 4.3 , 4.0)

# Hacer pruebas de permutaciones para la media y la varianza .
R = 5999

contrastar_hipotesis_permutaciones (a , b , repeticiones = R, FUN = mean ,alternative = "two.sided", plot = TRUE ,color = "blue", fill = "blue")
contrastar_hipotesis_permutaciones (a , b , repeticiones = R, FUN = var ,alternative = "two.sided", plot = FALSE )
