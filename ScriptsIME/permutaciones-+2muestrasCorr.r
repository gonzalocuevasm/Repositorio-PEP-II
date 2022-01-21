library ( boot )
library ( ggpubr )
library ( ez )
library ( tidyverse )

# Crear el data frame .
Quicksort <- c(11.2 , 22.6 , 23.4 , 23.3 , 21.8 , 40.1)
Bubblesort <- c(15.7 , 29.3 , 30.7 , 30.8 , 29.8 , 50.3)
Mergesort <- c(12.0 , 25.7 , 25.7 , 23.7 , 25.5 , 44.7)
Instancia <- factor (1:6)
datos_anchos <- data.frame ( Instancia , Quicksort , Bubblesort , Mergesort )

datos_largos <- datos_anchos %>% pivot_longer (c("Quicksort", "Bubblesort","Mergesort") ,names_to = "Algoritmo",values_to = "Tiempo")
datos_largos [["Algoritmo"]] <- factor ( datos_largos [["Algoritmo"]])

# Verificar condici ón de normalidad .
g <- ggqqplot ( datos_largos , "Tiempo", facet.by = "Algoritmo",color = "Algoritmo")
print ( g )

# Establecer nivel de significaci ón.
alfa <- 0.01
# Obtener el valor observado , correspondiente al estad í stico F entregado
# por ANOVA para la muestra original .
anova <- ezANOVA ( datos_largos , dv = Tiempo , within = Algoritmo ,wid = Instancia , return_aov = TRUE )

valor_observado <- anova [["ANOVA" ]][["F"]]

# Generar permutaciones .
R = 2999
permutaciones <- list ()
copia_ancha <- data.frame ( datos_anchos )

set.seed (432)

for ( i in 1:R) {
  copia_ancha [, 2:4] <- t( apply ( copia_ancha [ , 2:4] , 1 , sample ) )
  
  copia_larga <- copia_ancha %>% pivot_longer (c("Quicksort", "Bubblesort","Mergesort") ,names_to = "Algoritmo",values_to = "Tiempo")
  copia_larga[["Algoritmo"]] <- factor ( copia_larga [["Algoritmo"]])
  permutaciones <- append ( permutaciones , list ( copia_larga ) )
}

# Generar distribuci ón de estad í sticos F con las permutaciones .
distribucion <- c()

for ( i in 1:R) {
  datos <- as.data.frame ( permutaciones[ i ])
  anova <- ezANOVA ( datos , dv = Tiempo , within = Algoritmo , wid = Instancia ,return_aov = TRUE )
  distribucion <- c( distribucion , anova [["ANOVA"]][["F"]])
}

# Obtener valor p.
p <- ( sum( distribucion > valor_observado ) + 1) / (R + 1)
cat (" ANOVA de una vía para muestras pareadas con permutaciones \n")
cat ("p =", p , "\n\n")

# Aná lisis post -hoc.
# Funci ón para calcular la media de las diferencias para dos columnas de una
# matriz de datos en formato ancho .
media_diferencias <- function ( datos , columna_1 , columna_2) {
  media <- mean ( datos [[columna_1]] - datos [[ columna_2]])
  return ( media )
}
# Funci ón para generar la distribuciones de la diferencia de medias a
# partir de las permutaciones .
distribucion_diferencias <- function ( permutaciones , columna_1 , columna_2) {
  R <- length ( permutaciones )
  distribucion <- c()
  
  for ( i in 1:R) {
    datos <- as.data.frame ( permutaciones [ i ])
    datos <- datos %>% pivot_wider ( names_from = "Algoritmo",values_from = "Tiempo")
    
    diferencia <- media_diferencias ( datos , columna_1 , columna_2)
    distribucion <- c( distribucion , diferencia )
  }
  
  return ( distribucion )
}

if ( p < alfa ) {
  quick <- 2
  bubble <- 3
  merge <- 4
  
  # Calcular diferencias observadas en la muestra original .
  dif_obs_quick_bubble <- media_diferencias ( datos_anchos , quick , bubble )
  dif_obs_quick_merge <- media_diferencias ( datos_anchos , quick , merge )
  dif_obs_bubble_merge <- media_diferencias ( datos_anchos , bubble , merge )
  
  # Generar distribuciones para diferencias entre pares a partir de las
  # permutaciones .
  dif_quick_bubble <- distribucion_diferencias ( permutaciones , quick , bubble )
  dif_quick_merge <- distribucion_diferencias ( permutaciones , quick , merge )
  dif_bubble_merge <- distribucion_diferencias ( permutaciones , bubble , merge )
  
  # Obtener valores p.
  num <- sum(abs( dif_quick_bubble ) > (abs( dif_obs_quick_bubble ) + 1) )
  den <- R + 1
  p_quick_bubble <- num / den
  
  num <- sum(abs( dif_quick_merge ) > abs ( dif_obs_quick_merge ) + 1)
  den <- R + 1
  p_quick_merge <- num / den
  
  num <- sum(abs( dif_bubble_merge ) > abs ( dif_obs_bubble_merge ) + 1)
  den <- R + 1
  p_bubble_merge <- num / den
  cat ("\n\n")
  cat ("Aná lisis post -hoc ( permutaciones ) para la diferencia de las medias \n")
  cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - -\n")
  cat (" Valores p:\n")
  
  cat ( sprintf (" Quicksort - Bubblesort : %.3f\n", p_quick_bubble ) )
  cat ( sprintf (" Quicksort - Mergesort : %.3f\n", p_quick_merge ))
  cat ( sprintf (" Bubblesort - Mergesort : %.3f\n", p_bubble_merge ) )
  cat ("\ nDiferencias observadas :\n")
  cat ( sprintf (" Quicksort - Bubblesort : %.3f\n", dif_obs_quick_bubble ) )
  cat ( sprintf (" Quicksort - Mergesort : %.3f\n", dif_obs_quick_merge ) )
  cat ( sprintf (" Bubblesort - Mergesort : %.3f\n", dif_obs_bubble_merge ) )
}