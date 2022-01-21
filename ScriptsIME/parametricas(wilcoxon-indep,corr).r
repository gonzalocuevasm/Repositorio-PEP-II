#Suma de Rangos de Wilcoxon 

# Ingresar los datos .
a <- c(2.7 , 6.6 , 1.6 , 5.1 , 3.7 , 6.1 , 5.0 , 1.4 , 1.8 , 1.5 , 3.0 , 5.3)
b <- c(5.0 , 1.4 , 5.6 , 4.6 , 6.7 , 2.7 , 1.3 , 6.3 , 3.7 , 1.3 , 6.8)

# Establecer nivel de significaci ón.
alfa <- 0.05

# Hacer la prueba de Mann - Whitney .
prueba <- wilcox.test (a , b , alternative = "two.sided ", conf.level = 1 - alfa )
print ( prueba )


# prueba de rangos con signo de Wilcoxon



#Hipotesis:

#H0: las mismas personas no perciben diferencia en la usabilidad de ambas interfaces.
#HA: las mismas personas consideran que la interfaz A tiene mejor usabilidad que la interfaz B.

# Ingresar los datos .
a <- c(2.9 , 6.1 , 6.7 , 4.7 , 6.4 , 5.7 , 2.7 , 6.9 , 1.7 , 6.4)
b <- c(6.0 , 2.8 , 1.3 , 4.7 , 3.1 , 1.8 , 2.9 , 4.0 , 2.3 , 1.6)

# Establecer nivel de significaci ón.
alfa <- 0.05
# Hacer la prueba de rangos con signo de Wilcoxon .
prueba <- wilcox.test (a , b , alternative = "greater", paired = TRUE , conf.level = 1 - alfa )

print ( prueba )

