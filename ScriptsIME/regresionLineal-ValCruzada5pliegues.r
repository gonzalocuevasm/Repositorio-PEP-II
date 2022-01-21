library ( caret )
# Cargar los datos .
datos <- mtcars

# Crear conjuntos de entrenamiento y prueba .
set.seed (101)
n <- nrow ( datos )
n_entrenamiento <- floor (0.7 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo usando validaci ón cruzada de 5 pliegues .
modelo <- train ( mpg ~ wt , data = entrenamiento , method = "lm", trControl = trainControl ( method = "cv", number = 5) )
print ( summary ( modelo ) )

# Obtener error cuadrado promedio para el conjunto de entrenamiento .
mse_entrenamiento <- modelo$results$RMSE
cat (" MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict ( modelo , prueba )

# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba [["mpg"]] - predicciones
mse_prueba <- mean ( error ** 2)
cat (" MSE para el conjunto de prueba :", mse_prueba )