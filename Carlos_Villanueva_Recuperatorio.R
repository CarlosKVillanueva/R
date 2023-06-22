library(rpart)
library(readr)

# Cargar el conjunto de datos desde la URL
df <-  read_csv("C:/Users/carlo/OneDrive/Escritorio/Recuperatorio_R/mushrooms.csv")
#View(mushrooms)


# Ignorar las columnas que tienen valores todos iguales
df <- df[, sapply(df, function(x) length(unique(x))) > 1]

# Seleccionar 15 atributos al azar para predecir la comestibilidad
set.seed(42)
random_features <- sample(colnames(df)[-1], 15)
selected_df <- df[, c("type", random_features)]

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(42)
test_indices <- sample(1:nrow(selected_df), 55)
train_df <- selected_df[-test_indices, ]
test_df <- selected_df[test_indices, ]

# Construir el árbol de decisión
control <- rpart.control(cp = 0.001)
tree <- rpart(target ~ ., data = train_df, method = "class", control = control)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(tree, newdata = test_df, type = "type")

# Calcular la precisión del modelo
accuracy <- sum(predictions == test_df$type) / nrow(test_df)
print(paste("Precisión del modelo:", accuracy))

# Contar las predicciones incorrectas sobre la comestibilidad de los hongos
incorrect_predictions <- test_df[predictions != test_df$class, ]
incorrect_count <- nrow(incorrect_predictions)
print(paste("Número de predicciones incorrectas:", incorrect_count))
