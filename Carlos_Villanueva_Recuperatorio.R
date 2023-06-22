library(readr)
mushrooms <- read_csv("C:/Users/carlo/OneDrive/Escritorio/Recuperatorio_R/mushrooms.csv")

# Paso 2: Ignorar columnas con valores iguales
df <- df[, !sapply(df, function(x) all(x == x[1]))]

# Paso 3: Selección aleatoria de 15 atributos para predecir la comestibilidad
set.seed(123)
random_columns <- sample(names(df), 15)
df_subset <- df[random_columns]

# Paso 4: Conjunto de testing
set.seed(456)
testing_rows <- sample(1:nrow(df), 55)
df_testing <- df_subset[testing_rows, ]

# Paso 5: Conjunto de entrenamiento
df_training <- df_subset[-testing_rows, ]

# Convertir df_training a data.frame
df_training <- as.data.frame(df_training)

# Paso 6: Contar predicciones incorrectas
library(rpart)
# Verificar el nombre exacto de la columna objetivo en tu dataframe
target_column <- "type"

model <- rpart(as.formula(paste(target_column, "~ .")), data = df_training)
predicted <- predict(model, newdata = df_testing, type = "type")
actual <- df[testing_rows, target_column]
incorrect_predictions <- sum(predicted != actual)

# Resultado
incorrect_predictions