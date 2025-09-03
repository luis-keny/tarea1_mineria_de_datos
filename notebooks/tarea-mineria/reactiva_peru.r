#* @Titulo ----
#* @Autores: 
#*    - Luis Lucero
#*    - Dayvis Quispe
#* @Data Reactiva peru 2022: 
#*    https://www.mef.gob.pe/contenidos/archivos-descarga/REACTIVA_Lista_beneficiarios_270422.xlsx
#*  

### 1. Importación y preparación de datos:

# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(dplyr)
library(glmnet)
library(caret)
library(pROC)
getwd() 
setwd("./Analisis multivariado - UPEU")

reactiva_peru2022 <- read_excel("reactiva_peru_2022.xlsx")

# Renombrar la variable en el data.frame original
names(reactiva_peru2022)[names(reactiva_peru2022) == "RUC/DNI"] <- "RUC_O_DNI"
names(reactiva_peru2022)[names(reactiva_peru2022) == "RAZÓN SOCIAL"] <- "RAZoN_SOCIAL"
names(reactiva_peru2022)[names(reactiva_peru2022) == "SECTOR ECONÓMICO"] <- "SECTOR_ECONOMICO"
names(reactiva_peru2022)[names(reactiva_peru2022) == "SALDO INSOLUTO (S/)"] <- "SALDO_INSOLUTO"
names(reactiva_peru2022)[names(reactiva_peru2022) == "COBERTURA DEL SALDO INSOLUTO(S/)"] <- "COBERTURA_SALDO_INSOLUTO"
names(reactiva_peru2022)[names(reactiva_peru2022) == "NOMBRE DE ENTIDAD OTORGANTE DEL CRÉDITO"] <- "ENTIDAD_OTORGANTE_CREDITO"


# Convertir columnas a factores
reactiva_peru2022$RAZoN_SOCIAL <- as.factor(reactiva_peru2022$RAZoN_SOCIAL)
reactiva_peru2022$RUC_O_DNI <- as.factor(reactiva_peru2022$RUC_O_DNI)
reactiva_peru2022$SECTOR_ECONOMICO <- as.factor(reactiva_peru2022$SECTOR_ECONOMICO)
reactiva_peru2022$ENTIDAD_OTORGANTE_CREDITO <- as.factor(reactiva_peru2022$ENTIDAD_OTORGANTE_CREDITO)
reactiva_peru2022$DEPARTAMENTO <- as.factor(reactiva_peru2022$DEPARTAMENTO)
reactiva_peru2022$REPRO <- ifelse(train_data$REPRO == "SI", 1, 0)

str(reactiva_peru2022)

# Calcular la correlación de Spearman las variables numericas
cor(reactiva_peru2022$SALDO_INSOLUTO, reactiva_peru2022$COBERTURA_SALDO_INSOLUTO)



### 2. Analisis descriptivo

str(reactiva_peru2022)

# Crear un grafico circular donde indica el porcentaje de empresas que cuentan con REPRO SI y REPRO NO
# Calcular los porcentajes de REPRO
repro_counts <- reactiva_peru2022 %>%
  count(REPRO) %>%
  mutate(percentage = n / sum(n) * 100)

# Crear gráfico circular con etiquetas de porcentaje
ggplot(repro_counts, aes(x = "", y = percentage, fill = REPRO)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Porcentaje de empresas con REPRO") +
  theme(legend.position = "bottom")


# crear un grafico de barras de la frecuencia de empresas segun región
# Crear gráfico de barras horizontal según región, ordenado de mayor a menor (CANTIDAD)
ggplot(data = reactiva_peru2022, aes(x = reorder(DEPARTAMENTO, table(DEPARTAMENTO)[DEPARTAMENTO]), fill = DEPARTAMENTO)) +
  geom_bar() +
  coord_flip() +
  theme_bw() +
  geom_text(stat = 'count', aes(label = ..count..), hjust = -0.3) +
  labs(title = "Frecuencia de empresas según región",
       x = "Región (Departamento)",
       y = "Frecuencia de empresas") +
  theme(legend.position = "none")

# Calcular las proporciones por departamento (PORCENTAJE)
reactiva_peru2022 %>%
  count(DEPARTAMENTO) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(DEPARTAMENTO, percentage), y = percentage, fill = DEPARTAMENTO)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.3) +
  labs(title = "Porcentaje de empresas según región",
       x = "Región (Departamento)",
       y = "Porcentaje de empresas") +
  theme(legend.position = "none")

# crear un grafico de barras donde se separe segun categoria del sector economico
# Crear gráfico de barras horizontal según categoría del sector económico, ordenado de mayor a menor (FRECUENCIA)
ggplot(data = reactiva_peru2022, aes(x = reorder(SECTOR_ECONOMICO, -table(SECTOR_ECONOMICO)[SECTOR_ECONOMICO]), fill = SECTOR_ECONOMICO)) +
  geom_bar() +
  coord_flip() +
  theme_bw() +
  geom_text(stat = 'count', aes(label = ..count..), hjust = -0.3) +
  labs(title = "Frecuencia de empresas según sector económico",
       x = "Sector Económico",
       y = "Frecuencia de empresas") +
  theme(legend.position = "none")

# Calcular las proporciones por sector económico (PORCENTAJE)
reactiva_peru2022 %>%
  count(SECTOR_ECONOMICO) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(SECTOR_ECONOMICO, percentage), y = percentage, fill = SECTOR_ECONOMICO)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.3) +
  labs(title = "Porcentaje de empresas según sector económico",
       x = "Sector Económico",
       y = "Porcentaje de empresas") +
  theme(legend.position = "none")

# crear un grafico de barras donde y sera la frecuencia de empresas, x seran los sectores y colour seran REPRO
# Crear gráfico de barras horizontal con sectores y color REPRO, ordenado de mayor a menor
ggplot(data = reactiva_peru2022, aes(x = reorder(SECTOR_ECONOMICO, table(SECTOR_ECONOMICO)[SECTOR_ECONOMICO]), fill = REPRO)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  theme_bw() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), hjust = -0.3) +
  labs(title = "Frecuencia de empresas por sector económico y REPRO",
       x = "Sector Económico",
       y = "Frecuencia de empresas",
       fill = "REPRO") +
  theme(legend.position = "bottom")



### 3. Desarrollo del modelo

# Separar las variables necesarias para el modelo
reactiva_peru2022 <- reactiva_peru2022 %>%
  select(`SECTOR_ECONOMICO`, `ENTIDAD_OTORGANTE_CREDITO`, `DEPARTAMENTO`, `SALDO_INSOLUTO`, `REPRO`)


#Dividiendo la data
#set.seed(123)
#train_data <- reactiva_peru2022 %>% sample_frac(0.8)
#test_data <- reactiva_peru2022 %>% setdiff(train_data)

# Crear una columna combinada para muestreo estratificado
reactiva_peru2022$combined <- interaction(reactiva_peru2022$REPRO,reactiva_peru2022$ENTIDAD_OTORGANTE_CREDITO, reactiva_peru2022$DEPARTAMENTO, reactiva_peru2022$SECTOR_ECONOMICO, drop = TRUE)

# Realizar la división estratificada basada en la columna combinada
set.seed(123)
trainIndex <- createDataPartition(reactiva_peru2022$combined, p = 0.8, list = FALSE)
train_data <- reactiva_peru2022[trainIndex, ]
test_data <- reactiva_peru2022[-trainIndex, ]

# Eliminar la columna combinada después de la división
train_data$combined <- NULL
test_data$combined <- NULL

# Matrices de diseño
ames_train_x <- model.matrix(REPRO ~ . - 1, data = train_data)
ames_test_x <- model.matrix(REPRO ~ . - 1, data = test_data)

# Normalizar las matrices de diseño
scaler <- preProcess(ames_train_x, method = c("center", "scale"))
ames_train_x <- predict(scaler, ames_train_x)
ames_test_x <- predict(scaler, ames_test_x)

# Convertir la variable objetivo a numérica
ames_train_y <- train_data$REPRO
ames_test_y <- test_data$REPRO

# Ajustar el modelo de Lasso
ames_lasso <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1,
  family = "binomial"
)

# Graficar el modelo de Lasso
plot(ames_lasso, xvar = "lambda")



# Validación cruzada para seleccionar el mejor valor de lambda
set.seed(123)
ames_lasso_cv <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1,
  family = "binomial"
)

# Graficar los resultados de la validación cruzada
plot(ames_lasso_cv)

# Obtener el mejor valor de lambda
best_lambda <- ames_lasso_cv$lambda.min
best_lambda_1se <- ames_lasso_cv$lambda.1se

# Ajustar el modelo con el mejor lambda
ames_lasso_best <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1,
  family = "binomial",
  lambda = best_lambda
)


plot(ames_lasso_best)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(ames_lasso_best, newx = ames_test_x, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluar el rendimiento del modelo
confusion_matrix <- table(predictions, ames_test_y)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
  