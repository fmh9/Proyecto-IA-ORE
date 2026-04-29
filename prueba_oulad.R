library(glmnet)
library(tidyverse)

# 2. Cargar los datos
df <- read.csv("/home/alfons/Repos/Proyecto-IA-ORE/bases de datos/oulad_optimizado_final.csv")

# 4. Crear la matriz de diseño con TODAS las interacciones posibles
# La fórmula ( . )^2 genera automáticamente las combinaciones A:B
# model.matrix convierte todo a una matriz numérica que entiende glmnet
x <- model.matrix(final_result ~ (.)^2, data = df_modelo)[,-1]
y <- df_modelo$final_result

# 5. Ajustar el modelo Lasso mediante Validación Cruzada (CV)
# alpha = 1 indica que es Lasso (penalización para selección de variables)
set.seed(42)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# 6. Extraer los coeficientes del mejor modelo (lambda mínimo)
# El modelo selecciona automáticamente las interacciones más importantes
coeficientes <- coef(cv_lasso, s = "lambda.min")

# 7. Limpiar y mostrar solo los resultados significativos (coeficiente != 0)
coef_df <- as.data.frame(as.matrix(coeficientes))
colnames(coef_df) <- "Coeficiente"
variables_importantes <- coef_df %>% 
  filter(Coeficiente != 0) %>% 
  arrange(desc(abs(Coeficiente)))

print("--- VARIABLES E INTERACCIONES SELECCIONADAS POR LASSO ---")
print(variables_importantes)

# 8. Visualización de la importancia
plot(cv_lasso) # Este gráfico muestra cómo Lasso va eliminando variables
