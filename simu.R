# Cargar librerías necesarias
library(randtests)
library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
library(car)

# Definir ruta del archivo de Excel
ruta_excel <- "E:\\Programas de R\\simu_final.xlsx"

# Leer datos desde las hojas y rangos específicos
PaTiempo <- read_excel(ruta_excel, sheet = "Hoja1", range = "H1:H13")
MitasTiempo <- read_excel(ruta_excel, sheet = "Hoja1", range = "K1:K18")
SPapas <- read_excel(ruta_excel, sheet = "Hoja1", range = "I1:I13")
SPalomitas <- read_excel(ruta_excel, sheet = "Hoja1", range = "L1:L18")
Producto <- read_excel(ruta_excel, sheet = "Hoja1", range = "B1:B31")

# Imprimir los datos leídos para su revisión
print(PaTiempo, n = Inf)
print(MitasTiempo, n = Inf)
print(SPapas, n = Inf)
print(SPalomitas, n = Inf)
print(Producto, n = Inf)

# Crear diagramas de caja
ggplot(data.frame(Valor = PaTiempo[[1]]), aes(y = Valor)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Diagrama de Caja y Bigote para Tiempo de llegada Papas", x = "Tiempo", y = "Valor") +
  theme_minimal()

ggplot(data.frame(Valor = MitasTiempo[[1]]), aes(y = Valor)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Diagrama de Caja y Bigote para Tiempo de llegada Palomitas", x = "Tiempo", y = "Valor") +
  theme_minimal()

ggplot(data.frame(Valor = SPapas[[1]]), aes(y = Valor)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Diagrama de Caja y Bigote para Tiempo de Servicio Papas", x = "Tiempo", y = "Valor") +
  theme_minimal()

ggplot(data.frame(Valor = SPalomitas[[1]]), aes(y = Valor)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Diagrama de Caja y Bigote para Tiempo de Servicio Palomitas", x = "Tiempo", y = "Valor") +
  theme_minimal()

# Gráfico de barras para la variable Producto
ggplot(data.frame(Producto = Producto[[1]]), aes(x = factor(Producto), fill = factor(Producto))) +
  geom_bar() +
  labs(title = "Frecuencia de Valores de Producto", x = "Valor de Producto", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"), name = "Producto")

# Pruebas de rachas
runs.test(PaTiempo[[1]])
runs.test(MitasTiempo[[1]])
runs.test(SPapas[[1]])
runs.test(SPalomitas[[1]])

# Gráficos de series de tiempo
plot(PaTiempo[[1]], type = "l", main = "Serie de Tiempo entre llegada Papas", xlab = "Observaciones", ylab = "Tiempo")
plot(MitasTiempo[[1]], type = "l", main = "Serie de Tiempo entre llegada Palomitas", xlab = "Observaciones", ylab = "Tiempo")
plot(SPapas[[1]], type = "l", main = "Serie de Tiempo de Servicios Papas", xlab = "Observaciones", ylab = "Tiempo")
plot(SPalomitas[[1]], type = "l", main = "Serie de Tiempo de Servicios Palomitas", xlab = "Observaciones", ylab = "Tiempo")





#  coeficiente de correlación de Pearson entre las variables
cor_pearson_tiempo_papas <- cor(PaTiempo$PaTiempo, SPapas$SPapas, method = "pearson")
cor_pearson_tiempo_palomitas <- cor(MitasTiempo$MitasTiempo, SPalomitas$SPalomitas, method = "pearson")
cor_pearson_papas_palomitas <- cor(MitasTiempo$MitasTiempo, SPalomitas$SPalomitas, method = "pearson")

print(paste("Coeficiente de Pearson entre Tiempo de llegada y  Tiempo de Servicio Papas:", cor_pearson_tiempo_papas))
print(paste("Coeficiente de Pearson entre Tiempo de llegada y Tiempo de servicio de Palomitas:", cor_pearson_tiempo_palomitas))
print(paste("Coeficiente de Pearson entre Tiempo de llegada Papas y Tiempo de servicio Palomitas:", cor_pearson_papas_palomitas))


# Calcular estadísticos
media_tiempopa <- mean(PaTiempo[[1]], na.rm = TRUE)
varianza_tiempopa <- var(PaTiempo[[1]], na.rm = TRUE)

media_tiempomi <- mean(MitasTiempo[[1]], na.rm = TRUE)
varianza_tiempomi <- var(MitasTiempo[[1]], na.rm = TRUE)

media_spalomitas <- mean(SPalomitas[[1]], na.rm = TRUE)
varianza_spalomitas <- var(SPalomitas[[1]], na.rm = TRUE)

media_spapas <- mean(SPapas[[1]], na.rm = TRUE)
varianza_spapas <- var(SPapas[[1]], na.rm = TRUE)

# Histograma para 'Tiempo de llegada papas'
ggplot(data.frame(Valor = PaTiempo[[1]]), aes(x = Valor)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "khaki", color = "black", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(title = paste("Histograma para 'Tiempo de llegada papas'\nMedia:", media_tiempopa, "Varianza:", varianza_tiempopa),
       x = "Tiempo",
       y = "Densidad") +
  theme_minimal()

# Histograma para 'Papas'
ggplot(data.frame(Valor = MitasTiempo[[1]]), aes(x = Valor)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "khaki", color = "black", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(title = paste("Histograma para 'Tiempo de llegadaPalomitas'\nMedia:", media_tiempomi, "Varianza:", varianza_tiempomi),
       x = "Papas",
       y = "Densidad") +
  theme_minimal()

# Histograma para 'Palomitas'
ggplot(data.frame(Valor = SPalomitas[[1]]), aes(x = Valor)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "khaki", color = "black", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(title = paste("Histograma para 'Tiempo de servicio Palomitas'\nMedia:", media_spapas, "Varianza:", varianza_spalomitas),
       x = "Palomitas",
       y = "Densidad") +
  theme_minimal()


# Histograma para 'Palomitas'
ggplot(data.frame(Valor = SPapas[[1]]), aes(x = Valor)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "khaki", color = "black", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  labs(title = paste("Histograma para 'Tiempo de servicio Papas'\nMedia:", media_spalomitas, "Varianza:", varianza_spapas),
       x = "Palomitas",
       y = "Densidad") +
  theme_minimal()


#KS ajuste

# Variable 'Tiempo entre llegadas Papas'
lambda_tiempo <- 1 / mean(PaTiempo[[1]], na.rm = TRUE)  # Parámetro lambda de la distribución exponencial
ks_test_tiempo <- ks.test(PaTiempo[[1]], "pexp", rate = lambda_tiempo)
ks_test_tiempo

# Variable 'Tiempo entre llegada palomitas'
lambda_papas <- 1 / mean(MitasTiempo[[1]], na.rm = TRUE)
ks_test_papas <- ks.test(MitasTiempo[[1]], "pexp", rate = lambda_papas)
ks_test_papas

# Variable 'SPalomitas'
lambda_palomitas <- 1 / mean(SPalomitas[[1]], na.rm = TRUE)
ks_test_palomitas <- ks.test(SPalomitas[[1]], "pexp", rate = lambda_palomitas)
ks_test_palomitas

# Variable 'SPapas'
lambda_palomitas <- 1 / mean(SPapas[[1]], na.rm = TRUE)
ks_test_palomitas <- ks.test(SPapas[[1]], "pexp", rate = lambda_palomitas)
ks_test_palomitas


###Graficos

# Función para crear Q-Q plots y gráficos de residuos
generate_uniform_qq_plots <- function(data, variable_name) {
  # Calcular los parámetros de la distribución uniforme
  min_val <- min(data, na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE)
  
  # Q-Q plot para la distribución uniforme
  qqplot_unif <- ggplot(data.frame(sample = data), aes(sample = sample)) +
    stat_qq(distribution = qunif, dparams = c(min_val, max_val)) +
    stat_qq_line(distribution = qunif, dparams = c(min_val, max_val), color = "blue") +
    labs(title = paste("Q-Q Plot de Distribución Exponencial para", variable_name),
         x = "Valor Observado", y = "Valor Esperado Exponencial") +
    theme_minimal()
  
  # Gráfico de residuos
  residuals <- sort(data) - qunif(ppoints(data), min = min_val, max = max_val)
  residual_plot <- ggplot(data.frame(residuals = residuals), aes(x = 1:length(residuals), y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Gráfico de Residuos para Distribución Exponencial de", variable_name),
         x = "Índice", y = "Residuos") +
    theme_minimal()
  
  # Mostrar gráficos
  print(qqplot_unif)
  print(residual_plot)
}

#  gráficos para cada variable
generate_uniform_qq_plots(PaTiempo[[1]], "Tiempo entre llegadas papas")
generate_uniform_qq_plots(MitasTiempo[[1]], "Tiempo entre llegadas palomitas")
generate_uniform_qq_plots(SPalomitas[[1]], "Tiempo de servicio Palomitas")
generate_uniform_qq_plots(SPapas[[1]], "Tiempo de servicio Papas")

#Comparacion de media


# Calcular las medias y varianzas
media_papas <- mean(SPapas[[1]], na.rm = TRUE)
varianza_papas <- var(SPapas[[1]], na.rm = TRUE)

media_palomitas <- mean(SPalomitas[[1]], na.rm = TRUE)
varianza_palomitas <- var(SPalomitas[[1]], na.rm = TRUE)

# Imprimir las medias y varianzas
print(paste("Media Papas:", media_papas, "Varianza Papas:", varianza_papas))
print(paste("Media Palomitas:", media_palomitas, "Varianza Palomitas:", varianza_palomitas))

#  prueba de Levene para comparar varianzas
prueba_levene <- leveneTest(
  c(Papas[[1]], Palomitas[[1]]),
  group = factor(rep(c("Papas", "Palomitas"), times = c(length(Papas[[1]]), length(Palomitas[[1]]))))
)
                            
# prueba de Levene
print(prueba_levene)   

#  prueba t
prueba_t <- t.test(Papas[[1]], Palomitas[[1]], var.equal = TRUE)

#prueba t
print(prueba_t)




