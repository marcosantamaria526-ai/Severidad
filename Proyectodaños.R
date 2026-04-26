

# Librerías
library(fitdistrplus)
library(goftest)
library(readxl)
library(actuar)
library(dplyr)
library(moments)
library(tidyr)
library(tidyverse)
library(stringr)
library(lubridate)

# Leer datos
datos <- read_excel("C:/Users/jmnav/Downloads/Basehistorica_2000_a_2024.xlsx", sheet = "Datos")
# Limpiar nombres de columnas (MUY IMPORTANTE)
names(datos) <- make.names(names(datos))

# Ver estructura
str(datos)

# Ver primeras filas
head(datos)

# Resumen
summary(datos)

# Ver valores únicos de la clasificación
unique(datos$Clasificación.del.fenómeno)

# Filtrar hidrometeorológicos
datos_hidro <- datos %>%
  filter(Clasificación.del.fenómeno == "Hidrometeorológico")

nrow(datos_hidro)

# ==============================================
# CONTAR ESTADOS Y DIVIDIR DAÑOS ANTES DE SEPARAR
# ==============================================

# Convertir daños a numérico primero
datos_hidro$Total.de.daños..millones.de.pesos. <- 
  as.numeric(datos_hidro$Total.de.daños..millones.de.pesos.)

# Guardar la suma original para verificar después
suma_original <- sum(datos_hidro$Total.de.daños..millones.de.pesos., na.rm = TRUE)
cat("\n=== SUMA ORIGINAL DE DAÑOS ===\n")
cat("Suma total:", suma_original, "\n\n")

# Contar cuántos estados hay en cada celda (MEJORADO)
datos_hidro <- datos_hidro %>%
  mutate(
    # Limpiar la columna Estado para contar correctamente
    Estado_para_contar = Estado,
    # Reemplazar " y " por comas
    Estado_para_contar = str_replace_all(Estado_para_contar, " y ", ","),
    # Eliminar "Varios Estados" y similares
    Estado_para_contar = str_replace_all(Estado_para_contar, "(?i)varios estados", ""),
    Estado_para_contar = str_replace_all(Estado_para_contar, "(?i)varios", ""),
    # Contar estados (separados por coma)
    n_estados = str_count(Estado_para_contar, ",") + 1,
    # Si después de limpiar quedó vacío o es NA, es 1 estado
    n_estados = ifelse(
      is.na(Estado_para_contar) | 
        Estado_para_contar == "" | 
        str_detect(tolower(Estado), "varios"),
      1, n_estados
    )
  ) %>%
  select(-Estado_para_contar)

# Ver algunos ejemplos de cómo se dividen los daños
cat("=== EJEMPLOS DE DIVISIÓN DE DAÑOS ===\n")
datos_hidro %>%
  filter(n_estados > 1) %>%
  select(Estado, n_estados, Total.de.daños..millones.de.pesos.) %>%
  head(10) %>%
  print()

# Dividir los daños entre el número de estados
datos_hidro <- datos_hidro %>%
  mutate(
    Daños_original = Total.de.daños..millones.de.pesos.,
    Total.de.daños..millones.de.pesos. = Total.de.daños..millones.de.pesos. / n_estados
  )

# Mostrar cómo cambió la división
cat("\n=== CÓMO CAMBIARON LOS DAÑOS DESPUÉS DE DIVIDIR ===\n")
datos_hidro %>%
  filter(n_estados > 1) %>%
  select(Estado, n_estados, Daños_original, Total.de.daños..millones.de.pesos.) %>%
  head(10) %>%
  print()

# ==============================================
# AHORA SEPARAMOS LOS ESTADOS
# ==============================================

# Guardar el número de filas antes de separar
filas_antes <- nrow(datos_hidro)

datos_hidro <- datos_hidro %>%
  mutate(Estado_limpio = Estado) %>%
  separate_rows(Estado_limpio, sep = ",") %>%
  separate_rows(Estado_limpio, sep = " y ") %>%
  mutate(Estado_limpio = trimws(Estado_limpio)) %>%
  mutate(Estado_limpio = str_to_lower(Estado_limpio)) %>%
  mutate(Estado_limpio = case_when(
    Estado_limpio == "estado de méxico" ~ "estado de mexico",
    Estado_limpio == "méxico" ~ "estado de mexico",
    Estado_limpio == "ciudad de méxico" ~ "ciudad de mexico",
    Estado_limpio == "michoacán" ~ "michoacan",
    Estado_limpio == "nuevo león" ~ "nuevo leon",
    Estado_limpio == "san luis potosí" ~ "san luis potosi",
    TRUE ~ Estado_limpio
  )) %>%
  filter(!Estado_limpio %in% c("varios estados", "veracruz y tamaulipas", "varios")) %>%
  filter(!is.na(Estado_limpio), Estado_limpio != "")

# Mostrar cuántas filas se generaron
filas_despues <- nrow(datos_hidro)
cat("\n=== EXPANSIÓN DE FILAS ===\n")
cat("Filas antes de separar estados:", filas_antes, "\n")
cat("Filas después de separar estados:", filas_despues, "\n")
cat("Factor de expansión:", round(filas_despues / filas_antes, 2), "\n\n")

# Eliminar la columna original Estado
datos_hidro <- datos_hidro %>%
  select(-Estado)

# ==============================================
# VERIFICAR QUE LA SUMA DE DAÑOS NO CAMBIÓ
# ==============================================

suma_despues <- sum(datos_hidro$Total.de.daños..millones.de.pesos., na.rm = TRUE)
cat("=== VERIFICACIÓN DE SUMA TOTAL DE DAÑOS ===\n")
cat("Suma antes de dividir:", suma_original, "\n")
cat("Suma después de dividir y expandir:", suma_despues, "\n")
cat("Diferencia:", abs(suma_original - suma_despues), "\n")
cat("¿Son iguales?", ifelse(abs(suma_original - suma_despues) < 0.01, "SÍ ✓", "NO ✗"), "\n\n")

# ==============================================
# VERIFICAR QUE LOS DAÑOS POR ESTADO SON COHERENTES
# ==============================================

# Buscar un evento específico que tenía múltiples estados para verificar
cat("=== VERIFICACIÓN DE UN EVENTO CON MÚLTIPLES ESTADOS ===\n")
# Buscar una fila con varios estados (ejemplo de 2005 Huracán Stan que afectó varios estados)
evento_ejemplo <- datos_hidro %>%
  filter(grepl("Stan", Descripcion.general.de.los.daños, ignore.case = TRUE)) %>%
  group_by(Fecha.de.Inicio, Descripcion.general.de.los.daños) %>%
  summarise(
    estados_afectados = paste(unique(Estado_limpio), collapse = ", "),
    daños_totales = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    n_estados = n_distinct(Estado_limpio),
    .groups = "drop"
  ) %>%
  head(1)

print(evento_ejemplo)

# ==============================================
# ELIMINAR COLUMNAS NO DESEADAS
# ==============================================

# Mostrar columnas disponibles
cat("\n=== COLUMNAS DISPONIBLES ===\n")
print(names(datos_hidro))

# Definir patrones para buscar columnas a eliminar
patrones_a_eliminar <- c(
  "Defunciones",
  "Población",
  "Viviendas",
  "Escuelas",
  "Hospitales",
  "Comercios",
  "Descripcion",
  "Municipios",
  "Area.*cultivo",
  "Fuente"
)

# Encontrar todas las columnas que coinciden
columnas_a_eliminar <- c()
for(patron in patrones_a_eliminar) {
  encontradas <- grep(patron, names(datos_hidro), value = TRUE, ignore.case = TRUE)
  columnas_a_eliminar <- c(columnas_a_eliminar, encontradas)
}
columnas_a_eliminar <- unique(columnas_a_eliminar)

cat("\nColumnas a eliminar:\n")
print(columnas_a_eliminar)

# Eliminar las columnas
datos_hidro <- datos_hidro %>%
  select(-all_of(columnas_a_eliminar))

# Ver columnas finales
cat("\n=== COLUMNAS FINALES ===\n")
print(names(datos_hidro))

# ==============================================
# CONTINUAR CON EL ANÁLISIS
# ==============================================

# Quitar NA en daños si los hay
datos_hidro <- datos_hidro %>%
  filter(!is.na(Total.de.daños..millones.de.pesos.))

nrow(datos_hidro)

# Quitar duplicados
datos_hidro <- distinct(datos_hidro)

# Verificar datos limpios
str(datos_hidro)
summary(datos_hidro)

# Seleccionar solo la columna de daños
danos <- datos_hidro$Total.de.daños..millones.de.pesos.

# Estadísticas descriptivas
media <- mean(danos)
mediana <- median(danos)
desviacion <- sd(danos)
asimetria <- skewness(danos)
kurtosis_val <- kurtosis(danos)

cat("\n=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
cat("Media:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Desviación estándar:", desviacion, "\n")
cat("Asimetría:", asimetria, "\n")
cat("Curtosis:", kurtosis_val, "\n")

.0# Número de estados únicos
cat("\nNúmero de estados únicos:", length(unique(datos_hidro$Estado_limpio)), "\n")

# ==============================================
# LIMPIAR FECHAS
# ==============================================

datos_hidro <- datos_hidro %>%
  mutate(
    Fecha.de.Inicio = ifelse(is.na(Fecha.de.Inicio), Fecha.de.Fin, Fecha.de.Inicio),
    Fecha.de.Fin    = ifelse(is.na(Fecha.de.Fin), Fecha.de.Inicio, Fecha.de.Fin)
  )

colSums(is.na(datos_hidro[, c("Fecha.de.Inicio", "Fecha.de.Fin")]))

# ==============================================
# FILTRAR "Otros" Y "Sin clasificación"
# ==============================================
# ==============================================
# LIMPIEZA DE TIPO DE FENÓMENO
# ==============================================

datos_hidro <- datos_hidro %>%
  mutate(
    Tipo.de.fenómeno = str_to_lower(str_trim(Tipo.de.fenómeno)),
    Tipo.de.fenómeno = iconv(Tipo.de.fenómeno, from = "UTF-8", to = "ASCII//TRANSLIT")
  ) %>%
  
  # Unificar categorías
  mutate(
    Tipo.de.fenómeno = case_when(
      str_detect(Tipo.de.fenómeno, "lluvia") ~ "lluvia",
      str_detect(Tipo.de.fenómeno, "inund") ~ "inundacion",
      str_detect(Tipo.de.fenómeno, "torment") ~ "tormenta",
      TRUE ~ Tipo.de.fenómeno
    )
  ) %>%
  
  # Eliminar basura
  filter(!Tipo.de.fenómeno %in% c("otros", "sin clasificacion"))

unique(datos_hidro$Tipo.de.fenómeno)
# ==============================================
# NORMALIZACIÓN
# ==============================================

datos_hidro <- datos_hidro %>%
  mutate(
    feno_aux = toupper(iconv(Tipo.de.fenómeno, from = "UTF-8", to = "ASCII//TRANSLIT"))
  )

datos_hidro <- datos_hidro %>%
  mutate(
    Fenomeno_Ajustado = case_when(
      str_detect(feno_aux, "INUNDACION|LLUVIA|LLUVIAS|CICLON TROPICAL|TORMENTA|GRANIZADA|MAR DE FONDO|MAREA DE TORMENTA|TORMENTA ELECTRICA|TORMENTA SEVERA|TORMENTA TROPICAL|TORNADO|FUERTES VIENTOS") ~ "Agua",
      str_detect(feno_aux, "BAJAS TEMPERATURAS|TEMPERATURA EXTREMA|HELADA|SEQUIA|NEVADA") ~ "Temperatura",
      TRUE ~ "Eliminar"
    )
  )

# Verificación final
revision_final <- datos_hidro %>%
  group_by(Tipo.de.fenómeno, Fenomeno_Ajustado) %>%
  tally() %>%
  arrange(desc(n))

print(revision_final, n = 100)

# ==============================================
# RESUMEN FINAL POR ESTADO (CON DAÑOS YA DIVIDIDOS)
# ==============================================

resumen_estado <- datos_hidro %>%
  group_by(Estado_limpio) %>%
  summarise(
    total_eventos = n(),
    suma_daños = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    media_daños = mean(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(suma_daños))

cat("\n=== TOP 10 ESTADOS POR DAÑOS TOTALES ===\n")
print(head(resumen_estado, 10))

# Verificar que la suma total de daños por estado es igual a la suma original
cat("\n=== VERIFICACIÓN FINAL ===\n")
cat("Suma total de daños en resumen por estado:", sum(resumen_estado$suma_daños, na.rm = TRUE), "\n")
cat("Suma total de daños originales:", suma_original, "\n")
cat("¿Coinciden?", ifelse(abs(sum(resumen_estado$suma_daños) - suma_original) < 0.01, "SÍ ✓", "NO ✗"), "\n")

View(datos_hidro)

# ==============================================
# DRIVER DE REGIÓN (USANDO TUS DEFINICIONES)
# ==============================================

datos_hidro <- datos_hidro %>%
  mutate(
    edo_aux = toupper(iconv(Estado_limpio, from = "UTF-8", to = "ASCII//TRANSLIT")),
    edo_aux = str_trim(edo_aux),
    
    Estado_Ajustado = case_when(
      edo_aux %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "SINALOA", "TAMAULIPAS") 
      ~ "Norte_Costa",
      
      edo_aux %in% c("CHIHUAHUA", "COAHUILA", "NUEVO LEON", "DURANGO", "ZACATECAS") 
      ~ "Norte_Interior",
      
      edo_aux %in% c("NAYARIT", "JALISCO", "COLIMA", "MICHOACAN", "GUERRERO", 
                     "OAXACA", "CHIAPAS", "VERACRUZ", "TABASCO", "CAMPECHE", 
                     "YUCATAN", "QUINTANA ROO") 
      ~ "Sur_Costa",
      
      edo_aux %in% c("CIUDAD DE MEXICO", "ESTADO DE MEXICO", "PUEBLA", 
                     "TLAXCALA", "MORELOS", "HIDALGO", "QUERETARO", 
                     "GUANAJUATO", "AGUASCALIENTES", "SAN LUIS POTOSI") 
      ~ "Zona_Centro",
      
      TRUE ~ "Revisar"
    )
  )

table(datos_hidro$Estado_Ajustado)

# 2. Definir tu vector de inflación (ya lo tienes en tu código)
inflacion <- c(27.7, 15.72, 18.61, 12.32, 8.96, 4.4, 5.7, 3.98, 5.19, 3.33, 
               4.05, 3.76, 0.71, 3.57, 4.04, 3.87, 3.33, 1.09, 4.08, 2.13, 
               3.36, 2.12, 4.83, 2.83, 3.15, 7.01, 7.82, 4.66, 1.12, 3.8)



anios <- 1996:2025

# ==============================================
# AJUSTE POR INFLACIÓN USANDO SOLO "Año"
# ==============================================

# Asegurar que Año exista (si ya existe, esto no lo rompe)
datos_hidro <- datos_hidro %>%
  mutate(Año = as.numeric(Año))

# Crear tabla de inflación
df_inflacion <- data.frame(
  Año = anios,
  inflacion = inflacion / 100
)

# Calcular factor acumulado hacia 2025
df_inflacion <- df_inflacion %>%
  arrange(Año) %>%
  mutate(
    factor_2025 = sapply(Año, function(a) {
      prod(1 + df_inflacion$inflacion[df_inflacion$Año > a])
    })
  )

# Unir con tus datos SOLO por Año
datos_hidro <- datos_hidro %>%
  left_join(df_inflacion, by = "Año")

# Ajustar daños
datos_hidro <- datos_hidro %>%
  mutate(
    Danos_2025 = Total.de.daños..millones.de.pesos. * factor_2025
  )

# ==============================================
# VERIFICACIÓN
# ==============================================

cat("\n=== AJUSTE SOLO CON AÑO ===\n")
datos_hidro %>%
  select(Año, Total.de.daños..millones.de.pesos., Danos_2025) %>%
  head(10) %>%
  print()

# ==============================================
# AGRUPACIÓN, LOGARITMO Y SUAVIZADO
# ==============================================

# 1. Filtrar registros útiles para el modelo (Sin "Eliminar" y daños > 0)
datos_modelo <- datos_hidro %>%
  filter(Fenomeno_Ajustado != "Eliminar", 
         Danos_2025 > 0)

# 2. Aplicar Logaritmo a las pérdidas actualizadas
# Esto normaliza la escala y reduce el impacto de los valores extremos (Outliers)
datos_modelo <- datos_modelo %>%
  mutate(log_danos = log(Danos_2025))

cat("\n=== DATOS TRAS TRANSFORMACIÓN LOGARÍTMICA ===\n")
print(summary(datos_modelo$log_danos))


# ==============================================
# ANÁLISIS DESCRIPTIVO
# ==============================================

# 3. Suavizado y Visualización
# Usamos geom_density para suavizar la distribución de los datos agrupados
library(ggplot2)

# Gráfico de suavizado por Fenómeno
grafico_suave_feno <- ggplot(datos_modelo, aes(x = log_danos, fill = Fenomeno_Ajustado)) +
  geom_density(alpha = 0.5) +
  labs(title = "Suavizado de Pérdidas por Fenómeno (Escala Log)",
       x = "Logaritmo de los Daños",
       y = "Densidad (Suavizado)") +
  theme_minimal()

print(grafico_suave_feno)

# Gráfico de suavizado por Región (Drivers de Estado)
grafico_suave_region <- ggplot(datos_modelo, aes(x = log_danos, fill = Estado_Ajustado)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Estado_Ajustado) +
  labs(title = "Distribución Suavizada por Región",
       x = "Logaritmo de los Daños",
       y = "Densidad") +
  theme_light()

print(grafico_suave_region)

# 4. Agrupación final para el análisis de Severidad
# Creamos el portafolio homogéneo final
tabla_homogenea <- datos_modelo %>%
  group_by(Estado_Ajustado, Fenomeno_Ajustado) %>%
  summarise(
    n_siniestros = n(),
    media_log = mean(log_danos),
    sd_log = sd(log_danos),
    .groups = "drop"
  )

cat("\n=== PORTAFOLIO HOMOGÉNEO (RESUMEN LOG) ===\n")
print(tabla_homogenea)

summary(datos_modelo)
view(datos_modelo)
nrow(datos_hidro)

# ==============================================
# ANÁLISIS DESCRIPTIVO DE DAÑOS POR FENÓMENOS HIDROMETEOROLÓGICOS
# ==============================================

# 1. ESTADÍSTICAS GLOBALES
# ==============================================

cat("\n")
cat(rep("=", 80))
cat("\n📊 ANÁLISIS DESCRIPTIVO GLOBAL\n")
cat(rep("=", 80))
cat("\n\n")
# Estadísticas generales de daños actualizados
estadisticas_globales <- datos_modelo %>%
  summarise(
    Total_Daños_Millones = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Total_Daños_Miles_Millones = Total_Daños_Millones / 1000,
    Media_Daños = mean(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Mediana_Daños = median(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Desviacion_Estandar = sd(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Coeficiente_Variacion = Desviacion_Estandar / Media_Daños,
    Minimo = min(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Maximo = max(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Rango = Maximo - Minimo,
    Q1 = quantile(Total.de.daños..millones.de.pesos., 0.25, na.rm = TRUE),
    Q3 = quantile(Total.de.daños..millones.de.pesos., 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Asimetria = skewness(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Curtosis = kurtosis(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    N_Eventos = n()
  )

print(estadisticas_globales)

# 2. ANÁLISIS POR TIPO DE FENÓMENO
# ==============================================


cat("\n")
cat(rep("=", 80))
cat("\n🌊 ANÁLISIS POR TIPO DE FENÓMENO\n")
cat(rep("=", 80))
cat("\n\n")
# Estadísticas por fenómeno ajustado
estadisticas_fenomeno <- datos_modelo %>%
  group_by(Fenomeno_Ajustado) %>%
  summarise(
    N_Eventos = n(),
    Porcentaje_Eventos = round(n() / nrow(datos_modelo) * 100, 2),
    Total_Daños_Millones = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Total_Daños_Miles_Millones = Total_Daños_Millones / 1000,
    Participacion_Daños = round(Total_Daños_Millones / sum(datos_modelo$Total.de.daños..millones.de.pesos., na.rm = TRUE) * 100, 2),
    Media_Daños = mean(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Mediana_Daños = median(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    SD_Daños = sd(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Min_Daños = min(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Max_Daños = max(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    CV = SD_Daños / Media_Daños,
    Asimetria = skewness(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Curtosis = kurtosis(Total.de.daños..millones.de.pesos., na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Daños_Millones))

print(estadisticas_fenomeno)

# 3. ANÁLISIS POR REGIÓN
# ==============================================

cat("\n")
cat(rep("=", 80))
cat("\n🗺️  ANÁLISIS POR REGIÓN\n")
cat(rep("=", 80))
cat("\n\n")

# Estadísticas por región
estadisticas_region <- datos_modelo %>%
  group_by(Estado_Ajustado) %>%
  summarise(
    N_Eventos = n(),
    Porcentaje_Eventos = round(n() / nrow(datos_modelo) * 100, 2),
    Total_Daños_Millones = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Total_Daños_Miles_Millones = Total_Daños_Millones / 1000,
    Participacion_Daños = round(Total_Daños_Millones / sum(datos_modelo$Total.de.daños..millones.de.pesos., na.rm = TRUE) * 100, 2),
    Media_Daños = mean(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Mediana_Daños = median(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    SD_Daños = sd(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Min_Daños = min(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Max_Daños = max(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    CV = SD_Daños / Media_Daños
  ) %>%
  arrange(desc(Total_Daños_Millones))

print(estadisticas_region)

# 4. ANÁLISIS POR ESTADO (TOP 15)
# ==============================================

cat("\n")
cat(rep("=", 80))
cat("\n🏛️  TOP 15 ESTADOS CON MAYOR DAÑO ACUMULADO\n")
cat(rep("=", 80))
cat("\n\n")


# Estadísticas por estado
estadisticas_estado <- datos_modelo %>%
  group_by(Estado_limpio) %>%
  summarise(
    N_Eventos = n(),
    Total_Daños_Millones = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Total_Daños_Miles_Millones = Total_Daños_Millones / 1000,
    Media_Daños = mean(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Mediana_Daños = median(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Max_Daños = max(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Participacion_Nacional = round(Total_Daños_Millones / sum(datos_modelo$Total.de.daños..millones.de.pesos., na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Total_Daños_Millones)) %>%
  head(15)

print(estadisticas_estado)




# 7. ANÁLISIS DE RIESGO POR REGIÓN Y FENÓMENO
# ==============================================

cat("\n")
cat(rep("=", 80))
cat("\n⚠️  MATRIZ DE RIESGO (Región vs Fenómeno)\n")
cat(rep("=", 80))
cat("\n\n")

# Matriz de daños totales
matriz_riesgo_total <- datos_modelo %>%
  group_by(Estado_Ajustado, Fenomeno_Ajustado) %>%
  summarise(
    Daños_Totales_Millones = sum(Total.de.daños..millones.de.pesos., na.rm = TRUE),
    Daños_Totales_Miles_Millones = Daños_Totales_Millones / 1000,
    N_Eventos = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Fenomeno_Ajustado,
    values_from = c(Daños_Totales_Miles_Millones, N_Eventos),
    values_fill = 0
  )

print("Daños totales por Región y Fenómeno (Miles de Millones de Pesos):")
print(matriz_riesgo_total)

# Matriz de frecuencia
cat("\nFrecuencia de eventos por Región y Fenómeno:\n")
matriz_frecuencia <- datos_modelo %>%
  group_by(Estado_Ajustado, Fenomeno_Ajustado) %>%
  summarise(
    N_Eventos = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Fenomeno_Ajustado,
    values_from = N_Eventos,
    values_fill = 0
  )

print(matriz_frecuencia)

# 8. ESTADÍSTICAS DE LOS LOGARITMOS
# ==============================================


cat("\n")
cat(rep("=", 80))
cat("\n📐 ESTADÍSTICAS DE LA TRANSFORMACIÓN LOGARÍTMICA\n")
cat(rep("=", 80))
cat("\n\n")

estadisticas_log <- datos_modelo %>%
  summarise(
    Media_log = mean(log_danos, na.rm = TRUE),
    Mediana_log = median(log_danos, na.rm = TRUE),
    SD_log = sd(log_danos, na.rm = TRUE),
    Asimetria_log = skewness(log_danos, na.rm = TRUE),
    Curtosis_log = kurtosis(log_danos, na.rm = TRUE),
    Min_log = min(log_danos, na.rm = TRUE),
    Max_log = max(log_danos, na.rm = TRUE)
  )

print(estadisticas_log)



#Realizar la estimación de parámetros de las distribuciones de probabilidad para la
#severidad. 


names(datos_modelo)

#Realizar la estimación de parámetros de las distribuciones
#Dame la estimación de parámetros
#Realizar la estimación de parámetros de las distribuciones de probabilidad para la
#severidad.


# ==============================================================================
# 1. ESTIMACIÓN DE PARÁMETROS Y PRUEBAS DE BONDAD DE AJUSTE (P-VALUE)
# ==============================================================================

# Identificar segmentos con representatividad (n >= 30) para asegurar convergencia
segmentos_modelar <- datos_modelo %>%
  group_by(Estado_Ajustado, Fenomeno_Ajustado) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 30)

resultados_severidad <- data.frame()

for(i in 1:nrow(segmentos_modelar)) {
  
  reg <- segmentos_modelar$Estado_Ajustado[i]
  fen <- segmentos_modelar$Fenomeno_Ajustado[i]
  
  # Filtrar datos del segmento (Pérdidas actualizadas a 2025)
  x <- datos_modelo %>%
    filter(Estado_Ajustado == reg, Fenomeno_Ajustado == fen) %>%
    pull(Danos_2025)
  
  # --- Ajuste de Modelos ---
  # Lognormal es el estándar para severidad por su cola pesada
  fit_ln <- fitdist(x, "lnorm")
  # Gamma es útil por su flexibilidad en la forma
  fit_ga <- try(fitdist(x, "gamma"), silent = TRUE)
  
  # --- Pruebas de Bondad de Ajuste (KS Test) ---
  # Evaluamos la H0: "Los datos siguen la distribución X"
  ks_ln <- ks.test(x, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2])
  
  if(class(fit_ga) != "try-error") {
    ks_ga <- ks.test(x, "pgamma", shape = fit_ga$estimate[1], rate = fit_ga$estimate[2])
    
    # Selección del mejor modelo basado en AIC (menor pérdida de información)
    aic_ln <- fit_ln$aic
    aic_ga <- fit_ga$aic
    
    if(aic_ln < aic_ga) {
      mejor <- "Lognormal"
      p_val <- ks_ln$p.value
      # E[X] Lognormal = exp(mu + sigma^2 / 2)
      costo_medio <- exp(fit_ln$estimate[1] + (fit_ln$estimate[2]^2)/2)
    } else {
      mejor <- "Gamma"
      p_val <- ks_ga$p.value
      # E[X] Gamma = shape / rate
      costo_medio <- fit_ga$estimate[1] / fit_ga$estimate[2]
    }
  } else {
    mejor <- "Lognormal"
    p_val <- ks_ln$p.value
    costo_medio <- exp(fit_ln$estimate[1] + (fit_ln$estimate[2]^2)/2)
  }
  
  # Guardar resultados
  resultados_severidad <- rbind(resultados_severidad, data.frame(
    Region = reg,
    Fenomeno = fen,
    N = length(x),
    Distribucion_Optima = mejor,
    P_Value_KS = round(p_val, 4),
    Validacion = ifelse(p_val > 0.05, "Validado ✓", "Revisar (Cola Pesada) ✗"),
    Costo_Promedio_2025 = costo_medio
  ))
}

# ==============================================================================
# 2. RESULTADOS PARA EL REPORTE (PUNTO 3: RECOMENDACIONES)
# ==============================================================================

cat("\n", rep("=", 85), "\n")
cat("📊 REPORTE DE SEVERIDAD: ESTIMACIÓN Y VALIDACIÓN\n")
cat(rep("=", 85), "\n\n")

print(resultados_severidad)

# Cálculo del Costo Promedio Global Esperado si ocurre un siniestro
costo_global_2025 <- mean(resultados_severidad$Costo_Promedio_2025)

cat("\n--- RESPUESTAS PARA RECOMENDACIONES ---\n")
cat("1. ¿Cuánto dinero se espera perder en promedio dado que ocurre un siniestro?\n")
cat("   Respuesta: Se espera una pérdida promedio de $", 
    format(round(costo_global_2025, 2), big.mark = ","), " millones por evento en 2025.\n")
cat("2. Las reclamaciones deben tratarse como variables aleatorias independientes de severidad.\n")