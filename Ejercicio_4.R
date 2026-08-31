# TP2 - Ejercicio 4: Telefonía fija ENACOM

# Paso 0: Usar la carpeta de librerías nueva (para evitar el bloqueo de Windows)
.libPaths("C:/R_library") 

# Paso 1: fijar carpeta de trabajo
setwd("C:/Leonardo Fuentealba/Curso Ciencia de Datos/Trabajos Practicos")

# Paso 2: cargar el paquete para leer archivos Excel
library(readxl)
library(tidyverse, lib.loc = "C:/R_library")

# Paso 3: importar el archivo Excel
# skip = 1 porque la primera fila del Excel es un título, no datos
telefonia_fija <- read_excel("Telefonía Fija Accesos Totales.xlsx", skip = 1)

str(telefonia_fija)

# Paso 4: corregir columnas mal interpretadas por Excel
telefonia_fija_limpia <- telefonia_fija %>%
  mutate(
    # Hogares, Comercial y Total: vienen como texto con puntos de miles
    across(c(Hogares, Comercial, Total), ~ as.numeric(str_remove_all(as.character(.), "[.,]"))),
    # Gobierno y Otros: Excel los interpretó como decimales, perdieron ceros finales
    across(c(Gobierno, Otros), ~ round(. * 1000))
  ) %>%
  mutate(Total_calculado = Hogares + Comercial + Gobierno + Otros)

# Verificamos la corrección
glimpse(telefonia_fija_limpia)

# Paso 5: Punto 1: resumen estadístico descriptivo
print(
  telefonia_fija_limpia %>%
    select(Hogares, Comercial, Gobierno, Otros, Total) %>%
    summary()
)

# Paso 6. Punto 2: evolución anual del acceso total
evolucion_anual <- telefonia_fija_limpia %>%
  group_by(Año) %>%
  summarise(Total_promedio = mean(Total, na.rm = TRUE))

print(evolucion_anual)

# Paso 7. Punto 3: evolución trimestral
telefonia_fija_limpia %>%
  mutate(periodo = Año + (Trimestre - 1) / 4) %>%
  arrange(periodo) %>%
  select(Año, Trimestre, Total) %>%
  mutate(
    variacion_abs = Total - lag(Total),
    variacion_pct = round((Total - lag(Total)) / lag(Total) * 100, 2)
  ) %>%
  print(n = 49)

# Paso 8. Punto 4: proporción de cada tipo de cliente sobre el total
proporciones <- telefonia_fija_limpia %>%
  summarise(
    Hogares_pct   = mean(Hogares / Total) * 100,
    Comercial_pct = mean(Comercial / Total) * 100,
    Gobierno_pct  = mean(Gobierno / Total) * 100,
    Otros_pct     = mean(Otros / Total) * 100
  )

print(proporciones)

#Paso 9. Punto 5: (Falta una columna de Provincias)