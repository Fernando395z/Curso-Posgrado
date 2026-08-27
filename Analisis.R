# Análisis de emergencia de plántulas - El Cuy / Aguada Guzmán
# Pastoreo vs. clausura (sep-2013 a sep-2014)
# =============================================================

library(tidyverse)


# 0.a Ubicar automáticamente la carpeta donde está este script
#     y trabajar ahí (así el CSV se encuentra sin importar en
#     qué compu o carpeta esté guardado el proyecto).
#     Funciona al correr el script con "Source" en RStudio.
# -------------------------------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ruta_script <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(ruta_script)) setwd(dirname(ruta_script))
}

# Chequeo de seguridad: si el CSV no está en la carpeta actual,
# avisa con un mensaje claro en vez del error críptico de siempre.
if (!file.exists("elcuy.csv")) {
  stop(
    "No se encontró 'elcuy.csv' en la carpeta: ", getwd(), "\n",
    "Asegurate de que el archivo elcuy.csv esté en la MISMA carpeta ",
    "que este script (analisis_elcuy.R)."
  )
}

# -------------------------------------------------------------
# 0.b Carga de datos
#    - separador ";" y coma decimal.
# -------------------------------------------------------------
datos <- read_delim(
  "elcuy.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)
# La 6ta columna es el año (13/14); se renombra por posición para
# evitar problemas de encoding con la "ñ" del nombre original
names(datos)[6] <- "Anio"

glimpse(datos)


# 1. Frecuencia de emergencia por especie y tratamiento
# =============================================================
frec_especie_pastoreo <- datos %>%
  group_by(Pastoreo, Especie) %>%
  summarise(frecuencia = sum(emergencia), .groups = "drop")

frec_especie_pastoreo

# 2. Orden de mayor a menor frecuencia
# =============================================================

frec_especie_pastoreo_ord <- frec_especie_pastoreo %>%
  arrange(desc(frecuencia))

frec_especie_pastoreo_ord


# 3. Variable temporal a formato fecha y frecuencia por especie,
# tratamiento y momento del año
# Nota: "Año" esta codificado como 13/14 (2013/2014) y "mes"
# como el mes calendario de cada campaña de muestreo.
# =============================================================

datos <- datos %>%
  mutate(
    Anio_completo = 2000 + Anio,
    fecha = make_date( year = Anio_completo, month = mes, day = 1)
  )
frec_tiempo <- datos %>%
  group_by(Pastoreo, Especie, fecha) %>%
  summarise(frecuencia = sum(emergencia), .groups = "drop") %>%
  arrange(fecha, Pastoreo, desc(frecuencia))

frec_tiempo
  
# 4. Transformación logarítmica de la emergencia
# usaremos log(x+1) pues si se agregan valores donde la emergencia sea 0
# de esta manera evitamos log(0) = -inf
# =============================================================

datos <- datos %>%
  mutate(log_emergencia = log(emergencia + 1))
  
# 5. Media, desvío estándar y coeficiente de variación
#    de la emergencia por especie y momento del año
# =============================================================

resumen_especie_fecha <- datos %>%
  group_by (Especie, fecha) %>%
  summarise(
    media_log = mean(log_emergencia),
    de_log    = sd(log_emergencia),
    cv_log    = ifelse(media_log == 0, NA, de_log / media_log * 100),
    .groups = "drop"
  ) %>%
  arrange(fecha, Especie)

resumen_especie_fecha

# 6. Gráfico de emergencia por especie y tratamiento
# =============================================================

graf_especie_pastoreo <- datos %>%
  group_by(Pastoreo, Especie) %>%
  summarise(emergencia_total = sum(emergencia), .groups = "drop") %>%
  ggplot(aes( x = reorder(Especie, emergencia_total), y = emergencia_total, fill = Pastoreo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Emergencia total de plántulas por especie y tratamiento",
    x = "Especie", y = "Emergencia total",
    fill = "Tratamiento"
  ) +
  theme_minimal()

  graf_especie_pastoreo
  ggsave("gráfico_especie_pastoreo.png", graf_especie_pastoreo, width = 8, height = 6, dpi = 150)
  
  
  # 7. Detección y exclusión de especies no identificadas (Spx) con stringr.
  # =============================================================
  
  datos <- datos %>%
    mutate(
      no_identificada = str_detect(Especie, regex("^Sp\\d+", ignore_case = TRUE)),
      Especie = if_else(no_identificada, "No identicada", str_trim(Especie))
    )
  
  
  
# 8. Emergencia media por mes, año y tratamiento para las especies mas frecuentes.
  # =============================================================
  
  especies_top <- datos %>%
    group_by(Especie) %>%
    summarise(total =sum(emergencia), .groups = "drop") %>%
    slice_max(total, n = 4) %>%
    pull(Especie)
  
  resumen_top <- datos %>%
    filter(Especie %in% especies_top) %>%
    group_by(Especie, Pastoreo, fecha) %>%
    summarise(emergencia_media = mean(emergencia), .groups = "drop")
  
  graf_top <- resumen_top %>%
    ggplot(aes(x = fecha, y = emergencia_media, color = Pastoreo)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~Especie, scales = "free_y") +
    labs(
      title = "Emergencia media mensual de las especies más frecuentes",
      subtitle = "Según tratamiento de pastoreo",
      x = "Fecha de muestreo", y = "Emergencia media (plántulas/cuadro)",
      color = "Tratamiento"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  graf_top
  ggsave("grafico_top_especies.png", graf_top, width = 9, height = 6, dpi = 150)