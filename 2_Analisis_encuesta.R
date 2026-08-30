# Ejercicio 2
# =============================================================

library(tidyverse)


# 0.a Ubicar automáticamente la carpeta donde está este script
#     y trabajar ahí (así el CSV se encuentra sin importar en
#     qué compu o carpeta esté guardado el proyecto).
#     Funciona al correr el script con "Source"
# -------------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ruta_script <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(ruta_script)) setwd(dirname(ruta_script))
}

if (!file.exists("encuesta.csv")) {
  stop(
    "No se encontró 'encuesta.csv' en la carpeta: ", getwd(), "\n",
    "Asegurate de que el archivo encuesta.csv esté en la MISMA carpeta ",
    "que este script."
  )
}


# 0.b Carga de datos
#    (el archivo tiene columnas repetidas de nombre "Hora",
#     "Comida", "Bebida" -> se renombran automáticamente con
#     un sufijo numérico; no afecta a las columnas que usamos)
# -------------------------------------------------------------
datos_completos <- suppressMessages(
  read_delim("encuesta.csv", delim = ";", show_col_types = FALSE)
)


# 1. Selección de columnas 8 a 11 y 47 a 51
#    Columnas 8-11:  Año, Curso, Sexo, Edad
#    Columnas 47-51: consumiste_alcohol, consumiste_alcohol_este_anio,
#                     consumiste_alcohol_este_mes,
#                     consumiste_alcohol_esta_semana,
#                     tuviste_un_exceso_de_alcohol
# =============================================================
datos_sel <- datos_completos %>% select(8:11, 47:51)

# Como tibble
tibble_datos <- as_tibble(datos_sel)
tibble_datos

# Como data.frame
df_datos <- as.data.frame(datos_sel)
df_datos

# Diferencias principales al imprimirlos:
# - El tibble muestra solo las primeras 10 filas y el tipo de
#   cada columna (<dbl>, <chr>), y trunca columnas si no entran
#   en el ancho de pantalla.
# - El data.frame imprime TODAS las filas y no informa el tipo
#   de dato de cada columna en el encabezado.
# (En cuanto al contenido, ambas estructuras guardan los mismos
#  datos; lo que cambia es cómo los presentan e imprimen.)


# 2. Consumo de alcohol: filtro, porcentaje total y por sexo
# =============================================================
alcohol_si <- datos_sel %>% filter(consumiste_alcohol == "Sí")

pct_total_alcohol <- nrow(alcohol_si) / nrow(datos_sel) * 100
pct_total_alcohol

alcohol_por_sexo <- alcohol_si %>%
  count(Sexo, name = "frecuencia_absoluta") %>%
  mutate(frecuencia_relativa = frecuencia_absoluta / sum(frecuencia_absoluta) * 100)

alcohol_por_sexo


# 3. Histograma de edad para quienes consumieron alcohol,
#    comparado por sexo
# =============================================================
graf_edad <- alcohol_si %>%
  filter(!is.na(Sexo)) %>%
  ggplot(aes(x = Edad, fill = Sexo)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 0) +
  facet_wrap(~Sexo, ncol = 1) +
  labs(
    title = "Distribución de edad de quienes consumieron alcohol",
    subtitle = "Comparado por sexo",
    x = "Edad", y = "Cantidad de estudiantes"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

graf_edad
ggsave("grafico_histograma_edad.png", graf_edad, width = 7, height = 6, dpi = 150)


# 4. Exceso de consumo de alcohol
# =============================================================

# Porcentaje total de respuestas afirmativas
pct_exceso_total <- sum(datos_sel$tuviste_un_exceso_de_alcohol == "Sí", na.rm = TRUE) /
  nrow(datos_sel) * 100
pct_exceso_total

# -------------------------------------------------------------
# I) Nueva columna Nivel: Básico (1°-3°) / Superior (4°-6°)
# -------------------------------------------------------------
datos_sel <- datos_sel %>%
  mutate(
    Nivel = case_when(
      Año %in% c(1, 2, 3) ~ "Básico",
      Año %in% c(4, 5, 6) ~ "Superior",
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------------------
# II) Porcentaje de respuesta afirmativa por sexo y nivel
# -------------------------------------------------------------
resumen_exceso_nivel <- datos_sel %>%
  filter(!is.na(Sexo), !is.na(Nivel)) %>%
  group_by(Sexo, Nivel) %>%
  summarise(
    total = n(),
    afirmativos = sum(tuviste_un_exceso_de_alcohol == "Sí", na.rm = TRUE),
    porcentaje = afirmativos / total * 100,
    .groups = "drop"
  )

resumen_exceso_nivel

# -------------------------------------------------------------
# III) Gráfico de barras
# -------------------------------------------------------------
graf_exceso_nivel <- resumen_exceso_nivel %>%
  ggplot(aes(x = Nivel, y = porcentaje, fill = Sexo)) +
  geom_col(position = "dodge") +
  labs(
    title = "% de exceso de alcohol declarado, por sexo y nivel educativo",
    x = "Nivel educativo", y = "% de respuestas afirmativas",
    fill = "Sexo"
  ) +
  theme_minimal()

graf_exceso_nivel
ggsave("grafico_exceso_nivel.png", graf_exceso_nivel, width = 7, height = 5, dpi = 150)


# 5. Respuestas afirmativas por sexo para las demás variables
#    de consumo
# =============================================================
vars_consumo <- c(
  "consumiste_alcohol_este_anio",
  "consumiste_alcohol_este_mes",
  "consumiste_alcohol_esta_semana",
  "tuviste_un_exceso_de_alcohol"
)

resumen_consumo <- datos_sel %>%
  filter(!is.na(Sexo)) %>%
  select(Sexo, all_of(vars_consumo)) %>%
  pivot_longer(
    cols = all_of(vars_consumo),
    names_to = "variable",
    values_to = "respuesta"
  ) %>%
  filter(respuesta == "Sí") %>%
  count(variable, Sexo, name = "frecuencia")

resumen_consumo


# 6. Gráfico Lollipop de los resultados del punto 5
# =============================================================

lollipop_datos <- resumen_consumo %>%
  mutate(
    pos_base = as.numeric(factor(variable)),
    desplazamiento = if_else(Sexo == "Femenino", -0.15, 0.15),
    pos = pos_base + desplazamiento
  )

etiquetas_variable <- levels(factor(lollipop_datos$variable))

graf_lollipop <- lollipop_datos %>%
  ggplot(aes(x = pos, y = frecuencia, color = Sexo)) +
  geom_segment(aes(x = pos, xend = pos, y = 0, yend = frecuencia), linewidth = 1) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = seq_along(etiquetas_variable), labels = etiquetas_variable) +
  coord_flip() +
  labs(
    title = "Respuestas afirmativas por variable de consumo y sexo",
    x = "", y = "Cantidad de respuestas 'Sí'",
    color = "Sexo"
  ) +
  theme_minimal()

graf_lollipop
ggsave("grafico_lollipop.png", graf_lollipop, width = 8, height = 5, dpi = 150)
