# Ejercicio 3
# =============================================================

library(tidyverse)

# -------------------------------------------------------------
# 0.a Ubicar automáticamente la carpeta donde está este script
#   
# -------------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ruta_script <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(ruta_script)) setwd(dirname(ruta_script))
}

if (!file.exists("cases_deaths.csv")) {
  stop(
    "No se encontró 'cases_deaths.csv' en la carpeta: ", getwd(), "\n",
    "Asegurate de que el archivo esté en la MISMA carpeta que este script."
  )
}


# 2. Importación y exploración preliminar
# =============================================================
# El archivo original de OWID pesa ~180 MB y trae datos de TODO
# el mundo (más de 590.000 filas). Para no sobrecargar la
# memoria, se lee el archivo en chunks y nos
# quedamos solo con las filas de los países que nos interesan
# (Argentina y sus vecinos del Cono Sur) a medida que se leen.
paises_cono_sur <- c("Argentina", "Chile", "Brazil", "Uruguay",
                      "Bolivia", "Paraguay", "Peru")

datos <- read_csv_chunked(
  "cases_deaths.csv",
  callback = DataFrameCallback$new(function(chunk, pos) {
    chunk %>% filter(country %in% paises_cono_sur)
  }),
  chunk_size = 50000,
  show_col_types = FALSE
)

# Exploración preliminar
glimpse(datos)
summary(datos %>% select(new_cases, total_cases, new_deaths, total_deaths))

# Cantidad de valores faltantes en las variables clave
datos %>%
  summarise(
    na_new_cases  = sum(is.na(new_cases)),
    na_new_deaths = sum(is.na(new_deaths))
  )


# 3. Tipo de dato de la fecha y conversión a formato Date
# =============================================================
class(datos$date)
# readr ya reconoce automáticamente el formato "AAAA-MM-DD" y
# la importa como Date. Si no fuera así (por ejemplo si viniera
# como texto "chr"), lo convertimos con:
datos <- datos %>% mutate(date = lubridate::as_date(date))
class(datos$date)


# 4. Rango de fechas disponible
# =============================================================
rango_fechas <- range(datos$date)
rango_fechas

# El rango de fechas disponible va desde 4 de enero de 2020 hasta el 19 de julio de 2026.


# 5. Evolución diaria de casos confirmados - Argentina
# =============================================================
argentina <- datos %>% filter(country == "Argentina")

graf_casos_arg <- argentina %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = new_cases), color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = new_cases_7_day_avg_right), color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Casos diarios confirmados de COVID-19 - Argentina",
    subtitle = "Línea gris: dato diario crudo | Línea azul: promedio móvil 7 días",
    x = "Fecha", y = "Casos nuevos por día"
  ) +
  theme_minimal()

graf_casos_arg
ggsave("grafico_casos_diarios_argentina.png", graf_casos_arg, width = 9, height = 5, dpi = 150)


# 6. Comparación con vecinos del Cono Sur (Chile, Brasil, Uruguay)
# =============================================================
# Se usa "casos por millón de habitantes" (promedio 7 días) para
# poder comparar países de tamaño de población muy distinto
# (Brasil vs. Uruguay, por ejemplo) en un mismo eje.
comparacion_casos <- datos %>%
  filter(country %in% c("Argentina", "Chile", "Brazil", "Uruguay"))

graf_comparacion_casos <- comparacion_casos %>%
  ggplot(aes(x = date, y = new_cases_per_million_7_day_avg_right, color = country)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Casos diarios de COVID-19 por millón de habitantes",
    subtitle = "Promedio móvil de 7 días | Argentina vs. Chile, Brasil y Uruguay",
    x = "Fecha", y = "Casos nuevos por millón de hab.",
    color = "País"
  ) +
  theme_minimal()

graf_comparacion_casos
ggsave("grafico_comparacion_casos.png", graf_comparacion_casos, width = 9, height = 5, dpi = 150)


# 7. Fallecimientos: diarios y acumulados - Argentina (notacion cientifica)
# =============================================================
graf_muertes_diarias <- argentina %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = new_deaths), color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = new_deaths_7_day_avg_right), color = "firebrick", linewidth = 0.8) +
  labs(
    title = "Fallecimientos diarios por COVID-19 - Argentina",
    subtitle = "Línea gris: dato diario crudo | Línea roja: promedio móvil 7 días",
    x = "Fecha", y = "Fallecimientos por día"
  ) +
  theme_minimal()

graf_muertes_diarias
ggsave("grafico_muertes_diarias_argentina.png", graf_muertes_diarias, width = 9, height = 5, dpi = 150)

graf_muertes_acumuladas <- argentina %>%
  ggplot(aes(x = date, y = total_deaths)) +
  geom_line(color = "firebrick", linewidth = 0.8) +
  labs(
    title = "Fallecimientos acumulados por COVID-19 - Argentina",
    x = "Fecha", y = "Fallecimientos acumulados"
  ) +
  theme_minimal()

graf_muertes_acumuladas
ggsave("grafico_muertes_acumuladas_argentina.png", graf_muertes_acumuladas, width = 9, height = 5, dpi = 150)


# 8. Analisis de  patrón trimestral/estacional en los fallecimientos
# Primero se hace un gráfico de barras por trimestre, comparando año a año.
# =============================================================
argentina <- argentina %>%
  mutate(
    anio = lubridate::year(date),
    trimestre = lubridate::quarter(date),
    mes = lubridate::month(date, label = TRUE, abbr = TRUE)
  )

# a) Fallecimientos totales por trimestre y año
resumen_trimestral <- argentina %>%
  group_by(anio, trimestre) %>%
  summarise(muertes_trimestre = sum(new_deaths, na.rm = TRUE), .groups = "drop")

graf_estacional_trimestre <- resumen_trimestral %>%
  ggplot(aes(x = factor(trimestre), y = muertes_trimestre, fill = factor(anio))) +
  geom_col(position = "dodge") +
  labs(
    title = "Fallecimientos por trimestre - Argentina",
    subtitle = "Comparado año a año",
    x = "Trimestre", y = "Fallecimientos totales del trimestre",
    fill = "Año"
  ) +
  theme_minimal()

graf_estacional_trimestre
ggsave("grafico_estacional_trimestre.png", graf_estacional_trimestre, width = 9, height = 5, dpi = 150)

# b) Distribución de fallecimientos diarios por mes del año
#    (juntando todos los años, para ver si hay meses típicamente
#    más letales, independientemente del año)
graf_estacional_mes <- argentina %>%
  ggplot(aes(x = mes, y = new_deaths)) +
  geom_boxplot(fill = "salmon", outlier.alpha = 0.3) +
  labs(
    title = "Distribución de fallecimientos diarios según mes del año - Argentina",
    subtitle = "Datos de todos los años combinados",
    x = "Mes", y = "Fallecimientos por día"
  ) +
  theme_minimal()

graf_estacional_mes
ggsave("grafico_estacional_mes.png", graf_estacional_mes, width = 9, height = 5, dpi = 150)


# 9. Comparación de fallecidos entre países del Cono Sur
#    (Argentina, Chile, Perú, Bolivia, Paraguay, Brasil)
# =============================================================
paises_comparar <- c("Argentina", "Chile", "Peru", "Bolivia", "Paraguay", "Brazil")

# a) Total de fallecidos por país (valor final de total_deaths)
total_por_pais <- datos %>%
  filter(country %in% paises_comparar) %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(country, total_deaths, total_deaths_per_100k)

total_por_pais

graf_total_pais <- total_por_pais %>%
  ggplot(aes(x = reorder(country, total_deaths), y = total_deaths)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Fallecidos totales por COVID-19 - países del Cono Sur",
    x = "País", y = "Fallecidos totales (valor absoluto)"
  ) +
  theme_minimal()

graf_total_pais
ggsave("grafico_total_fallecidos_pais.png", graf_total_pais, width = 8, height = 5, dpi = 150)

# b) Lo mismo, pero cada 100 mil habitantes (comparación justa,
#    ya que Brasil tiene mucha más población que Uruguay o Paraguay)
graf_total_pais_percapita <- total_por_pais %>%
  ggplot(aes(x = reorder(country, total_deaths_per_100k), y = total_deaths_per_100k)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Fallecidos totales por COVID-19 cada 100.000 habitantes",
    subtitle = "Comparación ajustada por población",
    x = "País", y = "Fallecidos cada 100.000 hab."
  ) +
  theme_minimal()

graf_total_pais_percapita
ggsave("grafico_total_fallecidos_percapita.png", graf_total_pais_percapita, width = 8, height = 5, dpi = 150)

# c) Evolución temporal comparada (fallecidos acumulados por millón)
graf_evolucion_muertes_paises <- datos %>%
  filter(country %in% paises_comparar) %>%
  ggplot(aes(x = date, y = total_deaths_per_million, color = country)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Evolución de fallecidos acumulados por millón de hab.",
    subtitle = "Países del Cono Sur",
    x = "Fecha", y = "Fallecidos acumulados por millón de hab.",
    color = "País"
  ) +
  theme_minimal()

graf_evolucion_muertes_paises
ggsave("grafico_evolucion_muertes_paises.png", graf_evolucion_muertes_paises, width = 9, height = 5, dpi = 150)
