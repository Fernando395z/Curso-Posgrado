# Análisis de emergencia de plántulas - El Cuy / Aguada Guzmán
# Pastoreo vs. clausura (sep-2013 a sep-2014)
# =============================================================

library(tidyverse)

# -------------------------------------------------------------
# 0. Carga de datos
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
