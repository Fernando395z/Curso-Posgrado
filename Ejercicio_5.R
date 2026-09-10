# TP2 - Ejercicio 5: ARPU telefonía móvil
# =============================================================

# Paso 0: usar la carpeta de librerías nueva (para evitar el bloqueo de Windows)
.libPaths("C:/R_library")
# =============================================================

# Paso 1: fijar carpeta de trabajo
setwd("C:/Leonardo Fuentealba/Curso Ciencia de Datos/Trabajos Practicos")
# =============================================================

# Paso 2: cargar el paquete
library(tidyverse, lib.loc = "C:/R_library")
# =============================================================

# Paso 3: Ítem 1 importar los CSV
accesos_movil <- read_csv("Telefonía móvil Accesos Totales.csv")
ingresos_movil <- read_csv("Telefonía móvil Ingresos.csv")

print(str(accesos_movil))
print(str(ingresos_movil))
# =============================================================


# Paso 4: limpiar y renombrar accesos_movil
accesos_movil <- accesos_movil %>%
  rename(
    accesos_pospago = `Total de accesos pospago`,
    accesos_prepago = `Total de accesos prepago`,
    accesos_operativos = `Total de accesos operativos`
  ) %>%
  mutate(
    across(c(accesos_pospago, accesos_prepago, accesos_operativos),
           ~ as.numeric(str_remove_all(., "\\.")))
  )

print(str(accesos_movil))
# =============================================================

# Paso 5: limpiar ingresos_movil
ingresos_movil <- ingresos_movil %>%
  mutate(
    Ingresos = Ingresos %>%
      str_remove("\\$") %>%           # saca el signo peso
      str_remove_all("\\s") %>%        # saca espacios
      str_remove_all("\\.") %>%        # saca puntos de miles
      str_replace(",", ".") %>%        # cambia la coma decimal por punto
      as.numeric()
  )

print(str(ingresos_movil))

# =============================================================
# Paso 6. Item 3: unir accesos e ingresos, calcular accesos promedio, y filtrar el período pedido
arpu_datos <- accesos_movil %>%
  select(Año, Trimestre, accesos_operativos) %>%
  inner_join(ingresos_movil, by = c("Año", "Trimestre")) %>%
  arrange(Año, Trimestre) %>%
  mutate(
    A_barra = (accesos_operativos + lag(accesos_operativos)) / 2
  ) %>%
  filter(Año >= 2020, Año <= 2025)

print(arpu_datos)
# =============================================================

# Paso 7: Item 3: calcular ARPU nominal
# El título original del dataset es "Ingresos ($ miles)" -> el valor está en miles de pesos
arpu_datos <- arpu_datos %>%
  mutate(
    Ingresos_pesos = Ingresos * 1000,
    ARPU_nom = Ingresos_pesos / (A_barra * 3)
  )

print(arpu_datos %>% select(Año, Trimestre, Ingresos_pesos, A_barra, ARPU_nom))
# =============================================================

# Paso 8: gráfico de evolución del ARPU nominal
arpu_datos <- arpu_datos %>%
  mutate(periodo = Año + (Trimestre - 1) / 4)

ggplot(arpu_datos, aes(x = periodo, y = ARPU_nom)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 1.5, color = "darkgreen") +
  labs(
    title = "Evolución del ARPU nominal (telefonía móvil)",
    x = "Año",
    y = "ARPU nominal ($ por mes)"
  ) +
  theme_minimal()
# =============================================================

# Paso 9: Item 4 - obtener tipo de cambio mayorista vía API (httr2)

#install.packages("httr2", lib = "C:/R_library")
library(httr2, lib.loc = "C:/R_library")

url_dolar <- paste0(
  "https://apis.datos.gob.ar/series/api/series/?",
  "ids=168.1_T_CAMBIOR_D_0_0_26",
  "&start_date=2019-10-01",
  "&end_date=2025-12-31",
  "&collapse=month",
  "&collapse_aggregation=avg",
  "&format=csv"
)

dolar_mensual <- request(url_dolar) |>
  req_user_agent("Mozilla/5.0") |>
  req_perform() |>
  resp_body_raw() |>
  read_csv()

print(head(dolar_mensual))
print(str(dolar_mensual))
# =============================================================

# Paso 10: convertir tipo de cambio mensual a promedio trimestral
tc_trimestral <- dolar_mensual %>%
  rename(TC = tipo_cambio_bna_vendedor) %>%
  mutate(
    Año = year(indice_tiempo),
    Trimestre = quarter(indice_tiempo)
  ) %>%
  group_by(Año, Trimestre) %>%
  summarise(TC_promedio = mean(TC), .groups = "drop")

print(tc_trimestral)
# =============================================================

# Paso 11: unir tipo de cambio con ARPU, y calcular ARPU en USD
arpu_datos <- arpu_datos %>%
  inner_join(tc_trimestral, by = c("Año", "Trimestre")) %>%
  mutate(ARPU_usd = ARPU_nom / TC_promedio)

print(arpu_datos %>% select(Año, Trimestre, ARPU_nom, TC_promedio, ARPU_usd))
# =============================================================

# Paso 12: tabla completa y gráfico comparando con el ARPU regional
print(arpu_datos %>% select(Año, Trimestre, ARPU_nom, TC_promedio, ARPU_usd), n = 24)

ggplot(arpu_datos, aes(x = periodo, y = ARPU_usd)) +
  geom_line(linewidth = 1, color = "purple") +
  geom_point(size = 1.5, color = "purple") +
  geom_hline(yintercept = 6.5, linetype = "dashed", color = "red") +
  labs(
    title = "ARPU en dólares (telefonía móvil) vs. promedio regional",
    subtitle = "Línea roja punteada = promedio regional (USD 6,5)",
    x = "Año",
    y = "ARPU (USD/mes)"
  ) +
  theme_minimal()
