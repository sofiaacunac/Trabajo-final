# PROYECTO FINAL — ANALÍTICA DE DATOS
# Impacto de los Precios en las Ventas del Sector Moda

# =========================================================
# LIBRERÍAS
# =========================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(forecast)
library(tseries)
library(lmtest)
library(sandwich)
library(car)
library(scales)
library(broom)
library(ggcorrplot)
library(lubridate)
library(DT)
library(plotly)
library(rsconnect)
library(readr)

# =========================================================
# CARGA Y LIMPIEZA DE DATOS
# =========================================================

datos_raw <- read_csv("data.csv",show_col_types = FALSE)

datos <- datos_raw %>% mutate(
  date       = ymd(date),
  sales      = as.numeric(sales),
  price      = as.numeric(price),
  cost       = as.numeric(cost),
  category   = as.factor(category),
  brand      = as.factor(brand),
  gender     = as.factor(gender),
  price_tier = as.factor(price_tier),
  style      = as.factor(style)
) %>%
  filter(
    !is.na(sales),
    !is.na(price),
    !is.na(cost))

# =========================================================
# VARIACIÓN DE PRECIO 
# Esta variable NO es descuento real, es un PROXY de variación de precio
# =========================================================

datos <- datos %>%
  group_by(sku_id) %>%
  mutate(
    precio_promedio_sku =
      mean(price, na.rm = TRUE),
    
    variacion_precio =
      precio_promedio_sku - price,
    
    pct_variacion_precio =
      (variacion_precio /
         precio_promedio_sku) * 100
  ) %>%
  ungroup()

# =========================================================
# AGREGACIÓN DIARIA
# =========================================================

diario <- datos %>%
  group_by(date) %>%
  summarise(
    ventas_totales =
      sum(sales, na.rm = TRUE),
    
    precio_promedio =
      mean(price, na.rm = TRUE),
    
    costo_promedio =
      mean(cost, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    dia_num = row_number()
  )

# =========================================================
# SKU DIARIO
# =========================================================

sku_diario <- datos %>%
  group_by(date, sku_id, category, brand, price_tier) %>%
  summarise(
    ventas = sum(sales, na.rm = TRUE),
    precio = mean(price, na.rm = TRUE),
    costo  = mean(cost, na.rm = TRUE),
    variacion = mean(pct_variacion_precio, na.rm = TRUE),
    .groups = "drop"
  )

#MATRIZ DE CORRELACIÓN

cor_matrix <- cor(
  sku_diario[, c("ventas","precio","costo","variacion")],
  use = "complete.obs"
)

print(cor_matrix)
