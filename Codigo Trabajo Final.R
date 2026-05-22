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

# =========================================================
# MODELO
# =========================================================

modelo_datos <- sku_diario %>%
  filter(
    ventas > 0,
    precio > 0,
    costo > 0
  ) %>%
  mutate(
    ln_ventas = log(ventas),
    ln_precio = log(precio),
    ln_costo  = log(costo)
  )

modelo_ols <- lm(
  ln_ventas ~
    ln_precio +
    ln_costo +
    category +
    brand,
  data = modelo_datos
)

summary(modelo_ols)

# R cuadrado
r2 <- summary(modelo_ols)$r.squared
print(paste("R² =", round(r2,4)))

# =========================================================
# PRUEBAS DE ROBUSTEZ
# =========================================================

# Breusch-Pagan
bp_test <- bptest(modelo_ols)

# Errores robustos
robust_se <- coeftest( modelo_ols, vcov = vcovHC(modelo_ols,type = "HC3"))

# Jarque-Bera
jb_test <- jarque.bera.test(
  residuals(modelo_ols))

# Durbin-Watson
dw_test <- dwtest(modelo_ols)

# =========================================================
# VIF Y MODELO ALTERNATIVO
# =========================================================

vif_values <- tryCatch({
  
  vif(modelo_ols)
  
}, error = function(e) {
  
  NULL
  
})

# Modelo alternativo
modelo_alt <- lm(
  ln_ventas ~ price_tier + ln_costo + category + brand,
  data = modelo_datos
)

print(paste("R² modelo original:", round(summary(modelo_ols)$r.squared,4)))
print(paste("R² modelo alternativo:", round(summary(modelo_alt)$r.squared,4)))

# =========================================================
# FORECAST
# =========================================================
diario_modelo <- diario %>%
  mutate(
    trend = dia_num,
    lunes     = as.integer(wday(date) == 2),
    martes    = as.integer(wday(date) == 3),
    miercoles = as.integer(wday(date) == 4),
    jueves    = as.integer(wday(date) == 5),
    viernes   = as.integer(wday(date) == 6),
    sabado    = as.integer(wday(date) == 7)
  )

modelo_ts <- lm(
  ventas_totales ~ trend + lunes + martes +
    miercoles + jueves + viernes + sabado,
  data = diario_modelo
)

summary(modelo_ts)

# FORECAST 6 MESES
n_forecast <- 180

fechas_futuras <- seq(
  max(diario$date) + 1,
  by = "day",
  length.out = n_forecast
)

nuevos_datos <- data.frame(
  date = fechas_futuras,
  dia_num = max(diario$dia_num) + 1:n_forecast
) %>%
  mutate(
    trend     = dia_num,
    lunes     = as.integer(wday(date) == 2),
    martes    = as.integer(wday(date) == 3),
    miercoles = as.integer(wday(date) == 4),
    jueves    = as.integer(wday(date) == 5),
    viernes   = as.integer(wday(date) == 6),
    sabado    = as.integer(wday(date) == 7)
  )

pred_intervalo <- predict(
  modelo_ts,
  newdata = nuevos_datos,
  interval = "prediction",
  level = 0.95
)

forecast_df <- data.frame(
  fecha = fechas_futuras,
  prediccion = pred_intervalo[, "fit"],
  ic_inf = pmax(pred_intervalo[, "lwr"], 0),
  ic_sup = pred_intervalo[, "upr"]
)

# =========================================================
# TABLA COEFICIENTES
# =========================================================

tabla_coef <- tidy(modelo_ols) %>%
  mutate(
    Significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    across(where(is.numeric), ~ round(., 4))
  ) %>%
  rename(Variable = term, Coeficiente = estimate,
         Error_Std = std.error, t = statistic, p_valor = p.value)

write.csv(tabla_coef, "tabla_coeficientes.csv", row.names = FALSE)
# =========================================================
# UI
# =========================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard Moda"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Resumen",
        tabName = "resumen",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Forecast",
        tabName = "forecast",
        icon = icon("calendar")
      ),
      
      menuItem(
        "Correlaciones",
        tabName = "corr",
        icon = icon("project-diagram")
      ),
      
      menuItem(
        "Modelo",
        tabName = "modelo",
        icon = icon("calculator")
      ),
      
      menuItem(
        "Robustez",
        tabName = "robustez",
        icon = icon("shield-alt")
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ===================================================
      # RESUMEN
      # ===================================================
      
      tabItem(
        
        tabName = "resumen",
        
        fluidRow(
          
          valueBox(
            round(sum(datos$sales)),
            "Ventas Totales",
            icon = icon("shopping-cart"),
            color = "blue"
          ),
          
          valueBox(
            round(mean(datos$price),2),
            "Precio Promedio",
            icon = icon("dollar-sign"),
            color = "green"
          ),
          
          valueBox(
            round(summary(modelo_ols)$r.squared,3),
            "R² Modelo",
            icon = icon("chart-line"),
            color = "yellow"
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Ventas por Categoría",
            
            plotlyOutput("plot_categoria")
          )
          
        )
        
      ),
      
      # ===================================================
      # FORECAST
      # ===================================================
      
      tabItem(
        
        tabName = "forecast",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Forecast de Ventas",
            
            plotlyOutput("forecast_plot")
          )
          
        )
        
      ),
      
      # ===================================================
      # CORRELACIONES
      # ===================================================
      
      tabItem(
        
        tabName = "corr",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Matriz Correlación",
            
            plotOutput("corrplot")
          ),
          
          box(
            width = 6,
            title = "Precio vs Ventas",
            
            plotlyOutput("precio_plot")
          )
          
        )
        
      ),
      
      # ===================================================
      # MODELO
      # ===================================================
      
      tabItem(
        
        tabName = "modelo",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Coeficientes Modelo",
            
            DTOutput("tabla_modelo")
          )
          
        )
        
      ),
      
      # ===================================================
      # ROBUSTEZ
      # ===================================================
      
      tabItem(
        
        tabName = "robustez",
        
        fluidRow(
          
          valueBox(
            value = round(bp_test$p.value, 4),
            subtitle = "Breusch-Pagan p-value",
            icon = icon("balance-scale"),
            color = "red"
          ),
          
          valueBox(
            value = round(jb_test$p.value, 4),
            subtitle = "Jarque-Bera p-value",
            icon = icon("chart-area"),
            color = "yellow"
          ),
          
          valueBox(
            value = round(dw_test$statistic, 4),
            subtitle = "Durbin-Watson",
            icon = icon("wave-square"),
            color = "blue"
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Errores Robustos HC3",
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput("tabla_robusta")
          ),
          
          box(
            width = 6,
            title = "Variance Inflation Factor (VIF)",
            status = "warning",
            solidHeader = TRUE,
            
            DTOutput("tabla_vif")
          )
          
        )
        
      )
      
    )
    
  )
  
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =======================================================
  #  GRAFICO VENTAS POR CATEGORÍA
  # =======================================================
  
  output$plot_categoria <- renderPlotly({
    
    p <- datos %>%
      
      group_by(category, date) %>%
      
      summarise(
        ventas = sum(sales),
        .groups = "drop"
      ) %>%
      
      ggplot(
        aes(
          x = date,
          y = ventas,
          color = category
        )
      ) +
      
      geom_line() +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # =======================================================
  # GRAFICO FORECAST
  # =======================================================
  
  output$forecast_plot <- renderPlotly({
    
    p <- ggplot() +
      
      geom_ribbon(
        data = forecast_df,
        aes(
          x = fecha,
          ymin = ic_inf,
          ymax = ic_sup
        ),
        fill = "#2E6DA4",
        alpha = 0.2
      ) +
      
      geom_line(
        data = diario,
        aes(
          x = date,
          y = ventas_totales
        ),
        color = "#1B3A5C",
        linewidth = 1
      ) +
      
      geom_line(
        data = forecast_df,
        aes(
          x = fecha,
          y = prediccion
        ),
        color = "#E8A020",
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      scale_y_continuous(
        labels = label_comma()
      ) +
      
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y"
      ) +
      
      labs(
        title = "Forecasting de Ventas",
        x = "Fecha",
        y = "Ventas"
      ) +
      
      theme_minimal()
    
    ggsave(
      filename = "grafico_forecast.png",
      plot = p,
      width = 12,
      height = 6,
      dpi = 300
    )
    
    ggplotly(p)
    
  })
  # =======================================================
  # GRAFICO MATRIZ DE CORRELACIONES
  # =======================================================
  
  output$corrplot <- renderPlot({
    
    p_corr <- ggcorrplot(
      cor_matrix,
      lab = TRUE
    )
    
    ggsave( filename = "grafico_correlaciones.png",
            plot = p_corr,
            width = 8,
            height = 6,
            dpi = 300
    )
    
    p_corr
    
  })
  
  # =======================================================
  # GRAFICO PRECIO VS VENTAS
  # =======================================================
  
  output$precio_plot <- renderPlotly({
    
    p_precio <- ggplot(
      sku_diario,
      aes(
        x = precio,
        y = ventas,
        color = category
      )
    ) +
      
      geom_point(alpha = 0.3) +
      
      geom_smooth(
        method = "lm"
      ) +
      
      theme_minimal()
    
    ggsave( filename = "grafico_precio_ventas.png",
            plot = p_precio,
            width = 10,
            height = 6,
            dpi = 300
    )
    
    ggplotly(p_precio)
    
  })
  
  # =======================================================
  # TABLA MODELO
  # =======================================================
  
  output$tabla_modelo <- renderDT({
    
    write.csv( tabla_coef, "tabla_modelo.csv", row.names = FALSE)
    
    datatable( tabla_coef, options = list( pageLength = 10,scrollX = TRUE))
  })
  
  # =======================================================
  # TABLA ROBUSTA
  # =======================================================
  
  output$tabla_robusta <- renderDT({
    
    robust_df <- data.frame(
      Variable   = rownames(robust_se),
      Estimate   = robust_se[,1],
      Std_Error  = robust_se[,2],
      t_value    = robust_se[,3],
      p_value    = robust_se[,4]
    )
    
    robust_df <- robust_df %>%
      mutate(
        across(
          where(is.numeric),
          ~ round(., 6)
        )
      )
    
    write.csv( robust_df, "tabla_robusta.csv", row.names = FALSE)
    
    datatable(robust_df, options = list( pageLength = 10, scrollX = TRUE),
              rownames = FALSE) })
  # =======================================================
  # TABLA VIF
  # =======================================================
  
  output$tabla_vif <- renderDT({
    
    if (is.null(vif_values)) {
      
      vif_df <- data.frame(
        Mensaje = "No fue posible calcular VIF"
      )
      
    } else {
      
      vif_df <- tryCatch({
        
        if (is.vector(vif_values)) {
          
          data.frame(
            Variable = names(vif_values),
            VIF = as.numeric(vif_values)
          )
          
        } else {
          
          vif_temp <- as.data.frame(vif_values)
          
          vif_temp$Variable <- rownames(vif_temp)
          
          rownames(vif_temp) <- NULL
          
          vif_temp
          
        }
        
      }, error = function(e) {
        
        data.frame(Mensaje = "Error calculando VIF")
        
      })
    }
    
    write.csv( vif_df,"tabla_vif.csv", row.names = FALSE)
    datatable( vif_df, options = list( pageLength = 10, scrollX = TRUE))
    
  })
} 

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui, server)
