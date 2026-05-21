# Trabajo-Final
# Impacto del Precio sobre las Ventas en el Sector Moda

Este proyecto analiza el impacto del precio sobre las ventas en el sector moda. Se estiman modelos econométricos para medir la elasticidad de la demanda, validar su robustez mediante pruebas estadísticas y proyectar ventas a 6 meses con intervalos de confianza. Además, se desarrolla un dashboard interactivo para apoyar decisiones de pricing y planeación comercial.

# Objetivos del proyecto
1. Analizar la relación entre precios y ventas.
2. Estimar elasticidades de demanda mediante un modelo log-log.
3. Validar la calidad del modelo con pruebas de robustez.
4. Proyectar ventas futuras utilizando forecasting.
5. Visualizar resultados en un dashboard interactivo en Shiny.

# ¿Cómo funciona el proyecto?
1. Se cargan y limpian los datos del sector moda.
2. Se construyen variables económicas y transformaciones logarítmicas.
3. Se estima un modelo econométrico para medir el efecto del precio sobre las ventas.
4. Se aplican pruebas de robustez:
     - Breusch-Pagan
     - Durbin-Watson
     - Jarque-Bera
     - VIF
5. Se desarrolla un modelo de forecasting para proyectar ventas a 6 meses.
6. Los resultados se muestran en un dashboard interactivo con gráficos, tablas y simulaciones.

# Resultados principales
- El precio presenta una relación negativa y significativa con las ventas.
- El modelo principal explica aproximadamente el 56.5% de la variación de las ventas.
- El forecasting muestra patrones semanales claros y alta capacidad predictiva.
- El dashboard permite simular escenarios de precios y analizar curvas de demanda.

# Archivos 
Proyecto_Final.R → código completo del análisis y dashboard.

data.csv → base de datos utilizada.

Proyecto_Final_Analitica.docx → analisis completo del los resultados

README.md → descripción general del proyecto.

Gráficos .png y tablas .csv generadas automáticamente.




