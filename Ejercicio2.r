library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # Extensión de shinydashboard
library(readr)
library(dplyr)
library(ggplot2)
library(nortest) # Para pruebas de normalidad adicionales
library(DescTools) # Para eta cuadrado

# Definir la interfaz de usuario (UI)
ui <- dashboardPagePlus(
  # Encabezado
  header = dashboardHeaderPlus(
    title = "Análisis Comparativo de Libros Impresos",
    enable_rightsidebar = TRUE, # Habilitar barra lateral derecha
    rightSidebarIcon = "filter" # Ícono para la barra lateral derecha
  ),
  
  # Barra lateral izquierda
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "summary", icon = icon("info-circle")),
      menuItem("Visualizaciones", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Pruebas Estadísticas", tabName = "stats", icon = icon("calculator"))
    )
  ),
  
  # Cuerpo principal
  body = dashboardBody(
    tabItems(
      # Pestaña de Resumen
      tabItem(tabName = "summary",
              h2("Resumen de Datos"),
              verbatimTextOutput("data_summary")
      ),
      
      # Pestaña de Visualizaciones
      tabItem(tabName = "visualizations",
              fluidRow(
                box(plotOutput("histogram"), width = 6),
                box(plotOutput("boxplot"), width = 6)
              ),
              box(plotOutput("qq_plot"), width = 12)
      ),
      
      # Pestaña de Pruebas Estadísticas
      tabItem(tabName = "stats",
              h2("Resultados de Pruebas Estadísticas"),
              verbatimTextOutput("normality_tests"),
              verbatimTextOutput("kruskal_test"),
              verbatimTextOutput("posthoc_results"),
              verbatimTextOutput("eta_squared")
      )
    )
  ),
  
  # Barra lateral derecha (opcional)
  rightsidebar = rightSidebar(
    background = "dark", # Fondo oscuro
    rightSidebarTabContent(
      id = 1,
      title = "Filtros",
      icon = "filter",
      sliderInput("sample_size", "Tamaño de muestra:", min = 3, max = 5000, value = 1000)
    )
  ),
  
  # Configuración del skin
  skin = "purple" # Cambia el color del tema
)

# Definir el servidor
server <- function(input, output) {
  # Cargar datos
  data <- reactive({
    read_delim("datos.csv", delim = ";", col_names = TRUE, locale = locale(encoding = "latin1"))
  })
  
  # Limpieza de datos
  clean_data <- reactive({
    data() %>%
      mutate(P601 = ifelse(is.na(P601), median(P601, na.rm = TRUE), P601)) %>%
      filter(!is.na(ESTRATOSOCIO)) %>%
      mutate(ESTRATOSOCIO = as.factor(ESTRATOSOCIO))
  })
  
  # Filtrar outliers
  filtered_data <- reactive({
    Q1 <- quantile(clean_data()$P601, 0.25, na.rm = TRUE)
    Q3 <- quantile(clean_data()$P601, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    clean_data() %>%
      filter(P601 >= lower_bound & P601 <= upper_bound)
  })
  
  # Resumen de datos
  output$data_summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Histograma
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = P601)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
      geom_density(color = "red") +
      labs(title = "Histograma de P601", x = "P601", y = "Densidad")
  })
  
  # Boxplot por estrato
  output$boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = ESTRATOSOCIO, y = P601)) +
      geom_boxplot(fill = "orange", alpha = 0.7) +
      labs(title = "Boxplot por Estrato Socioeconómico", x = "Estrato", y = "P601")
  })
  
  # Q-Q Plot
  output$qq_plot <- renderPlot({
    ggplot(filtered_data(), aes(sample = P601)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot de P601", x = "Cuantiles teóricos", y = "Cuantiles muestrales")
  })
  
  # Pruebas de normalidad con nortest
  output$normality_tests <- renderPrint({
    subset_data <- filtered_data() %>% sample_n(min(input$sample_size, nrow(.)))
    
    # Verificar el tamaño de muestra
    if (nrow(subset_data) < 3) {
      return("El tamaño de muestra es demasiado pequeño para realizar la prueba de Shapiro-Wilk.")
    }
    
    # Shapiro-Wilk (base R)
    shapiro_test <- tryCatch({
      shapiro.test(subset_data$P601)
    }, error = function(e) {
      return(paste("Error en la prueba de Shapiro-Wilk:", e$message))
    })
    
    # Anderson-Darling (nortest)
    ad_test <- tryCatch({
      ad.test(subset_data$P601)
    }, error = function(e) {
      return(paste("Error en la prueba de Anderson-Darling:", e$message))
    })
    
    # Lilliefors (Kolmogorov-Smirnov) (nortest)
    lillie_test <- tryCatch({
      lillie.test(subset_data$P601)
    }, error = function(e) {
      return(paste("Error en la prueba de Lilliefors:", e$message))
    })
    
    list(
      "Prueba de Shapiro-Wilk" = shapiro_test,
      "Prueba de Anderson-Darling" = ad_test,
      "Prueba de Lilliefors" = lillie_test
    )
  })
  
  # Prueba de Kruskal-Wallis
  output$kruskal_test <- renderPrint({
    kruskal_test <- kruskal.test(P601 ~ ESTRATOSOCIO, data = filtered_data())
    kruskal_test
  })
  
  # Análisis post-hoc (Mann-Whitney U)
  output$posthoc_results <- renderPrint({
    pairwise_results <- pairwise.wilcox.test(
      filtered_data()$P601,
      filtered_data()$ESTRATOSOCIO,
      p.adjust.method = "bonferroni" # Ajuste de Bonferroni
    )
    pairwise_results
  })
  
  # Cálculo de eta cuadrado
  output$eta_squared <- renderPrint({
    kruskal_test <- kruskal.test(P601 ~ ESTRATOSOCIO, data = filtered_data())
    N <- nrow(filtered_data())
    H <- kruskal_test$statistic
    eta_squared <- (H - (length(unique(filtered_data()$ESTRATOSOCIO)) - 1)) / (N - 1)
    eta_squared
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
