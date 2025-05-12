library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)
library(thematic)
library(shinyWidgets)

# Activar tema oscuro automático
thematic::thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"), # o "darkly", "flatly", etc.
  titlePanel(" Calculadora de exposición por distancia para Cyanotipos (Ley del inverso del cuadrado)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("d0", "Distancia inicial (cm):", value = 10, min = 0.1),
      numericInput("t0", "Tiempo de exposición inicial (min):", value = 3, min = 0.1),
      sliderInput("rango", "Rango de distancias (cm):", min = 1, max = 100, value = c(1, 30)),
      
      helpText(
        "Esta app calcula el tiempo de exposición necesario para otras distancias, ",
        "siguiendo la ley del inverso del cuadrado:"
      ),
      tags$blockquote("Intensidad ∝ 1 / distancia²"),
      br(),
      
      h4("⏱ Cronómetro de exposición"),
      uiOutput("reloj_display"),
      actionGroupButtons(
        inputIds = c("start", "pause", "reset"),
        labels = c("Iniciar", "Pausar", "Reiniciar"),
        status = c("success", "warning", "danger")
      ),
      helpText("Cronómetro útil para tiempos de exposición manual.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica interactiva", plotlyOutput("plot")),
        tabPanel("Tabla de tiempos", DTOutput("tabla"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Calcular tiempos estimados
  datos <- reactive({
    dist <- seq(input$rango[1], input$rango[2], by = 0.5)
    tiempo <- input$t0 * (dist / input$d0)^2
    data.frame(Distancia_cm = dist, Tiempo_min = round(tiempo, 2))
  })
  
  # Gráfico interactivo
  output$plot <- renderPlotly({
    p <- ggplot(datos(), aes(x = Distancia_cm, y = Tiempo_min)) +
      geom_line(color = "#0072B2", size = 1.5) +
      geom_point(color = "#D55E00", size = 3) +
      geom_vline(xintercept = input$d0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Exposición según distancia",
        x = "Distancia (cm)",
        y = "Tiempo de exposición (min)"
      ) +
      theme_minimal(base_size = 15)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Tabla con filtro y búsqueda
  output$tabla <- renderDT({
    datatable(datos(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Cronómetro
  tiempo <- reactiveVal(0)
  activo <- reactiveVal(FALSE)
  
  observeEvent(input$start, { activo(TRUE) })
  observeEvent(input$pause, { activo(FALSE) })
  observeEvent(input$reset, {
    activo(FALSE)
    tiempo(0)
  })
  
  auto_timer <- reactiveTimer(1000)
  
  observe({
    auto_timer()
    isolate({
      if (activo()) tiempo(tiempo() + 1)
    })
  })
  
  output$reloj_display <- renderUI({
    min <- floor(tiempo() / 60)
    seg <- tiempo() %% 60
    prettyTime <- sprintf("%02d:%02d", min, seg)
    
    tags$h3(prettyTime, style = "font-family:monospace; color:#00B894; text-align:center;")
  })
}

shinyApp(ui = ui, server = server)
