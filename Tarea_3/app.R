library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', sans-serif;
        background-color: #f8f9fa;
      }
      h4 {
        font-weight: bold;
        color: #2c3e50;
      }
      .panel-title {
        font-size: 22px;
        font-weight: bold;
      }
      .btn {
        background-color: #0275d8;
        color: white;
      }
      .form-control {
        border-radius: 5px;
      }
    "))
  ),
  
  titlePanel("Sistema de Ecuaciones"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h5("Ingrese las ecuaciones:"),
      textInput("eq1", "➀ Ecuación 1", placeholder = "Ingrese la ecuación"),
      textInput("eq2", "➁ Ecuación 2", placeholder = "Ingrese la ecuación"),
      selectInput("metodo", " Método de resolución:",
                  choices = c("Sustitución", "Igualación", "Reducción")),
      actionButton("resolver", " Resolver", class = "btn btn-primary")
    ),
    
    mainPanel(
      h4(" Resultado:"),
      verbatimTextOutput("resultado"),
      br(),
      tags$hr(),
      tags$p(" Recuerde ingresar bien las ecuaciones. Ejemplo: 2*x + 3*y = 6 ")
    )
  )
)

server <- function(input, output) {
  
  parse_ecuacion <- function(ecuacion_str) {
    ecuacion_str <- gsub(" ", "", ecuacion_str)
    partes <- strsplit(ecuacion_str, "=")[[1]]
    if (length(partes) != 2) return(NULL)
    izquierda <- partes[1]
    derecha <- partes[2]
    f <- function(x = 0, y = 0) eval(parse(text = izquierda))
    a <- (f(x = 1, y = 0) - f(x = 0, y = 0))
    b <- (f(x = 0, y = 1) - f(x = 0, y = 0))
    c <- as.numeric(eval(parse(text = derecha)))
    return(c(a, b, c))
  }
  
  resolver_sustitucion <- function(ec1, ec2) {
    a1 <- ec1[1]; b1 <- ec1[2]; c1 <- ec1[3]
    a2 <- ec2[1]; b2 <- ec2[2]; c2 <- ec2[3]
    
    if (a2 == 0) stop("No se puede despejar x de la segunda ecuación")
    # si es 0
    expr_x <- function(y) (c2 - b2 * y) / a2
    expr_ec1 <- function(y) a1 * expr_x(y) + b1 * y
    #usa sustitución e uniroot() para encontrar el valor de y
    y_val <- uniroot(function(y) expr_ec1(y) - c1, c(-1e6, 1e6))$root
    x_val <- expr_x(y_val)
    
    return(c(x = x_val, y = y_val))
  }
  
  resolver_igualacion <- function(ec1, ec2) {
    a1 <- ec1[1]; b1 <- ec1[2]; c1 <- ec1[3]
    a2 <- ec2[1]; b2 <- ec2[2]; c2 <- ec2[3]
    #Usa igualación para encontrar y
    y_val <- (c1 * a2 - c2 * a1) / (a2 * b1 - a1 * b2)
    x_val <- (c1 - b1 * y_val) / a1
    
    return(c(x = x_val, y = y_val))
  }
  
  resolver_reduccion <- function(ec1, ec2) {
    a1 <- ec1[1]; b1 <- ec1[2]; c1 <- ec1[3]
    a2 <- ec2[1]; b2 <- ec2[2]; c2 <- ec2[3]
    
    mult1 <- a2
    mult2 <- a1
    
    b1_new <- b1 * mult1
    b2_new <- b2 * mult2
    c1_new <- c1 * mult1
    c2_new <- c2 * mult2
    
    y_val <- (c1_new - c2_new) / (b1_new - b2_new)
    x_val <- (c1 - b1 * y_val) / a1
    
    return(c(x = x_val, y = y_val))
  }

  observeEvent(input$resolver, {
    tryCatch({
      ec1 <- parse_ecuacion(input$eq1)
      ec2 <- parse_ecuacion(input$eq2)
      
      if (is.null(ec1) || is.null(ec2)) stop("Formato de ecuación inválido.")
      
      solucion <- switch(input$metodo,
                         "Sustitución" = resolver_sustitucion(ec1, ec2),
                         "Igualación" = resolver_igualacion(ec1, ec2),
                         "Reducción" = resolver_reduccion(ec1, ec2))
      
      output$resultado <- renderText({
        paste0("Método seleccionado: ", input$metodo,
               "\n x = ", round(solucion["x"], 4),
               "\n y = ", round(solucion["y"], 4))
      })
    }, error = function(e) {
      output$resultado <- renderText("Error al procesar la ecuación.")
    })
  })
}

shinyApp(ui, server)
