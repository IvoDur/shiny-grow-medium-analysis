# Paquetes necesarios para la app
library(shiny)         # Para correr la app
library(shinythemes)   # Para elegir un theme
library(shinyMatrix)   # Para la matriz de input de los datos
library(shinyjs)       # Para la reactividad del botón 'Confirmar'
library(shinyFeedback) # Para los feedbacks
library(shinyWidgets)  # Para los feedbacks

## Paquetes necesarios para la plantilla
library(tidyverse) # Manejo de datos
library(ggplot2)   # Creación de gráficos
library(ggthemes)  # Manejo de tema de gráficos
library(scales)    # Manejo de escala de gráficos
library(emmeans)   # Comparaciones múltiples post ANOVA
library(dunn.test) # Comparaciones múltiples post KW
library(nortest)   # Test de normalidad

 # ------------------------------- R -------------------------------------------

source(".\\www\\analisis.r")

 # ------------------------------- UI ------------------------------------------

ui <- 
  ## Diseño fluid
  fluidPage( 
    
    ## Funciones que uso para reactividad
    useShinyFeedback(),
    useShinyjs(), 
    
    ## Theme para la app
    theme = shinytheme("yeti"),
    
    ## Icono del logo de Rayen en la pestania del navegador
    list(
      tags$head(
        HTML('<link rel="icon", href="rayen_icono.ico", type="image/ico" />') 
      )
    ),
    
    ## Nombre en la pestania del navegador (cambiar windowTitle="Rayen" si agrego otros analisis más adelante)
    div(
      titlePanel(
        title="", 
        windowTitle="Evidencia Objetiva"
      )
    ), 
    
    ## Diseño con tabs dentro de la Shiny
    navbarPage(
      
      # Diseño de la tab
      tags$style(
        HTML(
          '.navbar-brand {
            padding-top:4px !important;
            padding-bottom:4px !important;
            height: 75px;
          }
          .navbar {min-height:25px !important;}'
        )
      ),
      
      ## Logo de Rayen en la tab
      title=img(style="max-height:100%", src="Rayen_logo_web.png"),
      
      ## Panel para el análisis de evidencia objetiva (se podrían agregar más para futuros análisis)
      tabPanel(
        HTML(
          "<span style='font-size:20px; line-height:60px; text-align:center;'>
          <b> <small> Evidencia Objetiva </small> </b>
          </span>"),
        
        ## Panel izquierdo con la informacion a completar por el usuario
        sidebarPanel(
          
          # Titulo del panel
          tags$h3("Información de la muestra"),
          
          # Numero de partida
          numericInput("partida", "Número de partida", value = 1, min = 1),
          
          # Texto utilizado como validacion en el server
          textOutput("val_partida"),
          textOutput("val_partida2"),
          
          # Medio de crecimiento a analizar
          selectInput("medio", "Medio de crecimiento", c("AM", "MOM", "MOM+AM","PTVM","TR+AM")),
          
          # Especie que depende del medio elegido
          uiOutput("especie"),
          
          # En caso de elegir la opción "Otra" en especie se abre un textInput para completar
          conditionalPanel(
            condition = "input.especie == 'Otra'",
            textInput("especie_otra", "Nueva especie (nombre biológico)")
          ),
          
          # % de CRH "medio"
          numericInput("capacidad", "Capacidad de retención media", value = 30, min= 5, max= 95),
          
          # Texto utilizado como validacion en el server
          textOutput("val_capacidad"),
          
          # Muestra debajo la CRH media y el +-5
          uiOutput("retenciones"),
          
          # Boton de confirmacion para mostrar el main panel
          actionButton("confirmar", "Confirmar"),
          
        ), # sidebarPanel
        
        ## Main panel oculto para mostrarlo luego de que carguen la informacion inicial
        hidden(
          div(
            id = "main",
            
            # Main panel
            mainPanel(
              
              # Titulo del panel
              h1("Datos de análisis"),
              
              # Fecha de analisis y de partida (generalmente no coinciden, entra la partida y se analiza despues)
              fluidRow(
                column(
                  airDatepickerInput(
                    "fecha_analisis", 
                    label = "Fecha de análisis de la partida",
                    autoClose = TRUE,
                    maxDate = Sys.Date(),
                    language = "es"),
                  
                  # Texto utilizado como validacion en el server
                  textOutput("val_fecha_a"),
                  width = 6
                ),
                column(
                  airDatepickerInput(
                    "fecha_partida", 
                    label = "Fecha de ingreso de la partida",
                    view = "months", 
                    minView = "months", 
                    dateFormat = "MMMM", 
                    autoClose = TRUE,
                    maxDate = Sys.Date(),
                    language = "es"),
                  textOutput("val_fecha_p"),
                  width = 6
                ),
              ),
              
              # Matriz donde se cargaran los datos para el analisis
              uiOutput("matrixResultados"),
              
              fluidRow(
                column(
                  # Boton para generar el analisis
                  actionButton("generarAnalisis", "Analizar resultados"),
                  width = 4
                ),
                
                column(
                  # Mostrar los resultados
                  textOutput("resultadosAnalisis"),
                  width = 8
                )
              ),
              
              hidden(
                div(
                  id="botonesCapacidad",
                fluidRow(
                  column(
                    # Boton para generar el analisis
                    actionButton("capac1", "Capacidad - 5"),
                    width = 4
                  ),
                  
                  column(
                    # Boton para generar el analisis
                    actionButton("capac2", "Capacidad"),
                    width = 4
                  ),
                  
                  column(
                    # Mostrar los resultados
                    actionButton("capac3", "Capacidad + 5"),
                    width = 4
                  )
                )
              )
              ),
                
              
              # Boton para generar el PDF
              hidden(
                actionButton("generarPDF", "Generar PDF")
               ),
              
              # Mostrar el pdf generado
              uiOutput("pdfview")
              
            ) # mainPanel
            
          ) # div
          
        ) # hidden
        
      ) # tabPanel
      
    ) # navbarPage
    
  ) # fluidPage


  # ------------------------------ SERVER -------------------------------------

server <- function(input, output) {
  
  ## Mostrar advertencia si el numero de partida pareciera estar fuera de rango
  val_partida <- 
    reactive({
      if(input$partida > 4) {
        feedbackWarning("partida", input$partida > 4, "¿Es correcto el número de partida?")
      } else {
        feedbackDanger("partida", input$partida < 1 | is.na(input$partida), "El número de partida es incorrecto")
      }
    })
  
  output$val_partida <- renderText(val_partida())
  
  ## Condiciona el input de la especie al medio elgido
  output$especie <- renderUI({
    if (input$medio == "AM") {
      elementos <- c("Glycine max", "Otra")
    }
    else if (input$medio == "MOM") {
      elementos <- c("Helianthus annuus", "Otra")
    }
    else if (input$medio == "MOM.AM") {
      elementos <- c("Helianthus annuus", "Otra")
    }
    else if (input$medio == "PTVM") {
      elementos <- c("Lolium sp.", "Sorghum sp.", "Otra")
    }
    else {
      elementos <- c("Glycine max", "Otra")
    }
    selectInput("especie", "Especie", elementos)
  })
  
  
  ## Mostrar error si el % de CRH esta fuera de rango
  val_capacidad <-
    reactive({
      feedbackDanger("capacidad", input$capacidad < 5 | input$capacidad > 95| is.na(input$capacidad), "El % de CRH es incorrecto")
    })
  
  output$val_capacidad <- renderText(val_capacidad())

  
  ## Mostrar main panel cuando se toca confirmar
  observeEvent(
    input$confirmar, {showElement("main")}
  )

  
  ## La fecha de analisis tiene que ser como maximo la del dia que se usa la app
  val_fecha_a <-
    reactive({
      feedbackDanger("fecha_analisis", difftime(as.Date(input$fecha_analisis), Sys.Date(), units = "days") > 0, "La fecha indicada es incorrecta")
    })
  
  output$val_fecha_a <- renderText(val_fecha_a())
  
  
  ## El mes de partida tiene que ser como maximo el mes en el que se usa la app
  val_fecha_p <-
    reactive({
      feedbackDanger("fecha_partida", month(input$fecha_partida) - month(Sys.Date()) < 0, "La fecha indicada es incorrecta!!")
    })
  
  output$val_fecha_p <- renderText(val_fecha_p())
  
  
  ## Matriz interactiva donde se van a cargar los % de plantulas normales
  output$matrixResultados <- renderUI({
    matrixInput(
      "matrixResultados",
      value=matrix(
        nrow = 3,
        ncol = 6,
        dimnames = list(c("Analista 1","Analista 2","Analista 3"),
                        c(paste0(input$capacidad-5,"%||Media"),
                          paste0(input$capacidad-5, "%||Alta"),
                          paste0(input$capacidad, "%||Media"),
                          paste0(input$capacidad, "%||Alta"),
                          paste0(input$capacidad+5,"%||Media"),
                          paste0(input$capacidad+5,"%||Alta")))),
      rows = list(names = TRUE, multiheader=TRUE),
      cols = list(names = TRUE, multiheader=TRUE),
      class = "numeric"
    )
  }) # output$matrixResultados
  
  
  ## Imprime las retenciones a analizar 
  output$retenciones <- renderUI({
    
    # Valida primero, si hay error no imprime
    validate(
      need(input$capacidad > 4 & input$capacidad < 96, " ")
    )
    
    # Porcentajes de CRH a analizar
    tags$h6(sprintf("%% CRH a evaluar: %s / %s / %s", 
                    input$capacidad-5,input$capacidad,input$capacidad+5))
  })
  
  ## Obtencion de los resultados en la app
  observeEvent(input$generarAnalisis, {
    # # Valida que los inputs de partida y de capacidad sean correctos
    # validate(
    #   need(input$capacidad > 4 & input$capacidad < 96, " "),
    #   need(input$partida > 0, " "),
    #   need(!(is.null(input$fecha_analisis)), " "),
    #   need(!(is.null(input$fecha_partida)), " "),
    #   need(!(any(is.na(input$matrixResultados))), " ")
    # )
    
    resultado <- analisis(input$capacidad, input$matrixResultados)
    
    showElement("botonesCapacidad")
    showElement("generarPDF")
    
    output$resultadosAnalisis <- renderText({
      resultado
    })
    
  })
  
  
  ## Renderizacion del pdf
  observeEvent(input$generarPDF, {
    
    # Valida que los inputs de partida y de capacidad sean correctos
    validate(
      need(input$capacidad > 4 & input$capacidad < 96, " "),
      need(input$partida > 0, " "),
      need(!(is.null(input$fecha_analisis)), " "),
      need(!(is.null(input$fecha_partida)), " "),
      need(!(any(is.na(input$matrixResultados))), " ")
    )
        
    # Se cea una ruta temporal para generar el pdf
    base_temp_dir <- tempdir()
    
    # Se manda a dicha direccion el rmd y la imagen para el pdf
    temp_report <- file.path(base_temp_dir, "plantilla.Rmd", fsep = "\\")
    temp_header <- file.path(base_temp_dir, "Rayen_header.png", fsep = "\\")
        
    # Se copian ambos archivos en la ruta temporal
    file.copy(".\\www\\plantilla.Rmd", temp_report, overwrite = TRUE)
    file.copy(".\\www\\Rayen_header.png", temp_header, overwrite = TRUE)
    
    # Se condiciona la especie (ante la posibilidad de "Otra")
    if (input$especie_otra == "") {especie_pdf = input$especie}
    else {especie_pdf = paste0(italic(input$especie_otra))}
    
    # Se obtienen los parametros para la plantilla y se genera el PDF
    params <- list(
      partida = input$partida,
      medio = input$medio,
      capacidad = input$capacidad,
      fechaAnalisis = input$fecha_analisis,
      fechaPartida = input$fecha_partida,
      matriz = input$matrixResultados,
      especie = especie_pdf
    )
    
    rmarkdown::render(
      temp_report, output_file = "prueba.pdf",
      params = params,
      envir = new.env(parent = globalenv())
    )
        
    # Se obtiene la ruta del pdf y se copia en el directorio de la app
    temp_dir_pdf <- file.path(base_temp_dir, "prueba.pdf", fsep = "\\")
    file.copy(temp_dir_pdf, ".\\www\\reporte.pdf", overwrite = TRUE)
        
    # Se muestra el PDF
    output$pdfview <- renderUI({
      pdf("reporte.pdf")
      dev.off()
      tags$iframe(style = "height:600px; width:100%", src = "reporte.pdf")
    })
  }) # observeEvent
} # server


 # ------------------------------ SHINY APP -------------------------------------
shinyApp(ui = ui, server = server)
