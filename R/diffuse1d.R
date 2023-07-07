

#'Bestimme aktuelle Konzentration für Feld i
#'
#' @param i Integer, Positionsindex
#' @param x Vektor aus reellen Zahler: Konzentrartion
#' @param delta Diffusionsfaktor
#'
#' @returns reelle Zahl, Konzentration an Stelle i in nächsten
#'          Zeitschritt
diffuse = function(i, x, delta) {
  # periodische Randbedingung
  left = ifelse(i == 1, length(x), i-1)
  right = ifelse(i == length(x), 1, i+1)

  # neuen Wert für xi berechnen
  xi = x[i] - 2*delta*x[i] + delta*x[left] + delta*x[right]
}


diffusion1dUI <- function(id){
  tabPanel("Diffusion 1D",
           fluidRow(
             column(6, sliderInput(shiny::NS(id, "start"), "Anzahl Partikel zu Beginn",
                                value = 100, min = 10, max = 300, step = 10)),
             column(6, sliderInput(shiny::NS(id, "position"),
                                   "relative Position Partikel zu Beginn",
                                value = 0.5, min = 0.01, max = 1, step = 0.01 ))),
           fluidRow(
             column(4, sliderInput(shiny::NS(id, "extension"), "Ausdehnung (length(x))",
                                value = 100, min = 10, max = 200, step = 10 )),
             column(4, sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                value = 200, min = 100, max = 800, step = 50)),
             column(4, sliderInput(shiny::NS(id, "delta"), "Diffusionskonstante (delta)",
                                value = 0.2, min = 0.01, max = 1, step = 0.01))),
           fluidRow(column(4), column(6, h5("zum Startzeitpunkt"))),
           fluidRow(
             column(12, plotOutput(NS(id, "diffusion1d_start")))),
           fluidRow(column(4), column(6, h5(textOutput(NS(id, "endpoint"))))),
           fluidRow(
             column(12, plotOutput(NS(id, "diffusion1d")))),
           fluidRow(
             column(1, actionButton(NS(id, "diffusion1d_button"), "Show / Hide Function Code",
                                    onClick="show_hide('diffusion1d_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "diffusion1d_box"), id="diffusion1d_function",
                            verbatimTextOutput(NS(id, "diffusion1d_function_text")))))
  )
}

diffusion1dServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$endpoint <- renderText({paste0("zum Endzeitpunkt(t = ", input$end, ")")})
    distrib <- reactive({
      x <- rep(0, input$extension)
      x[as.integer(input$extension*input$position)] <- input$start
      for (i in 1:input$end){
        y = sapply(1:length(x), "diffuse", x = x, delta = input$delta)
        x <- y
      }

      data.frame(concentration=x, cells=1:input$extension)
    })
    output$diffusion1d_start <- renderPlot({
      x <- rep(0, input$extension)
      x[as.integer(input$extension*input$position)] <- input$start
      ggplot2::ggplot(data = data.frame(concentration=x, cells=1:input$extension)) +
        ggplot2::geom_col(ggplot2::aes(x=cells, y=concentration))
    })

    output$diffusion1d <- renderPlot({
      ggplot2::ggplot(data = distrib()) +
        ggplot2::geom_col(ggplot2::aes(x=cells, y=concentration))
    })

    diffusion_function_text <- "
    diffuse = function(i, x, delta) {
      # periodische Randbedingung
      left = ifelse(i == 1, length(x), i-1)
      right = ifelse(i == length(x), 1, i+1)

      # neuen Wert für xi berechnen
      xi = x[i] - 2*delta*x[i] + delta*x[left] + delta*x[right]
    }
    "
    output$diffusion1d_function_text <- renderText({diffusion_function_text})
  })
}
