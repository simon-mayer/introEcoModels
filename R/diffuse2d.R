

#' Modelliere Diffusion in 2d
#'
#' @param A reelle Matrix:  Konzentrationen in x,y Koordinaten
#' @param delta  Diffusionskoeffizient, Anteil der Abwanderung in
#'               benachbarte Zellen
#'
#' @returns reelle Matrix: Konzentrationen in x,y Koordinaten nach erfolgter
#'                        Diffusion
#'
diffuse2D = function(A, delta) {

  # V matrix generieren und mit Null auffüllen
  V = matrix(0, nrow = nrow(A)+2, ncol = ncol(A)+2)

  # Indizes für Zeilen und Spalten wo A in V liegt
  rows = 2:(nrow(V)-1)
  cols = 2:(ncol(V)-1)

  # A verschieben und an entsprechender Stelle zu V hinzuaddieren
  V[rows, cols+1] = V[rows, cols+1] + delta*A # nach rechts
  V[rows-1, cols] = V[rows-1, cols] + delta*A # nach oben
  V[rows, cols-1] = V[rows, cols-1] + delta*A # nach links
  V[rows+1, cols] = V[rows+1, cols] + delta*A # nach unten

  # Bilanz für Zellen von A
  Aneu = A - 4*delta*A
  Aneu = Aneu + V[rows, cols]
  return(Aneu)
}


diffusion2dUI <- function(id){
  tabPanel("Diffusion 2D",
          fluidRow(
             column(6, sliderInput(shiny::NS(id, "start"), "Anzahl Partikel zu Beginn",
                                   value = 50, min = 10, max = 200, step = 10)),
             column(6, sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                   value = 100, min = 100, max = 600, step = 50))),

          fluidRow(
             column(6, sliderInput(shiny::NS(id, "positionX"),
                                   "relative Position Partikel zu Beginn X",
                                   value = 0.5, min = 0.01, max = 1, step = 0.01 )),
             column(6, sliderInput(shiny::NS(id, "positionY"),
                                   "relative Position Partikel zu Beginn Y",
                                   value = 0.5, min = 0.01, max = 1, step = 0.01 ))),
           fluidRow(
             column(6, sliderInput(shiny::NS(id, "extension"), "Ausdehnung  (ncol(A))",
                                   value = 50, min = 10, max = 100, step = 10 )),
             column(6, sliderInput(shiny::NS(id, "delta"), "Diffusionskonstante (delta)",
                                   value = 0.05, min = 0.01, max = 1, step = 0.01))),
           fluidRow(
             column(12, plotOutput(NS(id, "diffusion2d_start")))),
           fluidRow(
             column(12, plotOutput(NS(id, "diffusion2d")))),
           fluidRow(
             column(1, actionButton(NS(id, "diffusion2d_button"), "Show / Hide Function Code",
                                    onClick="show_hide('diffusion2d_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "diffusion2d_box"), id="diffusion2d_function",
                            verbatimTextOutput(NS(id, "diffusion2d_function_text")))))
  )
}

diffusion2dServer <- function(id){
  moduleServer(id, function(input, output, session){
    distrib <- reactive({
      A = matrix(0, nrow = input$extension, ncol = input$extension)
      positionX <- as.integer(input$extension * input$positionX)
      positionY <- as.integer(input$extension * input$positionY)
      A[positionY, positionX] = input$start # Menge in einer Zelle

      # Modell anwenden
      for (t in 1:input$end) {
        A = diffuse2D(A, delta = input$delta)
      }
      ad <- as.data.frame(A)
      names(ad) <- 1:input$extension
      col_names <- names(ad)
      ad$y_coord <- 1:input$extension
      along <- tidyr::pivot_longer(ad, cols=col_names, names_to="x_coord", values_to="concentration")
      along$x_coord <- as.integer(along$x_coord)
      along
    })

    output$diffusion2d_start <- renderPlot({
      A_start = matrix(0, nrow = input$extension, ncol = input$extension)
      positionX <- as.integer(input$extension * input$positionX)
      positionY <- as.integer(input$extension * input$positionY)
      A_start[positionY, positionX] = input$start # Menge in einer Zelle

      ad_start <- as.data.frame(A_start)
      names(ad_start) <- 1:input$extension
      col_names <- names(ad_start)
      ad_start$y_coord <- 1:input$extension
      along_start <- tidyr::pivot_longer(ad_start, cols=col_names, names_to="x_coord", values_to="concentration")
      along_start$x_coord <- as.integer(along_start$x_coord)
      ggplot2::ggplot(data = along_start) +
        ggplot2::geom_raster(ggplot2::aes(x=x_coord, y=y_coord, fill=concentration)) +
        ggplot2::scale_colour_viridis_c(aesthetics="fill")
    })

    output$diffusion2d <- renderPlot({
      ggplot2::ggplot(data = distrib()) +
        ggplot2::geom_raster(ggplot2::aes(x=x_coord, y=y_coord, fill=concentration)) +
        ggplot2::scale_colour_viridis_c(aesthetics ="fill")
    })

    diffusion2d_function_text <- "
diffuse2D = function(A, delta) {

  # V matrix generieren und mit Null auffüllen
  V = matrix(0, nrow = nrow(A)+2, ncol = ncol(A)+2)

  # Indizes für Zeilen und Spalten wo A in V liegt
  rows = 2:(nrow(V)-1)
  cols = 2:(ncol(V)-1)

  # A verschieben und an entsprechender Stelle zu V hinzuaddieren
  V[rows, cols+1] = V[rows, cols+1] + delta*A # nach rechts
  V[rows-1, cols] = V[rows-1, cols] + delta*A # nach oben
  V[rows, cols-1] = V[rows, cols-1] + delta*A # nach links
  V[rows+1, cols] = V[rows+1, cols] + delta*A # nach unten

  # Bilanz für Zellen von A
  Aneu = A - 4*delta*A
  Aneu = Aneu + V[rows, cols]
  return(Aneu)
}"
    output$diffusion2d_function_text <- renderText({diffusion2d_function_text})
  })
}
