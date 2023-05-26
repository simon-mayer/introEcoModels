# install.packages("CircStats")
library(CircStats)


# Modellfunktion

#' Simuliere einen RandomWalk in 2D
#'
#' @param x reelle Zahl, aktuelle X-Koordinate
#' @param y reelle Zahl, aktuelle Y-Koordinate
#' @param speed reelle Zahl, Erwartungswert Geschwindigkeit (Normalverteilung)
#' @param a reelle Zahl 0 ≤ a ≤ 2π: Richtungswinkel der vorigen Bewegung
#' @param rho reele Zahl 0 ≤ rho ≤ 1: Wahrscheinlichkeit des "Beharrens" auf
#'                                    voriger Bewegungsrichtung
#' @returns Vektor von 3 reelen Zahlen: x, y Koordinaten und
#'                                       eingeschlagene Richtung
random2d = function(x, y, a, speed, rho) {

  # Schrittlänge (Normalverteilt, aber nur positiver Teil)
  l = abs(rnorm(n = 1, mean = speed, sd = 1))

  # Ermittle neue Bewegungsrichtung (bereits auf alte addiert)
  alpha = CircStats::rwrpcauchy(n = 1, location = a, rho = rho)

  dx <- l * cos(alpha)
  dy <- l * sin(alpha)

  x = dx + x
  y = dy + y

  return(c(x, y, alpha))
}


random2dUI <- function(id){
  tabPanel("Random Walk 2D",
           fluidRow(
             column(6,
                    sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                value = 100, min = 10, max = 1000, step = 10)),
             column(6,
                    sliderInput(shiny::NS(id, "seed"), "Zufallsexperiment Nr",
                                value = 100, min = 1, max = 200, step = 1))),
           fluidRow(
             column(6,
                    sliderInput(shiny::NS(id, "speed1"), "erwartete Schrittweite (speed) rot",
                                value = 0.5, min = 0.0, max = 1, step = 0.05 )),
             column(6,
                    sliderInput(shiny::NS(id, "rho1"), "Wahrsch. des 'Beharrens' (rho) rot",
                         value = 0, min = 0, max = 1, step = 0.01))),
           fluidRow(
             column(12, plotOutput(NS(id, "twoD_walk")))),
           fluidRow(
             column(6,
                    sliderInput(shiny::NS(id, "speed2"), "erwartete Schrittweite (speed) blau",
                                value = 0.5, min = 0.0, max = 1, step = 0.05 )),
             column(6,
                    sliderInput(shiny::NS(id, "rho2"), "Wahrsch. des 'Beharrens' (rho) blau",
                                value = 0, min = 0, max = 1, step = 0.01))),
           fluidRow(
             column(1, actionButton(NS(id, "twoD_button"), "Show / Hide Function Code",
                                    onClick="show_hide('twoD_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "twoD_box"), id="twoD_function",
                            verbatimTextOutput(NS(id, "twoD_function_text")))))
  )
}




random2dServer <- function(id){
  moduleServer(id, function(input, output, session){
    red <- reactive({
      set.seed(input$seed)
      n = input$end
      x = rep(NA, n+1)
      x[1] = 0
      y = rep(NA, n+1)
      y[1] = 0
      a = rep(NA, n+1)
      a[1] = 0

      for (t in 1:n) {
        xy = random2d(x[t], y[t], a[t], speed = input$speed1, rho = input$rho1)
        x[t+1] = xy[1]
        y[t+1] = xy[2]
        a[t+1] = xy[3]
      }
      data.frame(x=x, y=y, a=a, participant="red")
    })

    blue <- reactive({
      set.seed(input$seed)
      n = input$end
      x = rep(NA, n+1)
      x[1] = 0
      y = rep(NA, n+1)
      y[1] = 0
      a = rep(NA, n+1)
      a[1] = 0

      for (t in 1:n) {
        xy = random2d(x[t], y[t], a[t], speed = input$speed2, rho = input$rho2)
        x[t+1] = xy[1]
        y[t+1] = xy[2]
        a[t+1] = xy[3]
      }
      data.frame(x=x, y=y, a=a, participant="blue")
    })


    output$twoD_walk <- renderPlot({
      df <- rbind(red(), blue())
      ggplot2::ggplot(data = df, ggplot2::aes(x=x, y=y, colour=participant)) +
        ggplot2::geom_path() +
      ggplot2::scale_colour_manual(values = c("blue", "red"))
    })

    twoD_function_text <-"
random2d = function(x, y, a, speed, rho) {
  # Schrittlänge (Normalverteilt, aber nur positiver Teil)
  l = abs(rnorm(n = 1, mean = speed, sd = 1))

  # Ermittle neue Bewegungsrichtung (bereits auf alte addiert)
  alpha = CircStats::rwrpcauchy(n = 1, location = a, rho = rho)

  dx <- l * cos(alpha)
  dy <- l * sin(alpha)

  x = dx + x
  y = dy + y

  return(c(x, y, alpha))
}
"
    output$twoD_function_text <- renderText({twoD_function_text})
  })

}
