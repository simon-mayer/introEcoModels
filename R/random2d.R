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
                                value = 100, min = 10, max = 300, step = 10)),
             column(6,
                    numericInput(shiny::NS(id, "participants"), "Anzahl Simulationen",
                                 value = 1, min=1, max=10))),
           fluidRow(
             column(6,
                    sliderInput(shiny::NS(id, "speed"), "erwartete Schrittweite (speed)",
                                value = 0.5, min = 0.0, max = 1, step = 0.05 )),
             column(6,
                    sliderInput(shiny::NS(id, "rho"), "Richtungstendenz (rho)",
                         value = 0, min = 0, max = 1, step = 0.01))),
           fluidRow(
             column(12, plotOutput(NS(id, "twoD_walk")))),
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

    walk <- reactive({
      set.seed(121212)
      some_letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                       "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
                       "x", "y", "z")

      result <- tibble::tibble(x=numeric(), y=numeric(), a=numeric(), participant=character())
      for (participant in some_letters[1:input$participants]){
        n = input$end
        x = rep(NA, n+1)
        x[1] = 0
        y = rep(NA, n+1)
        y[1] = 0
        a = rep(NA, n+1)
        a[1] = 0

        for (t in 1:n) {
          xy = random2d(x[t], y[t], a[t], speed = input$speed, rho = input$rho)
          x[t+1] = xy[1]
          y[t+1] = xy[2]
          a[t+1] = xy[3]
        }
        result <- tibble::add_row(result, x=x, y=y, a=a, participant=participant)
      }
      result
    })



    output$twoD_walk <- renderPlot({
      figure <- walk()
      ggplot2::ggplot(figure, ggplot2::aes(x=x, y=y, colour=participant)) +
        ggplot2::geom_path() + ggplot2::coord_fixed()
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
