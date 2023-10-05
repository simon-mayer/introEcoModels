

# Modellfunktion

#' Simuliere einen Münzwurf
#' @param p reelle Zahl, Wahrscheinlichkeit für Bild
#' @returns  Integer  +1 (zu p * 100 %) oder −1 (zu (1−p) \* 100 %
random1d = function(p) {
  # wenn Bild, dann nach oben (+1), ansonsten nach unten (-1)
  ifelse(test = purrr::rbernoulli(n = 1, p = p), yes = 1, no = -1)
}

#'Simuliere einen kontinuierlichen 1d - Schritt
#'
#'@param mean reelle Zahl, Erwartungswert
#'@param sd   reelle Zahl, Standardabweichung
#'
#'@returns reelle Zahl, zufälligen Wert aus der Normalverteilung
random1d_c = function(mean, sd) {

# Zufälliger Wert aus der Normalverteilung
rnorm(n = 1, mean = mean, sd = sd)
}


random1dUI <- function(id){
  tabPanel("Random Walk 1D",
           fluidRow(column(12, h2("Random Walk aus Bernoulli-Verteilung"))),
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "prob"), "Wahrscheinlichkeit (p)",
                                value = 0.5, min = 0.0, max = 1, step = 0.05 )),
             column(4,
                    sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                value = 100, min = 10, max = 300, step = 10)),
             column(4,
                    numericInput(shiny::NS(id, "seed"), "Zufallsexperiment Nr",
                                value = 1, min=1, max=.Machine$integer.max))
             ),
           fluidRow(
             column(12, plotOutput(NS(id, "bernoulli")))),
           fluidRow(
             column(1, actionButton(NS(id, "bernoulli_button"), "Show / Hide Function Code",                                    onClick="show_hide('bernoulli_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "bernoulli_box"), id="bernoulli_function",
                            verbatimTextOutput(NS(id, "bernoulli_function_text"))))),
           fluidRow(
             column(12, h2("Random Walk aus Normalverteilung"))),
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "mean"), "Erwartungswert (mean)",
                                value = 0, min = -10, max = 10, step = 0.5 )),
             column(4,
                    sliderInput(shiny::NS(id, "end_walk"), "Endzeitpunkt (t)",
                                value = 100, min = 5, max = 300, step = 5)),
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "sd"), "Standardabweichung",
                                value = 2, min = 0, max = 10, step = 0.5)),
             column(4, sliderInput(shiny::NS(id, "participants"), "Anzahl Simulationen",
                                   value = 1, min = 1, max = 10, step = 1))

            ),
           fluidRow(
             column(12, plotOutput(NS(id, "oneD_walk")))),
           fluidRow(
             column(1, actionButton(NS(id, "oneD_walk_button"), "Show / Hide Function Code",
                                    onClick="show_hide('oneD_walk_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "oneD_walk_box"), id="oneD_walk_function",
                            verbatimTextOutput(NS(id, "oneD_walk_function_text"))))))
           )
}




random1dServer <- function(id){
  moduleServer(id, function(input, output, session){

    output$bernoulli <- renderPlot({
      set.seed(input$seed)
      n = input$end
      x = rep(NA, n+1)
      x[1] = 0

      # Modell anwenden
      for (t in 1:n) {
        x[t+1] = x[t] + random1d(input$prob)
      }
      df <- data.frame(y = x, Zeit=0:n)

      ggplot2::ggplot(data = df, ggplot2::aes(x=Zeit, y=y)) +
        ggplot2::geom_line()
    })

    bernoulli_function_text <-"
random1d = function(p) {
  # wenn Bild, dann nach oben (+1), ansonsten nach unten (-1)
  ifelse(test = rbernoulli(n = 1, p = p), yes = 1, no = -1)
}
    "
    output$bernoulli_function_text <- renderText({bernoulli_function_text})

    output$oneD_walk <- renderPlot({

      set.seed(121212)

      df <- data.frame(y=numeric(0), Zeit=integer(0), participant=character(0))
      n = input$end_walk

      some_letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
                       "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
                       "x", "y", "z")

      for(participant in some_letters[1:input$participants]){
        x = rep(NA, n+1)
        x[1] = 0
        # Modell anwenden
        for (t in 1:n) {
          x[t+1] = x[t] + random1d_c(mean=input$mean, sd=input$sd)
        }
        df <- tibble::add_row(.data = df, y=x,
                        Zeit=0:n,
                        participant=rep(participant, n+1))
      }
      ggplot2::ggplot(data = df, ggplot2::aes(x=Zeit, y=y, colour=participant)) +
        ggplot2::geom_line()
    })



    oneD_walk_function_text <-"
random1d_c = function(mean, sd) {

    # Zufälliger Wert aus der Normalverteilung
      rnorm(n = 1, mean = mean, sd = sd)
}
"
output$oneD_walk_function_text <- renderText({oneD_walk_function_text})


  })

}

