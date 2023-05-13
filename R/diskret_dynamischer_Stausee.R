# Thema 3 Hydrologisches Modell



# Modellfunktion ---------------------------------------------------------
#' Berechne Abfluss eines Stausees in Abhängigkeit vom Eintrag
#'
#' @param s0 Menge im Speicher zu Beginn
#' @param input kontinuierlicher Eintrag pro periode
#' @param k Verhältnis Abfluss/Gesamtmenge
#' @param schritte Dauer der Simulation
#'
#' @export
modell <- function(s0, input, k, schritte){ # Name der Funktion und ihre Argumente

  # Vektoren vorbereiten
  speicher <- rep(NA, schritte)
  abfluss <- rep(NA, schritte)

  # Abfluss und Speicher zum Zeitpunkt 1 (hängen von s0 ab)
  abfluss[1] <- k * (s0 + input[1])
  speicher[1] <- s0 + input[1] - abfluss[1]

  # Abfluss und Speicher ab Zeitpunkt 2 (hängen von vorherigen Zeitpunkten ab)
  for (i in 2:schritte) {
    abfluss[i] <- k * (speicher[i-1] + input[i])
    speicher[i] <- speicher[i-1] + input[i] - abfluss[i]
  }

  # Ergebnisse zu einem data.frame
  return(data.frame(Speicher = speicher, Abfluss = abfluss))

}

stauseeUI <- function(id){
  tabPanel("diskret dynamisch deterministisch – Stausee",
           fluidRow(
             column(6,
                    sliderInput(shiny::NS(id, "eintrag"), "Eintrag (input)",
                                value = 15, min = 0, max = 100, step = 1 ),
                    sliderInput(shiny::NS(id, "start"), "Startwert (s0)",
                                value = 10, min = 0, max = 1000, step = 5 )),
           column(6,
                  sliderInput(shiny::NS(id, "parameter"), "Abfluss/Speicher (k)",
                              value = 0.05, min = 0.0, max = 1, step = 0.01 ),
                  sliderInput(shiny::NS(id, "schritte"), "Endzeitpunkt (schrittte)",
                              value = 100, min = 10, max = 1000, step = 10 ))),
           fluidRow(column(8, plotOutput(NS(id, "speicher")))))

}



stauseeServer <- function(id){
  moduleServer(id, function(input, output, session){
    observe({
      print(input$eintrag)
      print(input$parameter)
      print(1:input$schritte)
      print(input$start)
      })
    pop <- reactive({
      sim <- modell(input = rep(input$eintrag, input$schritte), s0 = input$start,
             k = input$parameter, schritte = input$schritte)
      sim$Zeit <- 1:input$schritte
      sim
    })
    observe({print(pop())})
    output$speicher <- renderPlot({
      ggplot2::ggplot(data = pop()) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Speicher), color = "blue") +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Abfluss), color = "red")
    })
  })
}
