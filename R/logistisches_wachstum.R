# Thema 5 Logistisches Wachstum



# Modell ------------------------------------------------------------------


#' Modellfunktion für diskretes logistisches Wachstum
#'
#' @param r Zahl zwischen -0.1 und 4, gibt Wachstumsrate an
#' @param K Zahl, gibt Carrying Capacity der Umwelt an ~ max Population
#' @param N Array von Zahlen, hält am Index 1 die Populationsgröße zur Zeit 1
#' @param t Zahl Endzeitpunkt der Simulation
#'
#' @returns Vektor von Zahlen, hält am Index t Populationsgröße zur Zeit t
#' @export
Nt <- function(r, K, N, t) { # Parameter

  for (i in 1:(t - 1)) {     # Schleife von 1 bis t-1

    # Population zum nächsten Zeitpunkt ist die Population zum aktuellen Zeitpunkt + Wachstum
    N[i + 1] <- N[i] + r * (1 - N[i]/K) * N[i]
  }
  N
}

logisticUI <- function(id){
  tabPanel("Logistic Model",
           fluidRow(
             column(6,
              sliderInput(shiny::NS(id, "growth_rate"), "Wachstumsrate",
                          value = 0.2, min = -0.1, max = 3.5, step = 0.1 ),
              sliderInput(shiny::NS(id, "capacity"), "Kapazität",
                          value = 100, min = 10, max = 1000, step = 10 )),
              column(6,
                      sliderInput(shiny::NS(id, "start"), "Anfangspopulation (Ratio zu Kap)",
                          value = 0.1, min = 0, max = 2, step = 0.1),
              sliderInput(shiny::NS(id, "end"), "Endzeitpunkt",
                             value = 100, min = 10, max = 1000, step = 10))),
           fluidRow(
             column(12, plotOutput(NS(id, "population")))
           )

  )

}



logisticServer <- function(id){
  moduleServer(id, function(input, output, session){
      pop <- reactive(data.frame(Zeit=1:input$end, Population=Nt(input$growth_rate, input$capacity,
                         input$start, input$end)))
      output$population <- renderPlot({
        ggplot2::ggplot(data = pop(), ggplot2::aes(x=Zeit, y=Population)) + ggplot2::geom_line()
      })


  })

}


