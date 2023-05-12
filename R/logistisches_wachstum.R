# Thema 5 Logistisches Wachstum



# Modell ------------------------------------------------------------------


#' Modellfunktion für diskretes logistisches Wachstum
#'
#' `Nt(r, K, N_k, t)` erstellt einen Vektor der Populationsgröße zum
#' Zeitpunkt i am Index i.
#'
#' @param r Zahl zwischen -0.1 und 4, gibt Wachstumsrate an
#'    (Verhältnis zusätzliche Individuen zum nächsten Zeitpunkt zu aktuellen)
#' @param K Zahl, gibt Carrying Capacity der Umwelt an ~ max Population
#' @param N_k Zahl zwischen 0 und 1, relative Anfangspopulation
#'    im Verhältnis zur Kapazität an
#' @param t Zahl Endzeitpunkt der Simulation
#'
#' @returns Vektor von Zahlen, hält am Index i Populationsgröße zur Zeit i
#' @export
#'
#'
Nt <- function(r, K, N_k, t) { # Parameter
  N <- N_k * K
  for (i in 1:(t - 1)) {     # Schleife von 1 bis t-1

    # Population zum nächsten Zeitpunkt ist die Population
    # zum aktuellen Zeitpunkt + Wachstum
    N[i + 1] <- N[i] + r * (1 - N[i]/K) * N[i]
  }
  N
}

logisticUI <- function(id){
  tabPanel("Logistic Model",
    tags$script("function show_hide(identifier) {
                    var x = document.getElementById(identifier);
                    if (x.style.display === 'none') {
                      x.style.display = 'block';
                    } else {
                      x.style.display = 'none';
                    }
                  }"),
    fluidRow(
       column(6,
          sliderInput(shiny::NS(id, "growth_rate"), "Wachstumsrate (r)",
            value = 0.2, min = -0.1, max = 3, step = 0.1 ),
            sliderInput(shiny::NS(id, "capacity"), "Kapazität (K)",
              value = 100, min = 10, max = 1000, step = 10 )),
            column(6,
              sliderInput(shiny::NS(id, "start"), "relative Anfangspopulation (N/K)",
              value = 0.1, min = 0, max = 2, step = 0.1),
            sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                             value = 100, min = 10, max = 1000, step = 10))),
    fluidRow(
      column(10, plotOutput(NS(id, "population")))),
    fluidRow(
      column(1, actionButton(NS(id, "code_button"), "Show / Hide Function Code",
                            onClick="show_hide('code_function')"))),
    fluidRow(
      column(12, div(label=NS(id, "code_box"), id="code_function",
                            style.display="none",
                            verbatimTextOutput(NS(id, "da_code")))))
    )
}




logisticServer <- function(id){
  moduleServer(id, function(input, output, session){
      model_text <- "Nt <- function(r, K, N_k, t) { # Parameter
          N <- N_k * K
          for (i in 1:(t - 1)) {     # Schleife von 1 bis t-1

              # Population zum nächsten Zeitpunkt ist die Population
              # zum aktuellen Zeitpunkt + Wachstum
              N[i + 1] <- N[i] + r * (1 - N[i]/K) * N[i]
          }
          N
      }"

      pop <- reactive(data.frame(Zeit=1:input$end, Population=Nt(input$growth_rate, input$capacity,
                         input$start, input$end)))
      output$population <- renderPlot({
        ggplot2::ggplot(data = pop(), ggplot2::aes(x=Zeit, y=Population)) + ggplot2::geom_line()
      })
      output$da_code <- renderText({model_text})
  })

}


