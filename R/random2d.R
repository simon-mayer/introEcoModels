

random2dUI <- function(id){
  tabPanel("Random Walk 1D",
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "prob"), "Wahrscheinlichkeit (p)",
                                value = 0.5, min = 0.0, max = 1, step = 0.05 )),
             column(4,
                    sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                value = 100, min = 10, max = 1000, step = 10)),
             column(4,
                    sliderInput(shiny::NS(id, "seed"), "Zufallsexperiment Nr",
                                value = 100, min = 1, max = 200, step = 1))
           ),
           fluidRow(
             column(12, plotOutput(NS(id, "bernoulli")))),
           fluidRow(
             column(1, actionButton(NS(id, "bernoulli_button"), "Show / Hide Function Code",                                    onClick="show_hide('bernoulli_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "bernoulli_box"), id="bernoulli_function",
                            verbatimTextOutput(NS(id, "bernoulli_function_text")))))
  )
}




random2dServer <- function(id){
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
  })

}
