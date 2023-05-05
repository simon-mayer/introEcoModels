#' @import shiny
myApp <- function(...){
  ui <- shiny::fluidPage(
      shiny::tabsetPanel(
      logisticUI("logistic")
      )
)

  server <- function(input, output, session){
    logisticServer("logistic")
  }

  shinyApp(ui, server)
}
