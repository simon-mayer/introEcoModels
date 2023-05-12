#' @import shiny
myApp <- function(...){
  ui <- shiny::fluidPage(
    shiny::tabsetPanel(
      lachsUI("lachs")),
      shiny::tabsetPanel(
      logisticUI("logistic")),
)

  server <- function(input, output, session){
    logisticServer("logistic")
    lachsServer("lachs")
  }

  shinyApp(ui, server)
}
