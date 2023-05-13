#' @import shiny
myApp <- function(...){
  ui <- shiny::fluidPage(
    tags$script("function show_hide(identifier) {
                    var x = document.getElementById(identifier);
                    if (x.style.display === 'none') {
                      x.style.display = 'block';
                    } else {
                      x.style.display = 'none';
                    }
                  }"),
    shiny::navlistPanel(
      lachsUI("lachs"),
      stauseeUI("stausee"),
      logisticUI("logistic"))
)

  server <- function(input, output, session){
    logisticServer("logistic")
    stauseeServer("stausee")
    lachsServer("lachs")
  }

  shinyApp(ui, server)
}
