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
      ebolaUI("ebola_diagrams"),
      logisticUI("logistic"),
      lotkaVolterraUI("lotkaVolterra"),
      diffusion1dUI("diffusion1d"),
      diffusion2dUI("diffusion2d"),
      random1dUI("random1d"))
)

  server <- function(input, output, session){
    logisticServer("logistic")
    stauseeServer("stausee")
    ebolaServer("ebola_diagrams")
    lachsServer("lachs")
    lotkaVolterraServer("lotkaVolterra")
    diffusion1dServer("diffusion1d")
    diffusion2dServer("diffusion2d")
    random1dServer("random1d")
  }

  shinyApp(ui, server)
}
