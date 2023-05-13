# Lachsmodell -------------------------------------------------------------



#' Modellfunktion zur Vorhersage einer Lachspopulation
#' @param S4 Population 4 Jahre vorher
#' @param S5 Population 5 Jahre vorher
#' @param E Laicherfolg als Zahl zwischen 0 und 1
#'
#' @export
model = function(S4, S5, E = 0.57) {
  St = E*0.28*S4 + E*0.72*S5
  return(St)
}


lachsUI <- function(id){
  tabPanel("diskret dynamisch deterministisch - Lachse",
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "laich"), "Laicherfolg (E)",
                                value = 0.57, min = 0.3, max = 1,1, step = 0.01 ),
                    sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                                value = 30, min = 10, max = 100, step = 1 ))),
             fluidRow(5,
               column(8, plotOutput(NS(id, "population")))),
             fluidRow(
               column(1, actionButton(NS(id, "function_button"), "Show / Hide Function Code",
                                      onClick="show_hide('lachs_function')"))),
             fluidRow(
               column(12, div(label=NS(id, "lachs_fun_box"), id="lachs_function",
                              verbatimTextOutput(NS(id, "da_function_code"))))),
             fluidRow(
               column(1, actionButton(NS(id, "model_button"), "Show / Hide Model Code",
                                      onClick="show_hide('lachs_model')"))),
             fluidRow(
               column(12, div(label=NS(id, "lachs_model_box"), id="lachs_model",
                              verbatimTextOutput(NS(id, "da_model_code")))))
    )
}





lachsServer <- function(id){
  moduleServer(id, function(input, output, session){
    function_text <- "
  # Das Modell als Funktion
  model = function(S4, S5, E = 0.57) {
    St = E*0.28*S4 + E*0.72*S5
    return(St)
  }
  "
    model_text = "
  # Startbedingungen als Zufallswerte
  Jahre = 2000:2009
  set.seed(123)
  S = rpois(10, 100)


  # das Modell nutzen um zukünftige Lachspopulationen vorherzusagen
  for (i in 1:30) {
    index = 10+i                         # 10 Anfangsjahre
    S[index] = model(S4 = S[index-4],
                     S5 = S[index-5],
                     E = 0.57)    # Modell anwenden, man kann auch E = 1 setzen
    Jahre[index] = Jahre[index-1] + 1
  }
  plot(Jahre, S)
  "

    pop <- reactive({
      # Startbedingungen
      Jahre = 2000:2009
      set.seed(123)
      S = rpois(10, 100)

      for (i in 1:(input$end-10)) {
        index = 10+i
        S[index] = model(S4 = S[index-4],
                         S5 = S[index-5],
                         E = input$laich)
        Jahre[index] = Jahre[index-1] + 1
      }
      data.frame(Zeit=1:input$end, Population=S)
    })
    output$population <- renderPlot({
      ggplot2::ggplot(data = pop(), ggplot2::aes(x=Zeit, y=Population)) +
        ggplot2::geom_line()
    })
    output$da_function_code <- renderText({function_text})
    output$da_model_code <- renderText({model_text})
  })

}

