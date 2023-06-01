stadienModelleUI <- function(id){
  tabPanel("Stadienmodelle",
           fluidRow(
             column(2,
                    numericInput(NS(id, "mat00"), "", min=0.0, max=20,
                                 step=0.01, value=0.95)),
             column(2,
                    numericInput(NS(id, "mat01"), "", min=0.0, max=20,
                                 step=0.01, value=0.21)),
             column(2,
                    numericInput(NS(id, "mat02"), "", min=0.0, max=20,
                                 step=0.01, value=0.01)),
             column(1),
             column(2,
                    numericInput(NS(id, "x0"), "", min=0.0, max=20,
                                 step=0.01, value=1.0)),
             column(1),
           column(2,
                  verbatimTextOutput(NS(id, "v0")),
                  )),
           fluidRow(
             column(2,
                    numericInput(NS(id, "mat10"), "", min=0.0, max=20,
                                 step=0.01, value=0.33)),
             column(2,
                    numericInput(NS(id, "mat11"), "", min=0.0, max=20,
                                 step=0.01, value=0.0)),
             column(2,
                    numericInput(NS(id, "mat12"), "", min=0.0, max=20,
                                 step=0.01, value=0.0)),
            column(1),
            column(2,
                  numericInput(NS(id, "x1"), "", min=0.0, max=20,
                                step=0.01, value=10)),
            column(1),
            column(2, verbatimTextOutput(NS(id, "v1")),
                    )),
           fluidRow(
              column(2,
                    numericInput(NS(id, "mat20"), "", min=0.0, max=20,
                                 step=0.01, value=0.0)),
              column(2,
                    numericInput(NS(id, "mat21"), "", min=0.0, max=20,
                                 step=0.01, value=10.0)),
              column(2,
                    numericInput(NS(id, "mat22"), "", min=0.0, max=20,
                                 step=0.01, value=0.25)),
              column(1),
              column(2,
                           numericInput(NS(id, "x2"), "", min=0.0, max=20,
                                        step=0.01, value=50)),
                    column(1),
                    column(2, verbatimTextOutput(NS(id, "v2")),
                    )),
           fluidRow(
             column(4,
                    sliderInput(NS(id, "n_years"), "Endzeitpunkt(n_years)",
                      min = 1, max=100, value=50, step=1))),
           fluidRow((
             column(12, plotOutput(NS(id, "plot")))
           ))

           )

}




stadienModelleServer <- function(id){
  moduleServer(id, function(input, output, session){
    stages <-  c("rosette", "shoot", "seed")

    M <- reactive({

        mat <- matrix(data = c(input$mat00,      input$mat01,   input$mat02,
                       input$mat10,      input$mat11,   input$mat12,
                       input$mat20,      input$mat21,   input$mat22),
                       nrow = 3, ncol = 3, byrow=TRUE)
        colnames(mat) <- rownames(mat) <- stages
        mat})

    population_init <- reactive({c(rosette = input$x0, shoot = input$x1, seed = input$x2)})

    one_time_vector <- reactive({M() %*% population_init()})
    output$v0 <- renderText({one_time_vector()[1]})
    output$v1 <- renderText({one_time_vector()[2]})
    output$v2 <- renderText({one_time_vector()[3]})

    output$plot <- renderPlot({
        Timeseries <- matrix(NA, nrow = input$n_years, ncol = 3) # leere Matrix erstellen
        Timeseries[1,] <- population_init() # erste Zeile mit Anfangsbedingungen auffüllen
        times <- 1:(input$n_years-1) # Vektor von 1 bis n_years - 1 Zahlen erstellen

        for (t in times) {
          population_before <- Timeseries[t,] # die Zeile t extrahieren
          Timeseries[t+1,] <- M() %*% population_before
        }
        dat <- as.data.frame(Timeseries)
        names(dat) <- stages
        head(dat)
        dat$Zeit <- 1:nrow(dat)

        dat <- tidyr::pivot_longer(dat, cols=c("rosette", "shoot", "seed"),
                                   values_to = "Population", names_to = "Stadium")


        ggplot2::ggplot(dat) +
          ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Population, colour=Stadium))
    })






  })
}
