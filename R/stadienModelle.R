stadienModelleUI <- function(id){
  tabPanel("Stadienmodelle",
           fluidRow(
             column(6, h3("Lebenstafel"))
           ),
           fluidRow(
             column(6, "1 = Rossete, 2 = Blütenstand, 3 = Samen")
           ),
           fluidRow(
             column(2,
                    numericInput(NS(id, "mat11"), "1ₜ→1ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.95)),
             column(2,
                    numericInput(NS(id, "mat12"), "2ₜ→1ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.21)),
             column(2,
                    numericInput(NS(id, "mat13"), "3ₜ→1ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.01)),
             column(1),
             column(2,
                    numericInput(NS(id, "x1"), "1₁", min=0.0, max=20,
                                 step=0.01, value=1.0)),
             column(1),
           column(2,
                  verbatimTextOutput(NS(id, "v1")),
                  )),
           fluidRow(
             column(2,
                    numericInput(NS(id, "mat21"), "1ₜ→2ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.33)),
             column(2,
                    numericInput(NS(id, "mat22"), "2ₜ→2ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.0)),
             column(2,
                    numericInput(NS(id, "mat23"), "3ₜ→2ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.0)),
            column(1),
            column(2,
                  numericInput(NS(id, "x2"), "2₁", min=0.0, max=20,
                                step=0.01, value=10)),
            column(1),
            column(2, verbatimTextOutput(NS(id, "v2")),
                    )),
           fluidRow(
              column(2,
                    numericInput(NS(id, "mat31"), "1ₜ→3ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.0)),
              column(2,
                    numericInput(NS(id, "mat32"), "2ₜ→3ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=10.0)),
              column(2,
                    numericInput(NS(id, "mat33"), "3ₜ→3ₜ₊₁", min=0.0, max=20,
                                 step=0.01, value=0.25)),
              column(1),
              column(2,
                           numericInput(NS(id, "x3"), "3₁", min=0.0, max=20,
                                        step=0.01, value=50)),
                    column(1),
                    column(2, verbatimTextOutput(NS(id, "v3")),
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

        mat <- matrix(data = c(input$mat11,      input$mat12,   input$mat13,
                       input$mat21,      input$mat22,   input$mat23,
                       input$mat31,      input$mat32,   input$mat33),
                       nrow = 3, ncol = 3, byrow=TRUE)
        colnames(mat) <- rownames(mat) <- stages
        mat})

    population_init <- reactive({c(rosette = input$x1, shoot = input$x2, seed = input$x3)})

    one_time_vector <- reactive({M() %*% population_init()})
    output$v1 <- renderText({paste0("1₂=", one_time_vector()[1])})
    output$v2 <- renderText({paste0("2₂=", one_time_vector()[2])})
    output$v3 <- renderText({paste0("3₂=", one_time_vector()[3])})

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
