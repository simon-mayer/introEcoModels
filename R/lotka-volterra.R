# Thema 7 Lotka Volterra



# Modell ------------------------------------------------------------------

#' Modellfunktion für Lotka-Volterra-Modell
#'
#' LotVmod(Time, State, Pars) gibt die Veränderungen der Popoulationen
#' in einem Zeitschritt wider, je nach vorigen Populationen und Parametern
#'
#' @param Time Integer, Index der Zeit
#' @param State Liste mit Integern `State$B` = Beutepopulation und
#'              `State$R` = Räuberpopulation
#' @param Pars Liste mit Reals `Pars$r` = Wachstum Beute
#'                             `Pars$m` = Schwund Räuber
#'                             `Pars$a` = Jagderfolg
#'                             `Pars$b` = Umwandlung Räuber
#'
#' @returns Liste mit Feldern `dB` Veränderung der Beutepopulation und
#'                            `dR` Veränderung der Räuberpopulation
#' @export
LotVmod <- function (Time, State, Pars) {
  # with lässt R code auf ein Objekt laufen, vereinfacht Code
  # with ermöglicht einem Code-Fragment Zugriff auf Variablen,
  # die in einer Liste oder einem Dataframe gespeichert sind
  # (hier B, R in State, a, b, r, m in Pars)
  with(as.list(c(State, Pars)), {
    dB = r*B - a*B*R
    dR = -m*R + b*a*B*R
    str(dB)
    str(dR)
    # gibt Veränderungen der Populationen zurück
    return(list(c(dB, dR)))
  })
}


lotkaVolterraUI <- function(id){
  tabPanel("Lotka Volterra",
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "growth_rate_red"),
                                "Wachstumsrate Beute rot (r)",
                                value = 5, min = 0.0, max = 12, step = 0.1 ),
                    sliderInput(shiny::NS(id, "loss_rate_red"),
                                "Schwundrate Räuber rot (m)",
                                value = 10, min = 0.0, max = 20, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "success_rate_red"),
                                "Jagderfolg rot (a)",
                                value = 0.1, min = 0.0, max = 3, step = 0.1 ),
                    sliderInput(shiny::NS(id, "conversion_rate_red"),
                                "Umwandlungsrate Räuber rot (b)",
                                value = 1.3, min = 0.0, max = 3, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "start_prey_red"),
                                "Anfangspopulation Beute rot",
                                value = 40, min = 10, max = 1000, step = 10),
                    sliderInput(shiny::NS(id, "start_pred_red"),
                                "Anfangspopulation Raüber rot",
                                value = 90, min = 10, max = 1000, step = 10))),
           fluidRow(
             column(12, plotOutput(NS(id, "population")))),
           fluidRow(
             column(12, plotOutput(NS(id, "phase")))),
           fluidRow(
             column(4,
                sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                       value = 3, min = 1, max = 10, step = 0.05))),
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "growth_rate_blue"),
                                "Wachstumsrate Beute blau (r)",
                                value = 5, min = 0.0, max = 12, step = 0.1 ),
                    sliderInput(shiny::NS(id, "loss_rate_blue"),
                                "Schwundrate Räuber blue (m)",
                                value = 10, min = 0.0, max = 20, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "success_rate_blue"),
                                "Jagderfolg blau (a)",
                                value = 0.1, min = 0.0, max = 3, step = 0.1 ),
                    sliderInput(shiny::NS(id, "conversion_rate_blue"),
                                "Umwandlungsrate Räuber blau (b)",
                                value = 1.3, min = 0.0, max = 3, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "start_prey_blue"),
                                "Anfangspopulation Beute blue",
                                value = 40, min = 10, max = 1000, step = 10),
                    sliderInput(shiny::NS(id, "start_pred_blue"),
                                "Anfangspopulation Raüber blau",
                                value = 90, min = 10, max = 1000, step = 10))),


           fluidRow(
             column(1, actionButton(NS(id, "code_button"), "Show / Hide Function Code",
                                    onClick="show_hide('lotka_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "lotka_function"), id="lotka_function",
                            verbatimTextOutput(NS(id, "lotka_function_text")))))
  )
}


lotkaVolterraServer <-  function(id){
  moduleServer(id, function(input, output, session){
     pop_blue <- reactive({
        Pars <- c(a = input$success_rate_blue,
                  b = input$conversion_rate_blue,
                  r = input$growth_rate_blue,
                  m = input$loss_rate_blue)
        State <- c(B = input$start_prey_blue,
                   R = input$start_pred_blue)
        Time <- seq(0, input$end, by = 0.01)
        out <- as.data.frame(deSolve::ode(func = LotVmod, y = State, parms = Pars, times = Time))
        names(out) <- c("Zeit", "Beute", "Räuber")
        out$Modell <- "blau"
        out
    })
     pop_red <- reactive({
       Pars <- c(a = input$success_rate_red,
                 b = input$conversion_rate_red,
                 r = input$growth_rate_red,
                 m = input$loss_rate_red)
       State <- c(B = input$start_prey_red,
                  R = input$start_pred_red)
       Time <- seq(0, input$end, by = 0.01)
       out <- as.data.frame(deSolve::ode(func = LotVmod, y = State, parms = Pars, times = Time))
       names(out) <- c("Zeit", "Beute", "Räuber")
       out$Modell <- "rot"
       out
     })
    output$population <- renderPlot({
      out <- pop_blue()
      out <- dplyr::bind_rows(out, pop_red())
      out <- tidyr::pivot_longer(data=out,
                                 cols = c("Beute", "Räuber"),
                                 names_to = "Rolle",
                                 values_to = "Population")
      ggplot2::ggplot(out, ggplot2::aes(x = Zeit, y = Population,
                                        lty = Rolle, colour=Modell)) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = c("blue", "red"))

    })
    output$phase <- renderPlot({
      out <- pop_blue()
      out <- dplyr::bind_rows(out, pop_red())
      vert_blue <- input$growth_rate_blue/input$success_rate_blue
      vert_red <- input$growth_rate_red/input$success_rate_red
      hor_blue <- input$loss_rate_blue / (input$success_rate_blue *
                                         input$conversion_rate_blue)
      hor_red <- input$loss_rate_red / (input$success_rate_red *
                                        input$conversion_rate_red)

      ggplot2::ggplot(out, ggplot2::aes(x = Beute, y = Räuber,
                                          colour=Modell)) +
        ggplot2::geom_path() +
        ggplot2::geom_hline(yintercept = hor_blue, colour = "blue", lty = 2) +
        ggplot2::geom_hline(yintercept = hor_red, colour = "red", lty = 2) +
        ggplot2::geom_vline(xintercept = vert_blue, colour = "blue", lty = 2) +
        ggplot2::geom_vline(xintercept = vert_red, colour = "red", lty = 2) +
        ggplot2::scale_colour_manual(values = c("blue", "red"))

    })
    lotka_function_text<-
      "
LotVmod <- function (Time, State, Pars) {
  # with ermöglicht einem Code-Fragment Zugriff auf Variablen,
  # die in einer Liste oder einem Dataframe gespeichert sind
  # (hier B, R in State, a, b, r, m in Pars)
  with(as.list(c(State, Pars)), {
    dB = r*B - a*B*R
    dR = -m*R + b*a*B*R
    # gibt Veränderungen der Populationen zurück
    return(list(c(dB, dR)))
  })
}
"
    output$lotka_function_text <- renderText(lotka_function_text);

  })
}

