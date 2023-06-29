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
    # gibt Veränderungen der Populationen zurück
    return(list(c(dB, dR)))
  })
}


lotkaVolterraUI <- function(id){
  tabPanel("Lotka Volterra für Räuber/Beute",
           fluidRow(
             column(4,
                    sliderInput(shiny::NS(id, "growth_rate"),
                                "Wachstumsrate Beute (r)",
                                value = 5, min = 0.0, max = 12, step = 0.1 ),
                    sliderInput(shiny::NS(id, "loss_rate"),
                                "Schwundrate Räuber (m)",
                                value = 10, min = 0.0, max = 20, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "success_rate"),
                                "Jagderfolg (a)",
                                value = 0.1, min = 0.0, max = 3, step = 0.1 ),
                    sliderInput(shiny::NS(id, "conversion_rate"),
                                "Umwandlungsrate Räuber (b)",
                                value = 1.3, min = 0.0, max = 3, step = 0.1 )),
             column(4,
                    sliderInput(shiny::NS(id, "start_prey"),
                                "Anfangspopulation Beute",
                                value = 40, min = 10, max = 200, step = 5),
                    sliderInput(shiny::NS(id, "start_pred"),
                                "Anfangspopulation Räuber",
                                value = 90, min = 10, max = 200, step = 5))),
           fluidRow(
             column(12, plotOutput(NS(id, "population")))),
           fluidRow(
             column(12, plotOutput(NS(id, "phase")))),
           fluidRow(
             column(4,
                sliderInput(shiny::NS(id, "end"), "Endzeitpunkt (t)",
                       value = 3, min = 1, max = 10, step = 0.05))),
           fluidRow(
             column(1, actionButton(NS(id, "code_button"), "Show / Hide Function Code",
                                    onClick="show_hide('lotka_function')"))),
           fluidRow(
             column(12, div(label=NS(id, "lotka_function"), id="lotka_function",
                            verbatimTextOutput(NS(id, "lotka_function_text")))))
  )
}


lotkaVolterraServer <-  function(id){
  get_fixed_axis_limits <- function(vals){
    max_pop <- max(vals)
    if (max_pop < 200){
      200
    } else if(max_pop < 500){
      500
    } else if (max_pop < 1000){
      1000
    } else {
      max_pop
    }
  }


  moduleServer(id, function(input, output, session){
     pop <- reactive({
       Pars <- c(a = input$success_rate,
                 b = input$conversion_rate,
                 r = input$growth_rate,
                 m = input$loss_rate)
       State <- c(B = input$start_prey,
                  R = input$start_pred)
       Time <- seq(0, input$end, by = 0.01)
       out <- as.data.frame(deSolve::ode(func = LotVmod, y = State, parms = Pars, times = Time))
       names(out) <- c("Zeit", "Beute", "Räuber")
       out
     })
    output$population <- renderPlot({
      out <- pop()
      axis_beute <- get_fixed_axis_limits(out$Beute)
      axis_raeuber <- get_fixed_axis_limits(out$Räuber)
      out <- tidyr::pivot_longer(data=out,
                                 cols = c("Beute", "Räuber"),
                                 names_to = "Rolle",
                                 values_to = "Population")
      ggplot2::ggplot(out, ggplot2::aes(x = Zeit, y = Population,
                                        lty = Rolle)) +
      ggplot2::geom_line(colour="red") +
      ggplot2::ylim(0, max(axis_beute, axis_raeuber))


    })
    output$phase <- renderPlot({
      out <- pop()
      vert <- input$growth_rate/input$success_rate
      hor <- input$loss_rate / (input$success_rate *
                                        input$conversion_rate)

      ggplot2::ggplot(out, ggplot2::aes(x = Beute, y = Räuber)) +
        ggplot2::geom_path(colour="red") +
        ggplot2::geom_hline(yintercept = hor, colour = "red", lty = 2) +
        ggplot2::geom_vline(xintercept = vert, colour = "red", lty = 2) +
        ggplot2::xlim(0, get_fixed_axis_limits(out$Beute)) +
        ggplot2::ylim(0, get_fixed_axis_limits(out$Räuber))

    })


    lotka_function_text<-

      "
# ermittelt anhand Zustand und Parametern Zuwachs von Räuber(dR) und Beute(dB)
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


# später wird die LotVMod als Funktion die eine Ableitung liefert interpretiert
# und ein Solver für Differentialgleichung auf sie losgelassen

out <- deSolve::ode(func = LotVmod, y = State, parms = Pars, times = Time)

"
    output$lotka_function_text <- renderText(lotka_function_text);

  })
}

