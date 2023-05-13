# Thema 3 Hydrologisches Modell



# Modellfunktion ---------------------------------------------------------
#' Berechne Abfluss eines Stausees in Abhängigkeit vom Eintrag
#'
#' @param s0 Menge im Speicher zu Beginn
#' @param input kontinuierlicher Eintrag pro periode
#' @param k Verhältnis Abfluss/Gesamtmenge
#' @param schritte Dauer der Simulation
#'
#' @export
modell <- function(s0, input, k, schritte){ # Name der Funktion und ihre Argumente

  # Vektoren vorbereiten
  speicher <- rep(NA, schritte)
  abfluss <- rep(NA, schritte)

  # Abfluss und Speicher zum Zeitpunkt 1 (hängen von s0 ab)
  abfluss[1] <- k * (s0 + input[1])
  speicher[1] <- s0 + input[1] - abfluss[1]

  # Abfluss und Speicher ab Zeitpunkt 2 (hängen von vorherigen Zeitpunkten ab)
  for (i in 2:schritte) {
    abfluss[i] <- k * (speicher[i-1] + input[i])
    speicher[i] <- speicher[i-1] + input[i] - abfluss[i]
  }

  # Ergebnisse zu einem data.frame
  return(data.frame(Speicher = speicher, Abfluss = abfluss))

}





stauseeUI <- function(id){
  tabPanel("diskret dynamisch deterministisch – Stausee",
    fluidRow(
      column(6,
        sliderInput(shiny::NS(id, "eintrag"), "Eintrag (input)",
          value = 15, min = 0, max = 100, step = 1 ),
        sliderInput(shiny::NS(id, "start"), "Startwert (s0)",
          value = 10, min = 0, max = 1000, step = 5 )),
      column(6,
        sliderInput(shiny::NS(id, "parameter"), "Abfluss/Speicher (k)",
          value = 0.05, min = 0.0, max = 1, step = 0.01 ),
        sliderInput(shiny::NS(id, "schritte"), "Endzeitpunkt (schrittte)",
          value = 100, min = 10, max = 1000, step = 10 ))),
    fluidRow(column(12, plotOutput(NS(id, "speicher")))),
    fluidRow(column(12, plotOutput(NS(id, "speicher_var")))),
    fluidRow(column(12, plotOutput(NS(id, "daten_vs_rand_par")))),
    fluidRow(column(12, plotOutput(NS(id, "daten_vs_cal_par")))),
    fluidRow(column(12, plotOutput(NS(id, "validierung")))),
    fluidRow(column(12, plotOutput(NS(id, "szenario"))))
    )
}



stauseeServer <- function(id){
  # vorbereitungen variable daten
  # Zeitraum wählen
  tage = hydrodat$Date
  start <- which(tage=="2002-01-01")
  ende <- which(tage=="2004-12-31")

  # gemessene Niederschlagsdaten
  niederschlag <- hydrodat$P[start:ende]

  # Modell anwenden, s0 und k sind erstmal wie oben
  sim_var <- modell(input = niederschlag, s0=10, k=0.2, schritte=length(niederschlag))
  sim_var$Zeit <- tage[start:ende]

  # kalibrierung
  Abfluss = data.frame(Daten = hydrodat$Q[start:ende],
                       Modell = sim_var$Abfluss,
                       Zeit = tage[start:ende])

  # Zielfunktion = mittlerer absoluter Fehler maf
  maf <- function(par, data_nied, data_ab){
    sim <- modell(input=data_nied,               # Modell anwenden
                  s0=par[1], k=par[2],
                  schritte=length(data_nied))
    fehler <- mean(abs(sim$Abfluss - data_ab))   # mittlerer absoluter Fehler
    return(fehler)
  }

  # Numerische Optimierung mit der Funktion optim()
  optim_speicher <- optim(par=c(s0=10, k=0.2), # Startwerte der Parameter
                          fn=maf,              # Zielfunktion
                          data_nied=hydrodat$P[start:ende],
                          data_ab=hydrodat$Q[start:ende])

  par = optim_speicher$par
  sim_cal = modell(input = niederschlag, s0=par[1], k=par[2], schritte=length(niederschlag))

  Abfluss$Modell_kal <- sim_cal$Abfluss


  start <- which(tage=="2005-01-01")
  ende <- which(tage=="2007-12-31")

  # gemessene Niederschlagsdaten
  niederschlag <- hydrodat$P[start:ende]

  # Modell anwenden
  # s0 als letzter Wert von Kalibrierungszeitraum
  s0 = sim_cal$Speicher[nrow(sim_var)]
  # k kalibriert
  sim_val <- modell(input = niederschlag, s0 = s0, k=par[2], schritte=length(niederschlag))

  # Abflussvergleich Daten und Modell
  Abfluss_val = data.frame(Daten = hydrodat$Q[start:ende],
                       Modell = sim_val$Abfluss,
                       Zeit = tage[start:ende])
  # Szenario ----------------------------------------------------------------


  # 25 % Reduktion im Niederschlag
  niederschlag_less <- dat$P[start:ende]*0.75


  # langsameres Ablaufen durch Veränderung am See
  k = 0.03   # statt 0.07


  # Modell anwenden
  # s0 als letzter Wert von Kalibrierungszeitraum
  sim_szenario <- modell(input = niederschlag_less, s0=s0, k=k, schritte=length(niederschlag))
  Abfluss_val$szenario <- sim_szenario$Abfluss




  moduleServer(id, function(input, output, session){
    pop <- reactive({
      sim <- modell(input = rep(input$eintrag, input$schritte), s0 = input$start,
             k = input$parameter, schritte = input$schritte)
      sim$Zeit <- 1:input$schritte
      sim
    })
    output$speicher <- renderPlot({
      ggplot2::ggplot(data = pop()) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Speicher), color = "blue") +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Abfluss), color = "red")
    })
    output$speicher_var <- renderPlot({
      ggplot2::ggplot(data = sim_var) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Speicher), color = "blue") +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Abfluss), color = "red")
    })
    output$daten_vs_rand_par <- renderPlot({
      ggplot2::ggplot(data = Abfluss) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Daten, color = "daten")) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Modell, color = "model")) +
        ggplot2::scale_colour_manual(values = c("blue", "red"),
                                     labels = c("Daten", "Modell")) +
        ggplot2::labs(colour="", title = "Modell mit k = 0.2")
    })
    output$daten_vs_cal_par <- renderPlot({
      ggplot2::ggplot(data = Abfluss) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Daten, color = "daten")) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Modell_kal, color = "model")) +
        ggplot2::scale_colour_manual(values = c("blue", "red"),
                                     labels = c("Daten", "Modell")) +
        ggplot2::labs(colour="",
                      title=paste0("kalibriertes Modell mit k = ", optim_speicher$par[2]))
    })
    output$validierung <- renderPlot({
      ggplot2::ggplot(data = Abfluss_val) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Daten, color = "daten")) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Modell, color = "model")) +
        ggplot2::scale_colour_manual(values = c("blue", "red"),
                                     labels = c("Daten", "Modell")) +
        ggplot2::labs(colour="",
                      title="kalibriertes Modell auf anderem Zeitraum ")
    })
    output$szenario <- renderPlot({
      ggplot2::ggplot(data = Abfluss_val) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Daten, color = "daten")) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=Modell, color = "modell")) +
        ggplot2::geom_line(ggplot2::aes(x=Zeit, y=szenario, color="scenario")) +
        ggplot2::scale_colour_manual(values = c("blue", "red", "green"),
                                     labels = c("Daten", "Modell", "Szenario")) +
        ggplot2::labs(colour="",
                      title="kalibriertes Modell auf anderem Zeitraum ")
    })


  })
}
