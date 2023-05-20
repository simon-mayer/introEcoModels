
# Thema 4 Exponentielles Wachstum



# Eboladatensatz ----------------------------------------------------------

# Daten visualisieren
#plot(totalSus ~ Day, ebola)  # Datenpunkte
#plot(totalSus ~ Day, ebola, type = "l") # Linie

# logarithmische y Skala
#plot(totalSus ~ Day, ebola, type = "l", log = "y")




# Modellparameter bestimmen -----------------------------------------------

# Kontinuierliches exponentielles Wachstum
#      Nt = N0 * e^(r*t)
# log(Nt) = log(N0 * e^(r*t))
#         = log(N0) + r*t
# entspricht linearer Regression
#       y = a + b*x

# Kniff: wenn die Populationsgröße logarithmiert ist, ist der Zusammenhang linear
# statistisch ist das hilfreich, da wir lineare Modelle sehr leicht fitten können
# (solche statistischen Pendants existieren aber für die meisten dynamischen Modelle nicht!)

#lm_ebola = lm(log(totalSus) ~ Day, data = ebola)
#coef(lm_ebola)

# in unserer Schreibweise
#exp(coef(lm_ebola)[1])   # geschätzter Startwert N0
#coef(lm_ebola)[2]        # Zuwachsrate r, täglich circa 2% mehr Fälle

### Nt = 93.5 * e^(0.02*t)

# logarithmische Skala
#plot(log(totalSus) ~ Day, ebola, type = "l")
#abline(lm_ebola, col = 2)


ebolaUI <- function(id){
  tabPanel("Exponentiell",
           fluidRow(
             column(10, plotOutput(NS(id, "faelle_pro_tag")))),
           fluidRow(
             column(10, plotOutput(NS(id, "faelle_log")))),
           fluidRow(
             column(10, plotOutput(NS(id, "faelle_model_log")))),
           fluidRow(
             column(10, verbatimTextOutput(NS(id, "exp_lm")))),
  )
}




ebolaServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$faelle_pro_tag <- renderPlot({
      ggplot2::ggplot(data = ebola, ggplot2::aes(x=Day, y=totalSus)) +
        ggplot2::geom_line() +
        ggplot2::xlab("Tage") + ggplot2::ylab("Fälle") +
        ggplot2::labs(title="Ebola-Fälle pro Tag")
    })
    output$faelle_log <- renderPlot({
        ggplot2::ggplot(data = ebola, ggplot2::aes(x=Day, y=totalSus)) +
        ggplot2::scale_y_continuous(trans='log10') +
        ggplot2::geom_line() +
        ggplot2::xlab("Tage") + ggplot2::ylab("Fälle (logarithmisch)") +
        ggplot2::labs(title="Ebola-Fälle pro Tag (logarithmisch)")
    })
    output$faelle_model_log <- renderPlot({
      ggplot2::ggplot(data = ebola, ggplot2::aes(x=Day, y=log(totalSus))) +
                        ggplot2::geom_line() +
                        ggplot2::geom_smooth(method="lm") +
                       ggplot2::xlab("Tage") + ggplot2::ylab("Expontent") +
        ggplot2::labs(title="Ebola-Fälle pro Tag (logarithmisch)")
    })
    output$exp_lm <- renderText({"lm_ebola = lm(log(totalSus) ~ Day, data = ebola)"})



  })
}

