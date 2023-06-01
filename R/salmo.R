salmoUI <- function(id){
  tabPanel("Salmo-Light",
    fluidRow(h2("Parameter")),
    fluidRow(
      column(4,
        numericInput(NS(id, "zin"), "zooplankton import (zin) C [g/(m³d)]",
                     value = 0.0005, min = 0.0, max = 0.002, step = 0.00001)),
      column(4,
        numericInput(NS(id, "assiz"), "assimiliation coeff zooplank. (assiz)",
                     value = 0.818, min = 0.4, max = 0.999, step = 0.001)),
      column(4,
         numericInput(NS(id, "azp"), "part of food utilized (azp)",
                     value = 0.6, min = 0.2, max = 0.99, step = 0.01))
        ),
    fluidRow(
      column(4,
             numericInput(NS(id, "c"), "carbon:chlorophyll ratio (c) [mg/μg)]",
                          value = 0.05, min = 0.0, max = 0.02, step = 0.001)),
      column(4,
             numericInput(NS(id, "Y"), "yield coefficient, Chl:P ratio. (Y)",
                          value = 1.00, min = 0.5, max = 1.5, step = 0.01)),
      column(4,
             numericInput(NS(id, "ep"), "ext. of plankton free water (ep) [1/m]",
                          value = 0.2, min = 0.0, max = 2, step = 0.01))
    ),
    fluidRow(
      column(4,
             numericInput(NS(id, "epchl"), "spec. ext. of chlorophyll (epchl) [m²/mg]",
                          value = 0.0017, min = 0.0, max = 0.01, step = 0.00001)),
      column(4,
             numericInput(NS(id, "frz"), "zoopl. filtration rate (frz) [m³/(g*d)]",
                          value = 0.5, min = 0.1, max = 0.9, step = 0.01)),
      column(4,
             numericInput(NS(id, "ik"), "half sat. light (ik) [J/(cm²*d)]",
                          value = 10, min = 0.0, max = 30, step = 0.1))
    ),
    fluidRow(
      column(4,
             numericInput(NS(id, "ks"), "half sat. phosphorous (ks) [mg/m³]",
                          value = 5, min = 0.0, max = 0.10, step = 0.01)),
      column(4,
             numericInput(NS(id, "ksa"), "half sat. grazing  (ksa) [Chl a mg/m³]",
                          value = 60, min = 1, max = 10, step = 1)),
      column(4,
             numericInput(NS(id, "mort"), "zooplankton mortality rate (mort) [1/d]",
                          value = 10, min = 0.0, max = 30, step = 0.1))
    ),
    fluidRow(
      column(4,
             numericInput(NS(id, "photxmax"), "max photosynth. rate (photxmax) [1/d]",
                          value = 1.8, min = 0.00, max = 5, step = 0.01)),
      column(4,
             numericInput(NS(id, "photxmin"), "min photosynth. rate (photxmin) [1/d]",
                          value = 0.17, min = 0.0, max = 2, step = 0.001)),
      column(4,
             numericInput(NS(id, "toptx"), "optimal temp. photosynthesis (deg C)",
                          value = 20, min = 1.0, max = 35, step = 0.1))
    ),
    fluidRow(
      column(4,
             numericInput(NS(id, "sed"), "max photosyneth. rate (photxmax) [1/d]",
                          value = 1.8, min = 0.00, max = 5, step = 0.01)),
      column(4,
             numericInput(NS(id, "photxmin"), "min photosynth. rate (photxmin) [1/d]",
                          value = 0.17, min = 0.0, max = 2, step = 0.001)),
          ),
  )
}

# sed = 0.06, # sedimentation rate (d-1)
# resp = 0.014 # temp. dep. resp. rate (1/(d * deg C))





salmoServer <- function(id){
  moduleServer(id, function(input, output, session){
  })
}
