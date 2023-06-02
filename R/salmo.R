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
                          value = 5, min = 0.1, max = 10, step = 0.1)),
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
             numericInput(NS(id, "sed"), "sedimentation rate  (sed) [1/d]",
                          value = 0.06, min = 0.00, max = 0.5, step = 0.001)),
      column(4,
             numericInput(NS(id, "resp"), "temp. dep. respiration rate (resp) [1/(d * °C)]",
                          value = 0.014, min = 0.0, max = 0.2, step = 0.0001)),
      column(4,
             numericInput(NS(id, "end"), "Endzeitpunkt  [Tage]",
                   value = 365, min = 10, max = 3650, step = 5)),
          ),
    fluidRow( h2("Initial State")),
    fluidRow(
      column(4,
             numericInput(NS(id, "p"), "phosphorous concentrattion (p) [??]",
                          value = 10, min = 0.00, max = 5, step = 0.01)),
      column(4,
             numericInput(NS(id, "x"), "phytoplankton concentration  [??]",
                          value = 0.5, min = 0.0, max = 5, step = 0.01)),
      column(4,
             numericInput(NS(id, "z"), "zooplankton concentration (z) [??]",
                          value = 0.1, min = 0, max = 5, step = 0.01))
    ),
    fluidRow(plotOutput(NS(id, "plot"))),
    fluidRow(
      column(1, actionButton(NS(id, "salmo_model_button"), "Show / Hide Function Code",
                             onClick="show_hide('salmo_model_function')"))),
    fluidRow(
      column(12, div(label=NS(id, "salmo_model_box"), id="salmo_model_function",
                     verbatimTextOutput(NS(id, "salmo_model_function_text")))))
  )
}




salmoServer <- function(id){
  moduleServer(id, function(input, output, session){
    salmo_model_function_text = "
model <- function(time, xx, parms, pscale = 1) {

  # quantities derived by time in year
  temp = 12+10*sin((time+220)*pi/182) # (deg C)
  #photoactive radiation
  par = 0.5*(776.33+622.36*sin(2*pi*(time+284)/365))# [J/(cm²*d]
  ice = ifelse(time < 90, 1, 0) # (ice cover)
  # ice = 0 # alternatively
  # ??
  zmix = ifelse(time < 90 | time > 240, 20, 4) # (m)
  # phosphorous intake
  pin = pscale*ifelse(time > 200 & time < 240, 40, 20) # (mu g/L)
  # pin = 100 # alternatively
  # ??
  qperve = 0.01 # (1/d)

  with (as.list(c(xx, parms)),{

    # quantitites derived with the help of some parameters
    eptot <- epchl * x + ep
    imean <- (par-par*exp(-eptot*zmix))/eptot/zmix*(1-0.9*ice)
    phoxl <- imean/(ik + imean)
    phoxp <- p/(ks + p)
    phoxt <- (photxmax - photxmin)/toptx*temp + photxmin
    pimport <- qperve * (pin - p)
    photosynthese <- phoxl * phoxp * phoxt * x
    respiration <- x * resp * temp
    growth <- photosynthese - respiration
    grazing <- z * x * frz * assiz * ksa/(ksa+x)
    zooexcr <- grazing * (1 - azp)
    sedimentation <- x * sed
    mortz <- z * mort

    dp <- pimport + zooexcr - growth
    dx <- 1/Y * growth - grazing - sedimentation
    dz <- zin + c * azp * grazing - mortz

    list(c(dp, dx, dz))
  })
}
"
    output$salmo_model_function_text = renderText({salmo_model_function_text})
    xstart <- reactive({c(
      p = input$p,
      x = input$x,
      z = input$z)
    })
    parms <- reactive({
      c(
        zin = input$zin,
        assiz = input$assiz,
        azp = input$azp,
        c = input$c,
        Y = input$Y,
        ep = input$ep,
        epchl = input$epchl,
        frz = input$frz,
        ik = input$ik,
        ks = input$ks,
        ksa = input$ksa,
        mort = input$mort,
        photxmax = input$photxmax,
        photxmin = input$photxmin,
        toptx = input$toptx,
        sed = input$sed,
        resp = input$resp)
    })
    times <- reactive({seq(1, input$end, 1)})

    output$plot <- renderPlot({
      plot(deSolve::ode(xstart(), times(), salmo_model, parms()))
    })

  })
}




salmo_model <- function(time, xx, parms, pscale = 1) {

  # quantities derived by time in year
  temp = 12+10*sin((time+220)*pi/182) # (deg C)
  # photo active radiation
  par = 0.5*(776.33+622.36*sin(2*pi*(time+284)/365))# [J/(cm²*d]
  ice = ifelse(time < 90, 1, 0) # (ice cover)
  # ice = 0 # alternatively
  # ??
  zmix = ifelse(time < 90 | time > 240, 20, 4) # (m)
  # phosphorous intake
  pin = pscale*ifelse(time > 200 & time < 240, 40, 20) # (mu g/L)
  # pin = 100 # alternatively
  # ??
  qperve = 0.01 # (1/d)

  with (as.list(c(xx, parms)),{

    # quantitites derived with the help of some parameters
    eptot <- epchl * x + ep
    imean <- (par-par*exp(-eptot*zmix))/eptot/zmix*(1-0.9*ice)
    phoxl <- imean/(ik + imean)
    phoxp <- p/(ks + p)
    phoxt <- (photxmax - photxmin)/toptx*temp + photxmin
    pimport <- qperve * (pin - p)
    photosynthese <- phoxl * phoxp * phoxt * x
    respiration <- x * resp * temp
    growth <- photosynthese - respiration
    grazing <- z * x * frz * assiz * ksa/(ksa+x)
    zooexcr <- grazing * (1 - azp)
    sedimentation <- x * sed
    mortz <- z * mort

    dp <- pimport + zooexcr - growth
    dx <- 1/Y * growth - grazing - sedimentation
    dz <- zin + c * azp * grazing - mortz

    list(c(dp, dx, dz))
  })
}

