library(devtools)
# 04.05. starting project
use_git()
use_r("logistisches_wachstum.R")
load_all()

install.packages("available")
library(available)
available("introEcoModels")

Nt
check()
document()
?Nt

usethis::git_default_branch_configure()
use_package("shiny")
use_r("app.R")

#05.05
use_package("ggplot2")
document()
load_all()
use_rstudio()
use_package("pkgload")

#10.05
install.packages("shinyjs")
usethis::use_package("shinyjs")

#13.05
usethis::use_package("htmltools")
getwd()
load(paste0(getwd(), "/../../Programming_Lisa/hydrodata.Rdata"))
usethis::use_data(hydrodat)
rsconnect::deployApp()

#17.05
ebola <- read.csv("../../Programming_Lisa/ebola.csv")
head(ebola)
usethis::use_data(ebola)

#19.05
usethis::use_package("deSolve")
usethis::use_package("tidyr")
usethis::use_package("dplyr")

#25.05
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("CircStats")

#13.07
install.packages("devtools")

usethis::use_package("deSolve")
usethis::use_package("tidyr")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("CircStats")

#05.10
rsconnect::setAccountInfo(name='simon-mayer', token='7D50025DB00961E0D25C868B6DA76821', secret='4iea80UtYmNBFokqWTu0mCzWR5ULJqmmo8olkD43')
rsconnect::deployApp()
