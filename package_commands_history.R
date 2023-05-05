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

