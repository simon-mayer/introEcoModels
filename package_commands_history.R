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
