pkg.list <- 
  c("XML","purrr", "ggforce", 
    "dplyr", "moments",'Rdpack','remotes','vctrs',
    "gridExtra","cowplot","data.table","ggpmisc","ggrepel",
    # "ROI.plugin.glpk","ompr.roi","ompr", 
    "rlang" ,"tidyr", "pacman" ,  "ggbeeswarm","smatr",
    "parallel","doParallel","broom")
local.pkg <- installed.packages()[,"Package"]
new.packages <- pkg.list[!(pkg.list %in% local.pkg)]
if(length(new.packages)) install.packages(new.packages)
# remotes::install_github("Illustratien/toolPhD",dependencies = T)

update.packages("dplyr")
update.packages("purrr")
