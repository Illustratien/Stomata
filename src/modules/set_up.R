pkg.list <- 
  c("XML","purrr", "ggforce",   
    "dplyr", "moments","ROI.plugin.glpk","ompr.roi","ompr", 
    "rlang" ,"tidyr", "pacman" ,   
    "parallel","doParallel","broom")
local.pkg <- installed.packages()[,"Package"]
new.packages <- pkg.list[!(pkg.list %in% local.pkg)]
if(length(new.packages)) install.packages(new.packages)