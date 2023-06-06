# install required packages
source("src/modules/set_up.R")
# project folder structure-------------------------------------------------------------------------
# put folders contains only .xml under "data"
# each folder is one batch of experiment
dir.create(file.path("./data"), showWarnings = FALSE)
dir.create(file.path("./result"), showWarnings = FALSE)
# read files-------------------------------------------------------------------------
# read xml, calculate row class,
# slope and generte checking graph
system.time(
  source("src/modules/read_xml.R")
)

# run statistics-------------------------------------------------------------------------

# calculate basics statistics
system.time(
  source("src/modules/stat_analysis.R")
)

# picture wise -------------------------------------------------------------------------
system.time(
  source("src/modules/summarize_and_merge.R")
)

