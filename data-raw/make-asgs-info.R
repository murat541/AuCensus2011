

library(dplyr)
asgs.info <- read.csv("levels.csv", as.is=TRUE)
save(asgs.info, file="../data/asgs.info.rda", compress="bzip2")
