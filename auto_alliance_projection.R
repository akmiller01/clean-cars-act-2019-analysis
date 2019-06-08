list.of.packages <- c("data.table","ggplot2","scales","reshape2","bsts","forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/clean-cars-act-2019-analysis"
setwd(wd)

dat = fread("data/alliance_dat.csv")
dat$Date = as.Date(dat$Date)
