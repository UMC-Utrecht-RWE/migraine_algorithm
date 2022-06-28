#Author: Ema Alsina MSc.
#email:e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/04/2022

# runs Migraine Algorithms

#if preselect script has not been run, set to TRUE, otherwise FALSE



rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


source("99_path.R")

source(paste0(pre_dir, "/packages.R"))

source(paste0(pre_dir, "/pregnancy_filter.R"))

source(paste0(pre_dir, "/lookback_function.R"))

source(paste0(pre_dir, "/MIG_ALG_PREG.R"))

lookback_windows<-c((-1825), (-365),(-90))

for(i in 1:length(lookback_windows)){
my_lookback<-lookback_windows[i]
source(paste0(pre_dir, "/lookback_migraine.R"))
}



