#Author:Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/03/2022

#This script sets and saves paths to folders needed for all subsequent scripts
# setwd('..') #in Data Characterisation
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
# 
# setwd('..') 
# dir_base<-getwd()
# set the name of the study
StudyName <- "DP4-Migraine"

path_dir<-paste0(projectFolder,"/scripts")
if(dir.exists(path_dir)==F){dir.create(path_dir)}

# path_dir<-paste0(dir_base,"/CDMInstances_preselect/") # use this option if you want to use the preselection files
# path<-path_dir

path_CDM<-paste0(projectFolder,"/CDMInstances/")
invisible(if(dir.exists(path_CDM)==F)
{dir.create(path_CDM)})

preg_folder<-paste0(projectFolder,"/CDMInstances_pregnant/")
invisible(if(dir.exists(preg_folder)==F)
{dir.create(preg_folder)})

preselect_folder<-paste0(projectFolder,"/CDMInstances_preselect/")
invisible(if(dir.exists(preselect_folder)==F)
{dir.create(preselect_folder)})

# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(path_dir, "/g_intermediate")), dir.create(paste0(path_dir, "/g_intermediate")), FALSE))
g_intermediate <- paste0(path_dir, "/g_intermediate")
invisible(ifelse(!dir.exists(paste0(path_dir, "/g_output")), dir.create(paste0(path_dir, "/g_output")), FALSE))
output_dir     <- paste0(path_dir, "/g_output")

invisible(ifelse(!dir.exists(paste0(path_dir, "/g_output/mig_alg_A")), dir.create(paste0(path_dir, "/g_output/mig_alg_A")), FALSE))
output_alg_A     <- paste0(path_dir, "/g_output/mig_alg_A")

invisible(ifelse(!dir.exists(paste0(path_dir, "/g_output/mig_alg_T")), dir.create(paste0(path_dir, "/g_output/mig_alg_T")), FALSE))
output_alg_T     <- paste0(path_dir, "/g_output/mig_alg_T")

invisible(ifelse(!dir.exists(paste0(path_dir, "/g_output/mig_alg_S")), dir.create(paste0(path_dir, "/g_output/mig_alg_S")), FALSE))
output_alg_S     <- paste0(path_dir, "/g_output/mig_alg_S")

# Sets path to p_steps (to read codelists)
pre_dir <- paste0(path_dir,"/p_steps")
invisible(ifelse(!dir.exists(paste0(path_dir, "/p_steps")), dir.create(paste0(path_dir, "/p_steps")), FALSE))
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/populations")), dir.create(paste0(g_intermediate, "/populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/tmp", sep="")), dir.create(paste0(g_intermediate, "/tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")


