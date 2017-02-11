# setup
rm(list=ls()) 
packages <- c("ggplot2", "foreign", "plyr", "lmtest", "reshape2", "data.table", 
              "lme4", "rstan", "MASS", "xtable", "scales", "arm", "dplyr",
              "RMySQL", "sandwich", "plm", "dlm", "coda", "MCMCpack", "gridExtra",
              "Hmisc", "parallel", "doSNOW")
lapply(packages, library, character.only = TRUE)
source("code/func.R")
source("code/dlmnat.R")
source("code/dlm.R")
set.seed(100)

# code
if (!file.exists("output/ohdata-clean.RData")){
  source("code/01-clean.R")  
}
source("code/02-do_setup.R")
source("code/03-do_descstats.R")
source("code/04-do_hlm.R")
source("code/05-do_dlmsetup.R")
source("code/06-do_dlmnat.R")
source("code/07a-do_dlmcd_load.R")
if (!file.exists("output/dlmPosterior.rds")){
  source("code/07b-do_dlmcd_run.R")
}
if (!file.exists("output/dlmnpPosterior.rds")){
  source("code/07c-do_dlmcd_nprun.R")
}
source("code/07d-do_dlmcd_results.R")
source("code/08-do_forecasts.R")
source("code/09-do_Q.R")
source("code/10-text.R")


