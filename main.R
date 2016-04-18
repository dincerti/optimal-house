# setup
rm(list=ls()) 
setwd("C:/Users/Devin/Dropbox/Projects/Optimal Resources House Elections")
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
if (!file.exists("output/ohdata.RData")){
  for(con in dbListConnections(MySQL())) dbDisconnect(con)
  source("code/1-load.R")
  source("code/2-clean.R")  
}
source("code/3-do_setup.R")
source("code/4-do_descstats.R")
source("code/5-do_hlm.R")
source("code/6-do_dlmsetup.R")
source("code/7-do_dlmnat.R")
source("code/8a-do_dlmcd_load.R")
if (!file.exists("output/dlmPosterior.Rdata")){
  source("code/8b-do_dlmcd_run.R")
}
if (!file.exists("output/dlmnpPosterior.Rdata")){
  source("code/8c-do_dlmcd_nprun.R")
}
source("code/8d-do_dlmcd_results.R")
source("code/9-do_forecasts.R")
source("code/10-do_Q.R")
source("code/11-text.R")


