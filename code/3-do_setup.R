# SET UP ----------------------------------------------------------------------
theme_set(theme_bw())
load("output/ohdata.RData")
varlabs <- fread("data/varlabs.csv")
yvar <- "houdv"
xvars <- c("l_houdvimp", "c_l_presdv", "houinc", "rdwnom1", 
           "fresh", "i_po", "prescontrolXaugapr", "gbdv_aug", "midterm")
xlabs <- varlabs$lab[match(xvars, varlabs$var)]
xlabs.short <- varlabs$labshort[match(xvars, varlabs$var)]
predyears <- c(2000, 2004, 2006, 2008, 2010)