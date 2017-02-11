rm(list=ls()) 
packages <- c("reshape2", "data.table", "RMySQL")
lapply(packages, library, character.only = TRUE)

# LOAD DATA -------------------------------------------------------------------
dwnom <- data.table(read.dta("dwnom.dta"))
setnames(dwnom, c("state"), c("st_icpsr"))
states <- fread("states.csv")
cong <- data.frame(cong = seq(1, 113), year = seq(1790, 2014, 2))

# CLEAN -----------------------------------------------------------------------
dwnom <- merge(dwnom, states, by = "st_icpsr", all.x = TRUE)
dwnom[, stcd := paste0(st_usps, formatC(cd, width = 2, format = "d", flag = "0"))]
dwnom[, c("st_icpsr", "cd", "statenm", "st_usps") := NULL]
dwnom <- merge(dwnom, cong, by = "cong")
setcolorder(dwnom, c("stcd", "cong", "year",
                     names(dwnom)[-which(names(dwnom) %in% c("stcd", "cong", "year"))]))

# SAVE-------------------------------------------------------------------------
saveRDS(dwnom, "dwnom.rds")