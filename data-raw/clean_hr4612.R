rm(list=ls()) 
library("data.table")
hr <- data.table(read.csv(unz("jacobson.zip", "HR4612.csv")))

# FUNCTIONS -------------------------------------------------------------------
SubstrRight <- function(x, n) {
  # Args:
  #   x: character vector
  #   n: number of characters from end of string
  #
  # Returns:
  #   last n characters of a string  
  substr(x, nchar(x)-n+1, nchar(x))
}

# CLEAN -----------------------------------------------------------------------
setkey(hr, stcd, year)

# update missing election winner
hr[(year == 1998 & stcd == 4501) | (year == 2000 & stcd == 4501) |
     (year == 2000 & stcd == 4605), pwin := 9]

# update missing data in louisiana jungle primary. a challenger is coded as being
# a previous office holder if any of the challengers had ever held office. the 
# democratic share of the vote is calculated using the highest vote getters
# from each party. po1 = 8 means that there were multiple challengers
# from the same party running for the election
hr[year == 2002 & stcd == 1805, po1 := 2]
hr[year == 2004 & stcd == 1801, ':=' (po1 = 8, dv = 8, dvp = 12.1)]
hr[year == 2004 & stcd == 1803, ':=' (po1 = 2)]
hr[year == 2004 & stcd == 1806, ':=' (po1 = 8, dv = 21.15)]
hr[year == 2004 & stcd == 1807, ':=' (po1 = 2)]
hr[year == 2006 & stcd == 1801, ':=' (po1 = 8, dv = 7.72)]
hr[year == 2006 & stcd == 1802, ':=' (po1 = 8)]
hr[year == 2006 & stcd == 1804, ':=' (po1 = 8, dv = 22.80)]
hr[year == 2008 & stcd == 1801, ':=' (po1 = 0, dvp = 7.72)]
hr[year == 2008 & stcd == 1802, ':=' (po1 = 0, dv = 48.59)]
hr[year == 2008 & stcd == 1804, ':=' (po1 = 4, dv = 49.8, dvp = 22.80)]
hr[year == 2012 & stcd == 1801, ':=' (po1 = 0, dv = 24.23)]
hr[year == 2012 & stcd == 1802, ':=' (po1 = 8, dv = 80.3)]

# update other missing previous office holder
hr[year == 2006 & stcd == 4323, ':=' (po1 = 8)]
hr[year == 2006 & stcd == 4325, ':=' (po1 = 0)]
hr[year == 2012 & stcd == 801, ':=' (po1 = 1)]

# consider bernie sanders a democrat
hr[stcd == 4501 & year == 1990, ':=' (dv = 56)]
hr[stcd == 4501 & year == 1992, ':=' (dvp = 56, dv = 58)]
hr[stcd == 4501 & year == 1994, ':=' (dvp = 58, dv = 50)]
hr[stcd == 4501 & year == 1996, ':=' (dvp = 50, dv = 55)]
hr[stcd == 4501 & year == 1998, ':=' (dvp = 55, dv = 63)]
hr[stcd == 4501 & year == 2000, ':=' (dvp = 63, dv = 69)]
hr[stcd == 4501 & year == 2002, ':=' (dvp = 69, dv = 64)]
hr[stcd == 4501 & year == 2004, ':=' (dvp = 64, dv = 67)]
hr[stcd == 4501 & year == 2006, ':=' (dvp = 67)]

hr[stcd == 4501 & year >= 1990 & year <= 2004, pwin := 1]
hr[stcd == 4501 & year >= 1992 & year <= 2004, inc := 1]

# other changes in vermont
hr[stcd == 4501 & year == 2008, ':=' (dv = 83)]
hr[stcd == 4501 & year == 2010, ':=' (dvp = 83)]

# virgil goode in virginia (treat as republican starting in 2000)
hr[stcd == 4605 & year == 2000, ':=' (dv = 36.52, pwin = 0, inc = 0)]
hr[stcd == 4605 & year == 2002, ':=' (dvp = 36.52)]

# edit variables
setnames(hr, c("dv", "dvp", "pwin", "dpres"), c("houdv", "l_houdv", "houwin", "presdv"))
hr[, ':=' (houdv = houdv/100, l_houdv = l_houdv/100, presdv = presdv/100)]

# impute uncontested seats
hr[houwin == 1 & is.na(houdv), uncontested := 1]
hr[houwin == 0 & is.na(houdv), uncontested := -1]
hr[!is.na(houdv), uncontested := 0]
hr[, houdvimp := ifelse(uncontested == 0, houdv, ifelse(uncontested == 1, 0.75, 0.25))]

# impute lagged uncontested seats
hr[, l_houwin :=  c(NA, houwin[-.N]), by = stcd]
hr[l_houwin == 1 & is.na(l_houdv), l_uncontested := 1]
hr[l_houwin == 0 & is.na(l_houdv), l_uncontested := -1]
hr[!is.na(l_houdv), l_uncontested := 0]
hr[, l_houdvimp := ifelse(l_uncontested == 0, l_houdv, ifelse(l_uncontested == 1, 0.75, 0.25))]

# create new variables
hr[, cd := as.numeric(SubstrRight(stcd, 2))]
hr[, st_no := as.numeric(mapply(sub, formatC(cd, width = 2, flag = "0"), "", stcd))]
hr[, i_inc := ifelse(inc == 0 | inc == 1, 1, 0)]
hr[, houinc := ifelse(inc == 0, -1, ifelse(inc == 1, 1, 0))]
hr[, houcontrol := ifelse(inc == 0 | inc == 3, -1, ifelse(inc == 1 | inc == 2, 1, 0))]
hr[, fresh := ifelse(houinc == 1 & (fr > 0 & fr < 4), 1, 
                     ifelse(houinc == -1 & (fr > 0 & fr < 4) , -1, 0))]
hr[, i_po := ifelse((po1 == 1 & inc == 1) | (po1 == 3 & i_inc == 0), -1, 
                    ifelse((po1 == 1 & inc == 0) | (po1 == 2 & i_inc == 0), 1, 0))] # former political office
hr[,i_hr := ifelse(po2 == 8 & (inc == 0 | inc == 3), 1, 
                   ifelse(po2 == 8 & (inc == 1 | inc == 2), -1, 0))] # former house member

# more lags
hr[, c("l_presdv") := lapply(.SD, function(x) c(NA, x[-.N])), by = stcd, .SDcols= c("presdv")]
hr[, c("l2_houdvimp","l2_presdv") := lapply(.SD, function(x) c(NA, x[-.N])),
   by = stcd, .SDcols= c("l_houdvimp",  "l_presdv")]

# change stcd to abbreviations-cd
states <- fread("states.csv")
hr <- merge(hr, states, by = "st_no")
hr[, stcd := paste0(st_usps, formatC(cd, width = 2, format = "d", flag = "0"))]
hr[, c("st_no", "cd") := NULL]

# SAVE ------------------------------------------------------------------------
saveRDS(hr, "hr.rds")

