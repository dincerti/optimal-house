# SETUP DATA FOR DLM GIBBS SAMPLER  -------------------------------------------
# prior
rnew2010 <- matrix(NA, ncol = ncol(ynew2010), nrow = nrow(ynew2010))
for (i in 1:ncol(rnew2010)){
  rnew2010[, i] <- ynew2010[, i] - afdv2010[i] 
}  
predsummary2010[, rforecast_mean := apply(rnew2010, 1, mean)]
predsummary2010[, rforecast_sd := mean(apply(rnew2010, 1, sd))]

# data
polls <- merge(polls, predsummary2010[, !"y", with = FALSE],
               by = c("stcd", "year"), all.x = TRUE)
polls <- merge(polls, natopinion, by = "pte", all.x = TRUE)
saveRDS(polls, file = "output/dlmdata.rds")


