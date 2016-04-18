# set window length for models
w.len <- 14
polls[, pte := ceiling(as.numeric((eldate - date_mid)/w.len))] # periods to event
gb[, pte := ceiling(as.numeric((eldate - date_mid)/w.len))]
print(as.Date("2010-11-02") - w.len * 4) # first day forecast is made for
ynew2010 <- bhm.pred$y_new[which(bhm.pred$year == 2010),]
uncontested2010 <- matrix(rep(uncontested[year == 2010, y], n.sims), ncol = n.sims)
ynew2010.full <- rbind(ynew2010, uncontested2010)
predsummary2010 <- predsummary[year == 2010]
