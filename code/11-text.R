
# STATISTICS CITED WITHIN TEXT OF PAPER ----------------------------------------
# statistics
rm(list = ls(pattern = "txt"))
txt.expindivsdem <- prettyNum(mean(cd$exp_indivs_dem, na.rm = TRUE), big.mark=",",scientific=F)
txt.expindivsrep <- prettyNum(mean(cd$exp_indivs_rep, na.rm = TRUE), big.mark=",",scientific=F)
txt.gbcount <- nrow(gb)
txt.gbdays <- max(gb[, dte]) - min(gb[, dte]) + 1
txt.final_gbday <- min(gb[, dte])
txt.countgbpollsters <- length(unique(gb[, poll]))
txt.gbpollstermedian <- median(gbcount$N)
txt.gbpollsterstop <- round(100 *sum(gbcount$N[1:2])/ sum(gbcount$N), 0)
txt.gbmedian_samplesize <- median(gb[, sample_size], na.rm = TRUE)
txt.gbundecided <- round(mean(100 - (gb[, dem] + gb[, rep])), 0)
txt.pollcdcount <- length(unique(polls[nmis == 1, stcd]))
txt.countpollsters <- length(unique(polls[, poll]))
txt.pollmedianpercd <- median(pollcount[polls >0, polls])
txt.pollmediansamplesize <- median(polls[, sample_size], na.rm = TRUE)
txt.bhmrmse <- round(RMSE(predsummary$y, predsummary$forecast), 3)
txt.bhmwrongwinner <- round(100 * mean(predsummary$fwinner != predsummary$winner), 0)
txt.finpollnatmean <- round(afdv2010.mean, 3)
txt.finpollnatsd <-  round(afdv2010.sd, 3)
txt.finpollsd <-  round(predsummary2010$rforecast_sd, 3)
txt.predseats2008 <- round(predseats2008,0)
txt.cormaxseats2008 <- round(cd[year == 2008, cor(Q_maxseats, exp)], 3)
txt.cormaj2008 <- round(cd[year == 2008, cor(Q_k218, exp)], 3)
txt.corpartyexp <- round(cd[year >= 2000, cor(exp_dem, exp_rep)], 3)
txt.cordlmfinal <- round(cor.models[Model == "DLM" & month == 0, value], 3)
txt.cordlmnpfinal <- round(cor.models[Model == "DLM (no prior)" & month == 0, value], 3)
txt.corbhmfinal <- round(cor.models[Model == "Hierarchical" & month == 0, value], 3)
txt.meanexpsharedem <- round(mean(cd[year >= 2000, expshare_dem]), 3)
txt.meanexpsharerep <- round(mean(cd[year >= 2000, expshare_rep]), 3)
txt.opendem <- round(coef(lm2.dem)["open"], 3)
txt.reppartyleader <- round(coef(lm3.rep)["repinc:cc661"], 3)

# convert statistics to data frame
l.txtstats <- setNames(lapply(ls(pattern="txt"), function(x) get(x)), ls(pattern="txt"))
txtstats <- data.frame(do.call(rbind, l.txtstats))
rownames(txtstats) <- gsub("txt.", "", rownames(txtstats))

# output to text file to input into latex
txtstats$def <-  "\\def"
names(txtstats)[1] <- "value"
txtstats$value <- as.character(txtstats$value)
txtstats <- data.frame(def = txtstats$def, name = rownames(txtstats), value =  txtstats$value)
txtstats$output <- paste(txtstats[, 1], " ", "\\", txtstats[, 2],
                         "{", txtstats[, 3], "}", sep = "")
fileConn <-file("output/ch1txtstats.txt")
writeLines(txtstats$output, fileConn)
close(fileConn)