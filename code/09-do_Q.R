# CALCULATE Q -----------------------------------------------------------------
###
### CALCULATIONS AND SETTING UP DATA FRAME FOR ANALYSIS
###
# Q from hierarchical model
qmaj <- qk145 <- qk290 <- qmaxseats <- list()
for (t in 1:length(predyears)){
  qmaj[[t]] <- QKRule(predyears[t], 218)
  #  qk145[[t]] <- QKRule(predyears[t], 145)
  #  qk290[[t]] <- QKRule(predyears[t], 290)
  qmaxseats[[t]] <- QMaxSeats(predyears[t])  
}
qmaj <- do.call(rbind, qmaj)
#qk145 <- do.call(rbind, qk145)
#qk290 <- do.call(rbind, qk290)
qmaxseats <- do.call(rbind, qmaxseats)

# Q from dlm
qdlm <- qdlmnp <- list()
for (t in 1:length(dlm.sims)){
  qdlm[[t]] <- QDlmMaxseats(t, TRUE)
  qdlmnp[[t]] <- QDlmMaxseats(t, FALSE)
}
qdlm <- do.call(rbind, qdlm)
qdlmnp <- do.call(rbind, qdlmnp)
setnames(qdlm, "Q", "Q_dlm")
setnames(qdlmnp, "Q", "Q_dlmnp")

# combining Q with dataframe cd
cd[, exp_cc := exp_dccc + exp_nrcc]
cd <- data.table(Reduce(function (x, y) merge(x, y, by = c("year", "stcd"), all.x = TRUE), 
                        list(cd, qmaxseats, qmaj)))
for (j in names(cd)[grepl("Q_", names(cd))]){
  set(cd, which(cd$year >= min(predyears) & cd$uncontested !=0), j, 0)
}
cd[, c("Qshare_maxseats", "Qshare_k218",
       "expshare_pacs", "expshare_dem", "expshare_rep", "expshare_fin",
       "expshare_pacs_fin_dem", "expshare_pacs_fin_rep") := 
     lapply(.SD, function(x) 100 * x / sum(x)),
   by = year, .SDcols = c("Q_maxseats", "Q_k218",
                          "exp_pacs", "exp_dem", "exp_rep", "exp_fin_inc",
                          "exp_pacs_fin_dem", "exp_pacs_fin_rep")]
cd <- merge(cd, predsummary, by = c("stcd", "year"), all.x = TRUE)
cd[, ':=' (margin = abs(houdv - 0.5), marginf = abs(forecast - 0.5), 
           fresh_abs = abs(fresh), dwnom1_abs = abs(dwnom1),
           open = ifelse(i_inc == 1, 0, 1), i_cc = ifelse(numcc > 0, 1, 0),
           deminc = ifelse(houinc == 1, 1, 0), 
           repinc = ifelse(houinc == -1, 1, 0),
           dem_po = ifelse(i_po == 1, 1, 0),
           rep_po = ifelse(i_po == -1, 1, 0))]
cd[year >= 2000, margin := ifelse(uncontested == -1 | uncontested == 1, 0.5, margin)]
cd[year >= 2000, marginf := ifelse(uncontested == -1 | uncontested == 1, 0.5, marginf)]
cd[year >= 2000, pwin := ifelse(uncontested == -1, 0, ifelse(uncontested == 1, 1, pwin))]

###
### PROBABILITY OF BEING A DECISIVE SWING STATE 
###
probDecisiveSwing <- function(t, sims){
  fixedpred <- FixedPredict(t, sims)
  decisive.swing <- DecisiveSwing(t, fixedpred)
  decisive <- merge(decisive.swing, cd[year==t, .(stcd, Q_maxseats, Q_k218)], by = c("stcd"))
  print(cor(decisive$swing, decisive$Q_k218))
  print(cor(decisive$swing, decisive$Q_maxseats)) 
  return(decisive)
}
#probDecisiveSwing(2008, 1000000)

###
### PLOT OF Q VS FORECASTED VOTE 
###
predseats2008 <- mean(apply(bhm.pred$y_new[which(bhm.pred$year == 2008), ] <= 0.5, 2, sum)) + 
  sum(uncontested[year == 2008, repwin])
q.data <- cd[year == 2008, .(forecast, Q_maxseats, Q_k218, stcd)]
q.data <- melt(q.data, id = c("stcd", "forecast"))
q.data[, variable :=  revalue(variable, c("Q_maxseats" = "Maximizing seats", 
                                          "Q_k218" = "Maximizing probability of majority"))]
p.q <- ggplot(q.data, aes(x = forecast, y = value)) + geom_point(size = .8) + 
  facet_wrap(~variable, scales = "free_y") + 
  xlab("Forecasted Democratic vote share") + ylab("Q")
ggsave("figs/qplot.pdf", p.q, height = 5, width = 6.5)

# RELATIONSHIP BETWEEN Q AND SPENDING -----------------------------------------
###
### COREELATON PLOT: Q AND SPENDING BY PAC TYPE
###
cor.type <- cd[year >= 2000, 
               .(qmax_natorg = cor(Q_maxseats, exp_natorg),
                 qmax_con = cor(Q_maxseats, exp_con),
                 qmax_ally = cor(Q_maxseats, exp_ally), 
                 qmax_other = cor(Q_maxseats, exp_other),
                 qmax_indivs = cor(Q_maxseats, exp_indivs),
                 qmaj_natorg = cor(Q_k218, exp_natorg),
                 qmaj_con = cor(Q_k218, exp_con),
                 qmaj_ally = cor(Q_k218, exp_ally), 
                 qmaj_other = cor(Q_k218, exp_other),
                 qmaj_indivs = cor(Q_k218, exp_indivs)),
               by = year]
cor.type <- melt(cor.type, id = "year")
cor.type[, Q := ifelse(grepl("qmax", variable), "Maximizing seats", "Maximizing probability of majority")]
cor.type[, variable := gsub("qmax_|qmaj_", "", variable)]
cor.type[, Type := mapvalues(variable, from = c("natorg", "con", "ally", "other", "indivs"),
                             to = c("National party committees", "Party-connected committees",
                                    "Allied PACs", "Other PACs", "Individuals"))]

#cor.type[, Type := ifelse(grepl("_cc", variable), "Party committees",
#                          ifelse(grepl("_indivs", variable), "Individuals", "All PACS"))]
p.cor.type <- ggplot(cor.type, aes(x = year, y = value, col = Type)) + 
  xlab("Year") + ylab("Correlation") + facet_wrap(~ Q, ncol = 1) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = pretty_breaks()) 
ggsave("figs/corbytype.pdf", p.cor.type, height = 5, width = 6.5)

###
### CORRELATION PLOT: ELECTORAL COMPETITIVENESS AND SPENDING 
###
cor.comp <- cd[year >= 2000, 
               .(qmax = cor(Q_maxseats, exp),
                 margin = abs(cor(margin, exp)),
                 marginf = abs(cor(marginf, exp))),
               by = year]
cor.comp <- melt(cor.comp, id = "year", variable.name = "Measure")
p.cor.comp <- ggplot(cor.comp, aes(x = year, y = value, col = Measure)) + 
  xlab("Year") + ylab("Correlation") +
  geom_point() + geom_line() + scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom") + 
  scale_colour_discrete(labels = c(expression(Q[i]^seats), "Margin (actual)", "Margin (forecasted)"))
ggsave("figs/corbymeasure.pdf", p.cor.comp, height = 4, width = 6)

###
### CORRELATION PLOT: Q AND SPENDING BY PARTY
###
cor.party <- cd[year >= 2000, .(qmax_D = cor(Q_maxseats, exp_dem),
                                qmax_R = cor(Q_maxseats, exp_rep),
                                qmaj_D = cor(Q_k218, exp_dem),
                                qmaj_R = cor(Q_k218, exp_rep)),
                by = year]
cor.party <- melt(cor.party, id = "year")
cor.party[, Q := ifelse(grepl("qmax", variable), "Maximizing seats", "Maximizing probability of majority")]
cor.party[, Party := ifelse(grepl("D", variable), "Democrat", "Republican")]
cor.party$Party <- factor(cor.party$Party, levels = c("Republican", "Democrat", "All"))
p.cor.party <- ggplot(cor.party, aes(x = year, y = value, col = Party)) + 
  xlab("Year") + 
  ylab("Correlation") + facet_wrap(~ Q, ncol = 1) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = pretty_breaks()) 
ggsave("figs/corbyparty.pdf", p.cor.party, height = 5, width = 6.5)

###
### CORRELATION PLOT: Q AND SPENDING BY PRESIDENTIAL ELECTION COMPETITIVENESS
###
cd[, swingst := ifelse(st_presdv >= .45 & st_presdv <= .55, 1, 0)]
table(cd[year >= 2000 , .(swingst, year)])
cor.swingst <- cd[year >= 2000 , .(qmax = cor(Q_maxseats, exp)),
                  by = .(year, swingst)]
cor.swingst[, swingst := factor(swingst, levels = c(1, 0), labels = c("Yes", "No"))]
p.cor.swingst <- ggplot(cor.swingst, aes(x = year, y = qmax, col = swingst)) + 
  xlab("Year") + ylab("Correlation") +
  geom_point() + geom_line() +
  scale_color_discrete(name="Battleground \nstate") +
  scale_x_continuous(breaks = pretty_breaks()) 
ggsave("figs/corbyswingstate.pdf", p.cor.swingst, height = 4, width = 6.5)

###
### OLS: INFLUENCE VS ELECTION MOTIVATED?
###
scatter <- ggplot(cd, aes(x = Qshare_maxseats, y = expshare_pacs)) + 
  geom_point() +  geom_smooth(method=lm)
cd[, i_uncontested := ifelse(uncontested == 0, 0, 1)]

# democrats
lm1.dem <- lm(expshare_dem ~ Qshare_maxseats, cd)
lm1a.dem <- lm(expshare_dem ~ Qshare_maxseats*presyear, cd)
lm2.dem <- lm(expshare_dem ~ Qshare_maxseats + open + deminc, cd)
lm3.dem <- lm(expshare_dem ~ Qshare_maxseats + open + deminc + deminc:cc196 + deminc:cc661 + deminc:chair, cd)
lm4.dem <- lm(expshare_dem ~ Qshare_maxseats + open + deminc + deminc:cc196 + deminc:cc661 + deminc:chair + pwin, cd)
lm5.dem <- lm(expshare_dem ~ Qshare_maxseats + open + deminc + deminc:cc196 + deminc:cc661 + deminc:chair + pwin +
                factor(stcd), cd[year > 2002])
lm.dem.table <- RegTable(list(lm1.dem, lm2.dem, lm3.dem, lm4.dem, lm5.dem), 
                         vars = c("Qshare_maxseats", "open", "deminc", "deminc:cc196", "deminc:cc661", "deminc:chair", "pwin"), 
                         varnames = c("Q share (maximizing seats)", "Open seat",
                                      "Incumbent", "Ways and Means Committee", "Party Leadership", 
                                      "Committee Chair", "Probability of Victory"))
print(xtable(lm.dem.table), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity, hline.after = NULL,
      file = "tables/regexp_dem.txt")

# republicans
lm1.rep <- lm(expshare_rep ~ Qshare_maxseats, cd)
lm1a.rep <- lm(expshare_rep ~ Qshare_maxseats*presyear, cd)
lm2.rep <- lm(expshare_rep ~ Qshare_maxseats + open + repinc, cd)
lm3.rep <- lm(expshare_rep ~ Qshare_maxseats + open + repinc + repinc:cc196 + repinc:cc661 + repinc:chair, cd)
lm4.rep <- lm(expshare_rep ~ Qshare_maxseats + open + repinc + repinc:cc196 + repinc:cc661 + repinc:chair + pwin, cd)
lm5.rep <- lm(expshare_rep ~ Qshare_maxseats + open + repinc + repinc:cc196 + repinc:cc661 + repinc:chair + pwin +
                factor(stcd), cd[year > 2002])
lm.rep.table <- RegTable(list(lm1.rep, lm2.rep, lm3.rep, lm4.rep, lm5.rep), 
                         vars = c("Qshare_maxseats", "open", "repinc", "repinc:cc196", "repinc:cc661", "repinc:chair", "pwin"), 
                         varnames = c("Q share (maximizing seats)", "Open seat",
                                      "Incumbent", "Ways and Means Committee", "Party Leadership", 
                                      "Committee Chair", "Probability of Victory"))
print(xtable(lm.rep.table), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity, hline.after = NULL,
      file = "tables/regexp_rep.txt")

###
### SPENDING CONTENTRATION BY INDUSTRY AND COMMITTEE TYPES 
###
indexp <- merge(pacs[CRPICO == "I", .(year, stcd, rAmount, Sector)],
                cd[, grep("cc\\d\\d\\d|year|^stcd", colnames(cd)), with = FALSE], 
                by = c("year", "stcd"), all.x = TRUE)
indexp <- melt(indexp, id = c("year", "stcd", "rAmount", "Sector"))
indexp <- indexp[value == 1 ]
indexp <- indexp[, .(exp = sum(rAmount)), by = c("Sector", "variable")]
indexp[, exp := 100 * exp/sum(exp), by = "variable"]
indexp <- indexp[order(-exp)]

###
### DETERMINANTS OF SPENDING BY FINANCE INDUSTRY
###
# Q share for incumbents
Qincshare_maxseats <- cd[i_inc == 1 & year >= 2000, .(stcd, year, Q_maxseats)]
Qincshare_maxseats[, Qincshare_maxseats := 100 * Q_maxseats/sum(Q_maxseats), by = "year"]
cd <- merge(cd, Qincshare_maxseats[, .(stcd, year, Qincshare_maxseats)], by = c("stcd", "year"), all.x = TRUE)

# OLS by party
lm.fin.dem <- lm(expshare_pacs_fin_dem ~ Qshare_maxseats + open + deminc + deminc:cc113 + deminc:finchair, cd)
lm.fin.rep <- lm(expshare_pacs_fin_rep ~ Qshare_maxseats + open + repinc + repinc:cc113 + repinc:finchair, cd)

# OLS for incumbents
lm1.fin <- lm(expshare_fin ~ Qshare_maxseats , cd[open == 0])
lm2.fin <- lm(expshare_fin ~ Qshare_maxseats + cc113 + finchair, cd[open == 0])
lm3.fin <- lm(expshare_fin ~Qshare_maxseats + cc113 + finchair + factor(stcd), 
              cd[year > 2000 & open == 0])
lm.fin.table <- RegTable(list(lm1.fin, lm2.fin, lm3.fin), 
                         vars = c("Qshare_maxseats", "cc113", "finchair"), 
                         varnames = c("Q share (maximizing seats)", "On Financial Services Committee",
                                      "Chair/ranking minority member of Finance Committee"))
print(xtable(lm.fin.table), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity, hline.after = NULL,
      file = "tables/regexp_fin.txt")  

# POLLS, Q AND SPENDING DURING 2010 ELECTION ----------------------------------
###
### EVOLUTION OF 2010 FORECASTS
###
tmp <- melt(predsummary2010[, .(stcd, forecast_np1, forecast_np2, forecast_np3, forecast_np4)], id = "stcd")
tmp[, fpte := as.numeric(gsub("\\D","", variable))]
sd <- tmp[, .(sd = sd(value)), by = "stcd"]
sd <- sd[order(-sd)]
sd <- sd[1:10]
tmp <- merge(tmp, sd, by = "stcd")
ggplot(tmp, aes(x = fpte, y = value, col = stcd)) + geom_line() 

###
### 2010 DATASET
###
# rectangular data table...note that periods to election corresponds to periods
# from the polls lagged one period since it refers to a forecast period using
# lagged values of polling data
dates <- rep(seq(as.Date('2010-09-01'), as.Date('2010-11-01'), by = 1))
cd2010 <- data.table(stcd = rep(cd[year == 2010, stcd], each = length(dates)),
                     date_mid = rep(dates, 435))
cd2010[, ':=' (pte = 1 + ceiling(as.numeric((as.Date("2010-10-31") - date_mid)/14)))]

# add polling data and fill missing observations
tmp <- polls[pollyear == 2010, .(dv = mean(dv)), by = c("stcd", "date_mid")]
cd2010 <- merge(cd2010, tmp, by = c("stcd", "date_mid"), all.x = TRUE)
cd2010[, dv := na.locf(dv, na.rm = FALSE), by = "stcd"]

# add pac expenditure data
tmp <- pacs[month(Date) >= 9 & yday(Date) <= yday(eldate) & year == 2010, 
            .(exp_pacs = sum(rAmount)), by = c("stcd", "Date")]
setnames(tmp, "Date","date_mid")
cd2010 <- merge(cd2010, tmp, by = c("stcd", "date_mid"), all.x = TRUE)
cd2010[, exp_pacs := ifelse(is.na(exp_pacs), 0, exp_pacs)]

# add individual expenditure data
tmp <- indivs[month(Date) >= 9 & yday(Date) <= yday(eldate) & year == 2010, 
              .(exp_indivs = sum(rAmount)), by = c("stcd", "Date")]
setnames(tmp, "Date","date_mid")
cd2010 <- merge(cd2010, tmp, by = c("stcd", "date_mid"), all.x = TRUE)
cd2010[, exp_indivs := ifelse(is.na(exp_indivs), 0, exp_indivs)]
cd2010[, exp := exp_pacs + exp_indivs] # combine individual and pac expenditures

# collapse data by forecast period
cd2010 <- cd2010[, .(dv = mean(dv), exp = sum(exp)), by = c("stcd", "pte")]
cd2010 <- cd2010[order(stcd, pte)]
cd2010[, expcum := cumsum(exp), by = "stcd"]

# add values of Q
cd2010 <- merge(cd2010, qmaxseats[year == 2010], by = "stcd", all.x = TRUE)
cd2010[, Q_maxseats := ifelse(is.na(Q_maxseats), 0, Q_maxseats)]
cd2010 <- merge(cd2010, qdlm, by = c("stcd", "pte"), all.x = TRUE)
cd2010 <- merge(cd2010, qdlmnp, by = c("stcd", "pte"), all.x = TRUE)
cd2010[, Q_dlm := ifelse(is.na(Q_dlm), Q_maxseats, Q_dlm)]
cd2010[, Q_dlmnp := ifelse(is.na(Q_dlmnp), Q_maxseats, Q_dlmnp)]

# add some yearly info
cd2010 <- merge(cd2010, cd[year == 2010, .(stcd, forecast, uncontested, houdvimp)], 
                by = "stcd", all.x = TRUE)
cd2010 <- cd2010[order(stcd, -pte)] # sort

# create variables
cd2010[, ':=' (Qshare_dlm= 100 * Q_dlm / sum(Q_dlm),
               expcumshare = 100 * expcum / sum(expcum)), by = c("pte")]
cd2010[, margin := abs(0.5 - houdvimp)]
cd2010[, margin_dv := abs(0.5 - dv)]
cd2010[, margin_sq := margin^2]
cd2010[, margin_norm := dnorm(margin, 0, .06)]

###
### CORRELATION BETWEEN Q AND SPENDING BY DATE
###
cor.models <- cd2010[, .(qdlm = cor(expcum, Q_dlm),
                         qdlmnp = cor(expcum, Q_dlmnp),
                         qbhm = cor(expcum, Q_maxseats)),
                     by = "pte"]
cor.models <- melt(cor.models, id = "pte", variable.name = "Model")
cor.models[, Model := factor(Model, 
                             levels = c("qdlm", "qdlmnp", "qbhm"),
                             labels = c("DLM", "DLM (no prior)", "Hierarchical"))]
cor.models[, month := (pte -1 )/2]
p.cormodels <- ggplot(cor.models[month > 0 & month < 2.5 ], 
                      aes(x = month, y = value, col = Model)) + geom_point() +
  geom_line() + xlab("Months to election") + ylab("Correlation") + scale_x_reverse()
ggsave("figs/cormodels.pdf", p.cormodels, height = 4, width = 6.5)
