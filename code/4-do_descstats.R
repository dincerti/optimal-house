# DESCRIPTIVE STATISTICS ------------------------------------------------------
###
### SUMMARY STATISTICS FOR FORECASTING VARIABLES 
###
sumstats <- matrix(NA, nrow = length(xvars), ncol = 3)
sumstats[, 1] <- as.numeric(cd[, lapply(.SD, min, na.rm = TRUE), .SDcols = c(xvars)])
sumstats[, 2] <- as.numeric(cd[, lapply(.SD, quantile, prob = 0.5, na.rm = TRUE), .SDcols = c(xvars)])
sumstats[, 3] <- as.numeric(cd[, lapply(.SD, max, na.rm = TRUE), .SDcols = c(xvars)])
rownames(sumstats) <- xlabs
print(xtable(sumstats, digits = 3), 
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL, file = "tables/sumstats.txt")

###
### SPENDING BY TYPE AND CYCLE
###
exp.year <- pacs[, .(sum = sum(rAmount, na.rm = TRUE)), by = .(DI, Cycle)]
exp.year[, DI := factor(DI, levels = c("D", "I"), labels = c("Direct", "Indirect"))]
exp.year.plot <- ggplot(exp.year, aes(x = Cycle, y = sum, col = DI)) + 
  geom_point() + geom_line() + 
  xlab("Election cycle") + ylab("Total expenditures") +
  scale_colour_discrete(name="Type") +
  scale_x_continuous(breaks = pretty_breaks()) 
ggsave("figs/spending.pdf", exp.year.plot, height = 5, width = 6.5)

###
### SPENDING BY MONTH 
###
pacs[, monthdays := ifelse(month(Date) != 11, monthDays(Date), mday(eldate))]
exp.month <- pacs[year >= min(Cycle) & month(Date) < 12 & yday(Date) <= yday(eldate), 
                  .(exp = sum(Amount) / monthdays), by = .(month(Date), year)]
exp.month <- exp.month[, .(exp = mean(exp)/1000000), by = .(month)]
exp.month[, month := as.Date(paste(month,"/01/2010",sep=""),  format = "%m/%d/%Y")]
p.exp.month <- ggplot(exp.month, aes(x = month, y = exp)) + 
  geom_line() + geom_point() + xlab("Month") + 
  ylab("Expenditures (millions of $2010)") + scale_x_date(breaks = pretty_breaks(10))
ggsave("figs/monthly_spending.pdf", p.exp.month, height = 4, width = 6)

###
### POST AUGUST SPENDING BY PARTY TYPE 
###
exp.dem <- t(cd[year >= 2000, lapply(.SD, mean, na.rm = TRUE), 
                .SDcols = c("expD_demorg", "expD_demcon", "expD_demally","expD_other")])
exp.rep <- t(cd[year >= 2000, lapply(.SD, mean, na.rm = TRUE), 
                .SDcols = c("expR_reporg", "expR_repcon", "expR_repally","expR_other")])
exp.party <- data.frame(dem_sum = exp.dem, dem_frac =  exp.dem/sum(exp.dem),
                        rep_sum = exp.rep, rep_frac = exp.rep/sum(exp.rep))
exp.party <- round(exp.party, 3)
exp.party[, 1] <- prettyNum(exp.party[, 1], big.mark=",", scientific = F)
exp.party[, 3] <- prettyNum(exp.party[, 3], big.mark=",", scientific = F)
rownames(exp.party) <- c("National party committees", "Party-connected committees", "Allied PACs",
                         "Other PACs")
print(xtable(exp.party), 
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL, file = "tables/partyexp.txt")

###
### POLLING DATA 
###
# national
gbcount <- gb[, .N, by = poll]
gbcount <- gbcount[order(-N)]

# district
pollcount <- polls[pollyear == 2010, .(polls = sum(nmis)), by = c("stcd")]
pollcount <- merge(cd[year == 2010, .(stcd, houdv)], pollcount, by = "stcd", all.x = TRUE)
pollcount[is.na(polls), polls := 0]
p.pollbyvote <- ggplot(pollcount, aes(x = houdv, y = polls)) + geom_point() + 
  geom_vline(xintercept = 0.5, linetype = "longdash") + 
  xlab("Democratic vote share") + ylab("Number of polls per district") + labs(title = "(a)\n")
ggsave("figs/pollbyvote.pdf", p.pollbyvote, height = 4, width = 4)

pollcumsum <- polls[year(polls$date_mid) == 2010, .(sum = sum(nmis)), by = "date_mid"]
pollcumsum <- pollcumsum[order(date_mid)]
pollcumsum[, cumsum := cumsum(sum)]
p.pollcumtot <- ggplot(pollcumsum, aes(x = date_mid, y = cumsum)) + geom_line() +
  xlab("Month") + ylab("Cumulative number of polls") + labs(title = "(b)\n")
ggsave("figs/pollcumstot.pdf", p.pollcumtot, height = 3, width = 5)

# print as one plot
pdf("figs/polldescstats.pdf", height = 5, width = 5)
grid.arrange(p.pollbyvote, p.pollcumtot, ncol=1)
dev.off()

###
### OLS REGRESSION
###
# similar to hierarchical
ols <- lm(as.formula(paste(yvar, "~", paste(xvars, collapse = "+"))),
          data = cd)

# deviations
cd[, houdv_avg := mean(houdv, na.rm= TRUE), by = "year"]
cd[, l_houdvimp_avg := mean(l_houdvimp, na.rm= TRUE), by = "year"]
cd[, ':=' (houdv_rel = houdv - houdv_avg, l_houdvimp_rel = l_houdvimp - l_houdvimp_avg)]
ols.dev <- lm(houdv ~ l_houdvimp + houinc + dwnom1 + fresh + i_po + prescontrolXaugapr + 
                gbdv_aug + midterm, data = cd)