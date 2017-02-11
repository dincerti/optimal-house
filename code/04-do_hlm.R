# BAYESIAN HIERARCHICAL MODEL -------------------------------------------------
###
### ESTIMATING THE MODEL WITH STAN
###
if (!file.exists("output/bhmPosterior.RData")) {
  bhm.sims <- bhm_dat <- list()
  for (i in 1:length(predyears)){
    bhm_dat[[i]] <- HierarchicalData(cd, t = predyears[i])
    bhm <- stan(file = "code/multilevel.stan", data = bhm_dat[[i]], 
                iter = 2000, chains = 5)
    sink(file = paste0("output/bhm", predyears[i], ".txt"))
    print(bhm)
    sink(NULL)
    bhm.sims[[i]] <- extract(bhm, permuted = TRUE)
    save(bhm.sims, bhm_dat, file = "output/bhmPosterior.RData")
  }
} else {
  load("output/bhmPosterior.RData")
}
names(bhm.sims) <- names(bhm_dat) <-  paste0("y", predyears)
n.sims <- length(bhm.sims[[1]]$lp_)

###
### TRACE PLOT AND DENSITY
### 
# rstan::traceplot(bhm, pars = "beta")

###
### TABLE OF POSTERIOR INFERENCES
### 
postsummary <- PostTable(bhm.sims$y2010)
print(xtable(postsummary, digits = 3),
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      file = "tables/postsummary.txt")

###
### DENSITY OF POSTERIOR DISTRIBUTIONS
### 
den.df <- with(bhm.sims$y2010, data.frame(cbind(beta, sigma, sigma_delta)))
names(den.df) <- c("Intercept", xlabs.short, "District error", "National error")
den.df <- melt(den.df)
p.den <- ggplot(den.df, aes(x = value)) + geom_density() +
  facet_wrap(~ variable, scales = "free") + ylab("Density") +
  theme(axis.title.x = element_blank(), text = element_text(size=11)) 
ggsave("figs/postden.pdf", p.den, height = 6, width = 9)

###
### PREDICTED VERSUS ACTUAL PLOT
###
p.pva <- PVA(y = bhm_dat$y2010$y, 
             y_pred = bhm_dat$y2010$x %*% apply(bhm.sims$y2010$beta, 2, mean),
             year = bhm_dat$y2010$actual_year)
ggsave("figs/pva.pdf", p.pva, height = 4, width = 6.5)

###
### FORECASTS 
###
if (!file.exists("output/bhmPred.RData")) {
  bhm.pred <- Forecast(predyears[1])
  for (t in 2:length(predyears)){
    temp <- Forecast(predyears[t])
    bhm.pred$y_new <- rbind(bhm.pred$y_new, temp$y_new)
    bhm.pred$y <- c(bhm.pred$y, temp$y)
    bhm.pred$year <- c(bhm.pred$year, temp$year)
    bhm.pred$stcd <- c(bhm.pred$stcd, temp$stcd)
  }
  save(bhm.pred, file = "output/bhmPred.RData")
} else {
  load("output/bhmPred.RData")
}
bhm.pred.dt <- data.table(stcd = bhm.pred$stcd, year = bhm.pred$year, y_new = bhm.pred$y_new)
uncontested <- cd[uncontested !=0 & year >= min(predyears), .(stcd, year, uncontested)]
uncontested[, y := ifelse(uncontested == 1, 0.75, 0.25)] # impute vote in uncontested districts
uncontested[, repwin := ifelse(uncontested == 1, 0, 1)]

###
### SUMMARY DATA FROM FORECASTS
###
predsummary <- data.table(stcd = bhm.pred$stcd, year = bhm.pred$year,
                          forecast = apply(bhm.pred$y_new, 1, mean),
                          pwin = apply(bhm.pred$y_new >= 0.5, 1, mean),
                          y = bhm.pred$y)
predsummary[, ':=' (fwinner = ifelse(forecast >= 0.5, "D", "R"),
                    winner = ifelse(y >= 0.5, "D", "R"))]

###
### OUT-OF-SAMPLE PREDICTED VERSUS ACTUAL PLOT 
###
p.pvaoos <- PVA(y = predsummary$y, y_pred = predsummary$forecast,
                year = predsummary$year)
ggsave("figs/pvaoos.pdf", p.pvaoos, height = 5, width = 6.5)

###
### RMSFE
###
# district
rmsfe <- predsummary[, .(rmsfe = RMSE(forecast, y)), by = "year"]

# national
natpred <-  predsummary[, .(afdv = mean(forecast), adv = mean(y)), by = "year"]
print(RMSE(natpred$afdv, natpred$adv))

###
### HISTOGRAM OF NUMBER OF SEATS 
###
p.seats <- HistSeats()
ggsave("figs/seats_hist.pdf", p.seats, height = 4, width = 7)

###
### DEVIATION FROM NATIONAL VOTE 
###
center <- function (x) x - mean(x)
year <- 2010
dev <- bhm.pred$y_new[which(bhm.pred$year == year), ]
dev <- apply(dev, 2, center)
dev <- apply(dev, 1, mean)
dev.y <- center(bhm.pred$y[which(bhm.pred$year == year)])
RMSE(dev, dev.y)