# FORECASTS FROM MODELS  ------------------------------------------------------
###
### FORECASTS
###
bhm.cpred2010 <- bhm.pred.dt[year == 2010]
for(j in 3:ncol(bhm.cpred2010)) set(bhm.cpred2010, , j, center(bhm.cpred2010[[j]]))

# forecast with priors
dlm.pred <- dlmForecast(dlmnat.sims, dlm.sims, bhm.cpred2010)

# forecast with no priors
dlmnp.pred <- dlmForecast(dlmnatnp.sims, dlmnp.sims, bhm.cpred2010)

###
### UPDATE SUMMARY DATA FROM FORECASTS
###
dlm.predsummary <- dlmnp.predsummary <- list()
for (i in 1:length(dlm.sims)){
  dlm.predsummary[[i]] <- data.table(forecast = apply(dlm.pred$overall[[i]], 2, mean), 
                                     pwin = apply(dlm.pred$overall[[i]] >= 0.5, 2, mean),
                                     stcd = names(apply(dlm.pred$overall[[i]], 2, mean)))
  dlmnp.predsummary[[i]] <- data.table(forecast = apply(dlmnp.pred$overall[[i]], 2, mean),
                                       pwin = apply(dlmnp.pred$overall[[i]] >= 0.5, 2, mean),
                                       stcd = names(apply(dlmnp.pred$overall[[i]], 2, mean)))
  setnames(dlm.predsummary[[i]], c("forecast", "pwin"), c(paste0("forecast", i), paste0("pwin", i)))
  setnames(dlmnp.predsummary[[i]], c("forecast", "pwin"), c(paste0("forecast_np", i), paste0("pwin_np", i)))
}
predsummary2010 <- Reduce(function (x, y) merge(x, y, by = c("stcd"), all.x = TRUE), 
                          c(list(predsummary2010), dlm.predsummary, dlmnp.predsummary))
predsummary2010[, d_winner := ifelse(winner == "D", 1, 0)]

###
### PLOT OF FORECASTED NATIONAL VOTE
###
natpred2010 <- data.table(pte = rep(seq_along(dlmnat.sims), 2),
                          model = c(rep("Prior", 4), rep("No prior", 4)))
for (i in seq_along(dlmnat.sims)){
  natpred2010[pte == i & model == "Prior", c("q025", "median", "q975") := 
                as.list(quantile(dlm.pred$nat[[i]], probs = c(0.025, 0.5, 0.975)))]
  natpred2010[pte == i & model == "No prior", c("q025", "median", "q975") := 
                as.list(quantile(dlmnp.pred$nat[[i]], probs = c(0.025, 0.5, 0.975)))]
}

natpred2010[, month := (pte -1)/2]
pollavg <- gb[pte <= 4, .(pollavg = mean(dv)), by = "pte"]
pollavg[, month := (pte -1)/2]

p.dlmnatpred <- ggplot(natpred2010, aes(x = month, y = median)) + 
  geom_point() + geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymin = q025, ymax = q975), width = .1) +
  facet_wrap(~ model, ncol = 1) +
  xlab("Months to election") + ylab("Democratic fraction of the vote") +
  geom_line(data = pollavg, aes(x = month, y = pollavg), linetype = "dotted", col = "gray28") +
  geom_hline(yintercept = afdv2010.mean, linetype = "dotted", col = "gray28") +
  geom_hline(yintercept = adv2010, linetype = "dotted", col = "gray28") +
  annotate("text", x = 0.75, y = 0.52, label = "Regression forecast", size = 3) +
  annotate("text", x = 0.75, y = 0.49, label = "Actual vote", size = 3) +
  annotate("text", x = 0.75, y = 0.458, label = "Poll average", size = 3)+
  scale_x_reverse() 
print(natopinion[pte == 1, theta1])
ggsave("figs/dlmnatpred.pdf", p.dlmnatpred, height = 5.5, width = 5.5)

###
### PLOT OF FORECASTED PERCENT OF DEMOCRATIC SEATS
###
dlmseats <- data.table(pte = seq_along(dlm.sims),
                       month = NA,
                       model = c(rep("Prior", 4), rep("No prior", 4)))
dlmseats[, month := (pte - 1)/2] 
for (i in seq_along(dlm.sims)){
  tmp.seats <- apply(dlm.pred$overall[[i]], 1, function (x) mean(x > 0.5))
  tmp.npseats <- apply(dlmnp.pred$overall[[i]], 1, function (x) mean(x > 0.5))
  dlmseats[pte == i & model == "Prior", c("q025", "median", "q975") := 
             as.list(quantile(tmp.seats, probs = c(0.025, 0.5, 0.975)))]
  dlmseats[pte == i & model == "No prior", c("q025", "median", "q975") := 
             as.list(quantile(tmp.npseats, probs = c(0.025, 0.5, 0.975)))]
}
bhm.medpredseats <- median(apply(bhm.pred$y_new[which(bhm.pred$year == 2010), ],
                                 2, function(x) mean (x > 0.5)))
p.dlmseats <- ggplot(dlmseats, aes(x = month, y = median)) + geom_point() +
  geom_line(linetype = "dashed") + 
  facet_wrap(~ model, ncol = 1) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = .1) +
  xlab("Months to election") + ylab("Democratic fraction of seats") +
  geom_hline(yintercept = bhm.medpredseats, linetype = "dotted", col = "gray28") +
  geom_hline(yintercept = mean(predsummary2010$winner == "D"), linetype = "dotted", col = "gray28") +
  annotate("text", x = 1.25, y = bhm.medpredseats*1.02, label = "Regression forecast", size = 3) +
  annotate("text", x = 1.25, y = 0.475, label = "Actual seats", size = 3) +
  scale_x_reverse() 
ggsave("figs/dlmseats.pdf", p.dlmseats, height = 5.5, width = 5.5)

###
### FORECAST EVALUATION
###
# RMSE
rmsfe2010 <- ForecastEvalTable(predsummary2010, "y", 
                               c(paste0("forecast", seq(1,4)), paste0("forecast_np", seq(1,4))),
                               FUN = RMSE)
tmp <- data.table(Model = "bhm", value = rmsfe[year == 2010, rmsfe],  pte = seq(1, 4))
rmsfe2010 <- rbind(rmsfe2010, tmp)
rmsfe2010[, month := (pte -1 ) /2]
rmsfe2010[, Model := factor(Model, levels = c("bhm", "forecast", "forecast_np"),
                            labels = c("Hierarchical", "DLM", "DLM (no prior)"))]
p.rmsfe2010 <- ggplot(rmsfe2010, aes(x = month, y = value, col = Model)) +
  geom_point() + geom_line() + ggtitle("(a)\n") +
  xlab("Months to election") + ylab("RMSFE") +
  scale_x_reverse() + theme(legend.position="bottom")

# BRIER SCORES
brier2010 <- ForecastEvalTable(predsummary2010, "d_winner", 
                               c(paste0("pwin", seq(1,4)), paste0("pwin_np", seq(1,4))),
                               FUN = Brier)
tmp <- data.table(Model = "bhm",
                  value = Brier(predsummary2010$pwin, predsummary2010$d_winner),
                  pte = seq(1, 4))
brier2010 <- rbind(brier2010, tmp)
brier2010[, month := (pte -1) /2]
brier2010[, Model := factor(Model, levels = c("bhm", "pwin", "pwin_np"),
                            labels = c("Hierarchical", "DLM", "DLM (no prior)"))]
p.brier2010 <- ggplot(brier2010, aes(x = month, y = value, col = Model)) +
  geom_point() + geom_line() +  ggtitle("(b)\n") +
  xlab("Months to election") + ylab("Brier score") +
  scale_x_reverse() + theme(legend.position="none")

# print as one plot
mylegend <- g_legend(p.rmsfe2010) 
pdf("figs/forecast_eval_2010.pdf", height = 3.75, width = 6)
grid.arrange(arrangeGrob(p.rmsfe2010 + theme(legend.position = "none"),
                         p.brier2010,
                         nrow=1),
             mylegend, nrow=2, heights=c(10, 1))
dev.off()

###
### FORECAST FOR DISTRICTS WITH AND WITHOUT POLLING DATA
###
# RMSFE
rmsfe2010.polls <- ForecastEvalTableByPoll("y", c(paste0("forecast", seq(1,4)), 
                                                  paste0("forecast_np", seq(1,4))),
                                           FUN = RMSE)
rmsfe2010.polls[, Model := factor(Model, levels = c("forecast", "forecast_np"),
                                  labels = c("DLM", "DLM (no prior)"))] 
p.rmsfe2010.polls <- ggplot(rmsfe2010.polls, aes(x = month, y = value, col = Model)) +
  geom_point() + geom_line() + facet_wrap(~type) +
  xlab("Months to election") + ylab("RMSFE") +
  scale_x_reverse() + theme(legend.position="bottom")
ggsave("figs/rmsfe2010_bypollfreq.pdf", p.rmsfe2010.polls, height = 3.5, width = 5.5)

# MPE
mpe2010.polls <- ForecastEvalTableByPoll("y", c(paste0("forecast", seq(1,4)), 
                                                paste0("forecast_np", seq(1,4))),
                                         FUN = MPE)
mpe2010.polls[, Model := factor(Model, levels = c("forecast", "forecast_np"),
                                labels = c("DLM", "DLM (no prior)"))]
p.mpe2010.polls <- ggplot(mpe2010.polls, aes(x = month, y = value, col = Model)) +
  geom_point() + geom_line() + facet_wrap(~type) +
  xlab("Months to election") + ylab("MPE") +
  scale_x_reverse() + theme(legend.position="bottom")
ggsave("figs/mpe2010_bypollfreq.pdf", p.mpe2010.polls, height = 3.5, width = 5.5)

# BRIER SCORE
brier2010.polls <- ForecastEvalTableByPoll("d_winner", 
                                           c(paste0("pwin", seq(1,4)), paste0("pwin_np", seq(1,4))),
                                           FUN = Brier)
brier2010.polls[, Model := factor(Model, levels = c("pwin", "pwin_np"),
                                  labels = c("DLM", "DLM (no prior)"))]
p.brier2010.polls <- ggplot(brier2010.polls, aes(x = month, y = value, col = Model)) +
  geom_point() + geom_line() + facet_wrap(~type, scale = "free") +
  xlab("Months to election") + ylab("Brier score") +
  scale_x_reverse() + theme(legend.position="bottom")
ggsave("figs/brier2010_bypollfreq.pdf", p.brier2010.polls, height = 3.5, width = 5.5)
