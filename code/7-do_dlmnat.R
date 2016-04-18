# NATIONAL DLM MODEL  ---------------------------------------------------------
###
### GIBBS SAMPLER FOR NATIONAL MODEL
###
# prior: forecast of average district vote including imputed uncontested as known
afdv2010 <- apply(ynew2010.full, 2, mean) 
afdv2010.mean <- mean(afdv2010)
afdv2010.sd <- sd(afdv2010)
adv2010 <- mean(c(predsummary[year == 2010, y], uncontested[year == 2010, y]))

# run gibbs sampler with prior
if (file.exists("output/dlmnatPosterior.RData") == FALSE) {
  dlmnat.sims <- list()
  for (i in seq(1, 4)){
    dlmnat.sims[[i]] <- gibbsNat(fpte = i, n = 6000, 
                                 finpoll_mean = afdv2010.mean,
                                 finpoll_sd = afdv2010.sd)  
  }
  save(dlmnat.sims, file = "output/dlmnatPosterior.RData")
} else {
  load("output/dlmnatPosterior.RData")
}

# run gibbs sampler with no prior
if (file.exists("output/dlmnatnpPosterior.RData") == FALSE) {
  dlmnatnp.sims <- list()
  for (i in seq(1, 4)){
    dlmnatnp.sims[[i]] <- gibbsNat(fpte = i, n = 6000, 
                                   finpoll_mean = NA,
                                   finpoll_sd = NA)  
  }
  save(dlmnatnp.sims, file = "output/dlmnatnpPosterior.RData")
} else {
  load("output/dlmnatnpPosterior.RData")
}

# diagnostic plots
diagplotNat(dlmnat.sims, "dlmnat_diagnostics")

# burn in
dlmnat.sims <- natBurnin(dlmnat.sims, 1000)
dlmnatnp.sims <-  natBurnin(dlmnatnp.sims, 1000)

# calculate national opinion during campaign
natopinion <- natOpinion(dlmnat.sims)
natopinion.np <- natOpinion(dlmnatnp.sims)

###
### PLOT OF NATIONAL OPINION
###
natopinion1 <- natopinion[, .(pte, theta1)]
natopinion1[, month := pte/2]
tmp <- apply(dlmnat.sims[[1]]$theta, 1, function (x) quantile(x, probs = c(0.025, 0.975)))
tmp <- t(tmp[, -c(1, ncol(tmp))])
colnames(tmp) <- c("q025", "q975")
natopinion1 <- cbind(natopinion1, tmp)
p.natopinion1 <- ggplot(natopinion1, aes(x = month, y = theta1)) +
  geom_point() + geom_line() + 
  #geom_errorbar(aes(ymin = q025, ymax = q975), width = .1) +
  xlab("Months to election") + ylab("Democratic fraction of the vote") +
  scale_x_reverse() 
ggsave("figs/natopinion1.pdf", p.natopinion1, height = 4, width = 6.5)

###
### PLOT OF HOUSE EFFECTS
###
houeffects <- data.table(pollster = colnames(dlmnat.sims[[1]]$lambda),
                         mean = 100 * apply(dlmnat.sims[[1]]$lambda, 2, mean),
                         q025 = 100 * apply(dlmnat.sims[[1]]$lambda, 2, 
                                            function (x) quantile(x, .025)),
                         q975 = 100 * apply(dlmnat.sims[[1]]$lambda, 2, 
                                            function (x) quantile(x, .975)))
houeffects <- houeffects[order(-mean)]
houeffects[, id := seq(1, nrow(houeffects))]
houeffects[, id := factor(id, labels = pollster)]
p.houeffects <- ggplot(houeffects, aes(x = id, y = mean,
                                       ymin = q025, ymax = q975)) + geom_pointrange() +
  coord_flip() + geom_hline(aes(yintercept=0), lty=2)+ xlab("") + ylab("Bias")
ggsave("figs/houeffects.pdf", p.houeffects, height = 7, width = 7)
