# RESULTS FOR DLM GIBBS SAMPLER  ----------------------------------------------
# loading
dlm.sims <- readRDS("output/dlmPosterior.rds")
dlmnp.sims <- readRDS("output/dlmnpPosterior.rds")

# diagnostic plots
if (!file.exists("figs/dlm_diagnostics1.pdf")){
  diagplotRel(dlm.sims, "dlm_diagnostics")
}

# burn in
dlm.sims <- relBurnin(dlm.sims, 1000)
dlmnp.sims <- relBurnin(dlmnp.sims, 1000)

###
### POSTERIOR QUANTILES FOR SQUARE ROOT OF VARIANCE PARAMETERS
###
quantPsi <- function(t, x){
  quantile((sqrt(x[[t]]$psi)), c(.025, .5, .975))
}
postpsi <- matrix(c(quantPsi(1, dlmnat.sims), quantPsi(1, dlm.sims),
                    quantPsi(2, dlmnat.sims), quantPsi(2, dlm.sims),
                    quantPsi(3, dlmnat.sims), quantPsi(3, dlm.sims),
                    quantPsi(4, dlmnat.sims), quantPsi(4, dlm.sims)
), ncol = 6, byrow = TRUE)                  
postpsi <- data.frame(pte = seq_along(dlm.sims)/2 - 0.5, postpsi)   
print(xtable(postpsi, digits = c(1, 1, rep(3, 6))), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      file = "tables/postpsi.txt")

