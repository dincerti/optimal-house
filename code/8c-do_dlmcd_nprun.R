# RUN DISTRICT LEVEL DLM WITH PRIOR -------------------------------------------
# packages doSNOW and parallel are needed for running gibbs sampler
# in parallel
dlm.sims <- parallelGibbs(prior = FALSE, nsims = 6000)
save(dlm.sims, file = "output/dlmnpPosterior.Rdata")  
