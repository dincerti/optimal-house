# RUN DISTRICT LEVEL DLM WITH PRIOR -------------------------------------------
# packages doSNOW and parallel are needed for running gibbs sampler
# in parallel
print("Running no prior DLM")
dlmnp.sims <- parallelGibbs(prior = FALSE, nsims = 6000)
saveRDS(dlmnp.sims, file = "output/dlmnpPosterior.rds")  
