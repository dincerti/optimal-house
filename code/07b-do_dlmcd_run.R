# RUN DISTRICT LEVEL DLM WITH PRIOR -------------------------------------------
# packages doSNOW and parallel are needed for running gibbs sampler
# in parallel
print("Running prior informed DLM")
dlm.sims <- parallelGibbs(prior = TRUE, nsims = 6000)
saveRDS(dlm.sims, file = "output/dlmPosterior.rds")  
