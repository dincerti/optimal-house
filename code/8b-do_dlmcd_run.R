# RUN DISTRICT LEVEL DLM WITH PRIOR -------------------------------------------
# packages doSNOW and parallel are needed for running gibbs sampler
# in parallel
dlm.sims <- parallelGibbs(prior = TRUE, nsims = 2)
save(dlm.sims, file = "output/dlmPosterior.Rdata")  
