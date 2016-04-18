# data
dlmDat <- function(x, fpte){
  x[, year := year(x$date_mid)] 
  x <- x[year == 2010]
  x[, rdv := dv - get(paste0("theta", fpte))]
  
  # means by stcd and window
  xmean <- x[,.(dv = mean(dv), rdv = mean(rdv), sample_size = sum(sample_size), forecast = mean(rforecast_mean)),
                  by = c("stcd", "pte")] 
  xmean <- xmean[pte >= fpte]
  xmean[, var := dv * (1 - dv)/sample_size]
  xmean[, ':=' (rdv = 100 * rdv, forecast = 100 * forecast, var = 100^2 * var)]
  
  # data matrices
  pte <- data.frame(pte = seq(max(x$pte), 1))
  y <- dcast.data.table(xmean, pte ~ stcd, value.var = "rdv")
  y <- merge(pte, y, by = "pte", all.x = TRUE)
  y <- y[order(-y$pte), ]
  v <- dcast.data.table(xmean, pte ~ stcd, value.var = "var")
  v <- merge(pte, v, by = "pte", all.x = TRUE)
  v <- v[order(-v$pte), ]  
  regy <- xmean[, .(forecast = mean(forecast)), by = "stcd"]  
  regy <- regy[order(stcd)]
  regy.var <- (mean(x$rforecast_sd) * 100)^2
  dlmdat <- list(y = as.matrix(y[, -1]), v = as.matrix(v[, -1]),
                 regy = regy[, forecast], regy_var = regy.var)
}

gibbs <- function(fpte, iter, prior = TRUE, data = polls){
  dlmdat <- dlmDat(data, fpte = fpte)
  list2env(dlmdat, globalenv())
  if(prior == TRUE){
    y <- rbind(y, regy) 
    v <- rbind(v, regy_var)    
  } else{
    y <- rbind(y, NA) 
    v <- rbind(v, NA) 
  }
  v[is.na(v)] <- 50
  m <- ncol(y); T <- nrow(y); p <- m  # m = districts, p = states, T = time periods
  
  # model
  mod <- dlm(m0 = regy, C0 = diag(regy_var, p),
             FF = diag(m), V = diag(.5^2/1000, m), JV = diag(seq(1, m)),
             GG = diag(p), W = diag(.05, p),
             X = v)
  
  # prior hyperparameters
  beta <- 50
  alpha <- (beta /25) + 1
  sqrt(beta/(alpha - 1)) #mean of standard deviation
  beta^2/((alpha - 1)^2 * (alpha - 2)) # variance
  den <- data.frame(x = rinvgamma(1000, shape = alpha , scale = beta))
  #ggplot(den, aes(x = x)) + geom_histogram(color = "black", fill = "white") + xlim(0, 81)
  
  # mcmc set up
  gibbsTheta <- array(NA, dim = c(iter, T + 1, m))
  dimnames(gibbsTheta)[[2]] <- paste0("theta", seq(dim(gibbsTheta)[2] - 1, 0))
  dimnames(gibbsTheta)[[3]] <- colnames(y)
  gibbsPsi <- rep(NA, iter)
  
  # starting values
  psi.init <- rinvgamma(1, shape = alpha , scale = beta)
  mod$W <- diag(psi.init, p)
  
  # gibbs sampler
  for (i in 1:iter){
    # FFBS
    try(modFilt <- dlmFilter(y, mod, debug = FALSE))
    theta <- dlmBSample(modFilt)
    gibbsTheta[i, , ] <- theta
  
    # update variance matrix W
    theta_t <- t(theta[-1, ])
    theta_lt <- mod$G %*% t(theta[-(T + 1), ])
    SStheta <- sum((theta_t - theta_lt)^2)
    psi <- rinvgamma(1, shape = alpha + (T * m)/2,
                          scale = beta + SStheta/2)
    gibbsPsi[i] <- psi
    mod$W <- diag(psi, p)
    print(paste0("fpte = ", fpte, "; iteration = ", i))
  }
  gibbsTheta <- gibbsTheta/100
  gibbsPsi <- gibbsPsi/ 100^2
  gibbs <- list(theta = gibbsTheta, psi = gibbsPsi)
  #saveRDS(gibbs, file = paste0("dlmPosterior", fpte,".rds"))
  return(gibbs)
} 

parallelGibbs <- function(prior = TRUE, nsims = 6000, code = "code/dlm.R",
                          packages = c("dlm", "data.table", "MCMCpack"),
                          datafile = "output/dlmdata.rds"){
  if (prior == TRUE){
    name <- "dlm"
  } else {
    name <- "dlmnp"
  }
  cl <- makeCluster(4, outfile = paste0("output/", name, "Progress.txt")) # iteration progress can be checked in file "output/dlmProgress.txt"
  registerDoSNOW(cl)
  tmp.gibbs <- function(i){
    source(code)
    lapply(packages, library, character.only = TRUE)
    polls <- readRDS(datafile)
    return(gibbs(i, nsims, prior, polls)) # set prior = TRUE or prior = FALSE
  } 
  ptm <- proc.time()
  dlm.sims <- clusterApply(cl, 1:4, tmp.gibbs)
  proc.time() - ptm
  stopCluster(cl)
  return(dlm.sims)
}


