# must set fpte which is the number of periods before the election that the
# forecast is being made on (i.e. pte >= 3)

# DATA
dlmNatDat <- function(x, fpte){
  # means by window and poll
  xmean <- x[,.(dv = weighted.mean(dv, sample_size), sample_size = sum(sample_size)),
             by = c("poll", "pte")] 
  xmean <- xmean[pte >= fpte]
  xmean[, var := dv * (1 - dv)/sample_size]
  xmean[, ':=' (dv = 100 * dv, var = 100^2 * var)]
  
  # missing observations
  xmean[, nmis := ifelse(is.na(dv), 0, 1)]
  xmean[, pollcount := sum(nmis), by = "poll"]
  xmean <- xmean[pollcount !=0] # exclude pollsters with no polls
  
  # data matrices
  pte <- data.frame(pte = seq(max(x$pte), 1))
  y <- dcast.data.table(xmean, pte ~ poll, value.var = "dv")
  y <- merge(pte, y, by = "pte", all.x = TRUE)
  y <- y[order(-y$pte), ]
  v <- dcast.data.table(xmean, pte ~ poll, value.var = "var")
  v <- merge(pte, v, by = "pte", all.x = TRUE)
  v <- v[order(-v$pte), ]
  dlmnatdat <- list(y = as.matrix(y[, -1]), v = as.matrix(v[, -1]))
}

# GIBBS SAMPLER
gibbsNat <- function(fpte, n, finpoll_mean, finpoll_sd){
  dlmdat <- dlmNatDat(gb, fpte = fpte)
  list2env(dlmdat, globalenv())
  y <- rbind(y, NA)
  y <- cbind(y, c(rep(NA, nrow(y) -1), finpoll_mean * 100))
  v <- rbind(v, NA)
  v <- cbind(v, c(rep(NA, nrow(v) -1), (finpoll_sd * 100)^2))
  v.complete <- v 
  v.complete[is.na(v.complete)] <- 1
  m <- ncol(y); T <- nrow(y)  # m = polls, T = time periods
  
  # model
  mod <- dlm(m0 = 50, C0 = 16 ,
             FF = rep(1, m), V = diag(.5^2/1000, m), JV = diag(seq(1, m)),
             GG = 1, W = 1,
             X = v.complete)
  
  # prior hyperparameters
  beta <- 50
  alpha <- (beta /25) + 1
  sqrt(beta/(alpha - 1)) #mean of standard deviation
  beta^2/((alpha - 1)^2 * (alpha - 2)) # variance
  den <- data.frame(x = rinvgamma(1000, shape = alpha , scale = beta))
  #ggplot(den, aes(x = x)) + geom_histogram(color = "black", fill = "white") + xlim(0, 81)
  
  # mcmc set up
  iter <- n
  gibbsTheta <- matrix(NA, nrow = T + 1, ncol = iter)
  rownames(gibbsTheta) <- paste0("theta", seq(nrow(gibbsTheta) - 1, 0))
  gibbsPsi <- rep(NA, iter)
  
  # starting values
  psi.init <- rinvgamma(1, shape = alpha , scale = beta)
  mod$W <- psi.init
  sigma.lambda <- 5
  lambda <- c(rnorm(m-1, 0, 5), 0) # prior is assumed to have 0 house effect
  gibbsLambda <- matrix(NA, nrow = iter, ncol = m- 1)
  colnames(gibbsLambda) <- colnames(y)[-m]
  
  # gibbs sampler
  ptm <- proc.time()
  for (i in 1:iter){
    # FFBS
    yadj <-  sweep(y, 2, lambda)
    modFilt <- dlmFilter(yadj, mod, debug = FALSE)
    theta <- dlmBSample(modFilt)
    gibbsTheta[, i] <- theta
    
    # update variance matrix W
    theta_t <- theta[-1]
    theta_lt <- theta[-(T + 1)]
    SStheta <- sum((theta_t - theta_lt)^2)
    psi <- rinvgamma(1, shape = alpha + T/2,
                       scale = beta + SStheta/2)
    gibbsPsi[i] <- psi
    mod$W <- psi
    
    # update lambda
    lambda.var <- 1/(apply(1/v[, -m], 2, sum, na.rm = TRUE) + (1 / sigma.lambda^2))
    lambda.mean <- apply((y[, -m] - theta[-1])/v[, -m], 2, sum, na.rm = TRUE) * lambda.var
    lambda <- c(rnorm(m -1, lambda.mean, lambda.var), 0)
    lambda <- lambda - mean(lambda)
    gibbsLambda[i, ] <- lambda[-m]
    print(i)
  }
  proc.time() - ptm
  
  # summarize results
  gibbsTheta <- gibbsTheta/100
  gibbsPsi <- gibbsPsi/ 100^2
  gibbsLambda <- gibbsLambda/100
  gibbs <- list(theta = gibbsTheta, psi = gibbsPsi, lambda = gibbsLambda)
  return(gibbs)  
}



