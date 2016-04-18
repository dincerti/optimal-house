
# IS NUMBER EVEN? ---------------------------------------------------------------
is.even <- function(x) x %% 2 == 0

# CENTER VECTOR/MATRIX ----------------------------------------------------------
center <- function (x) x - mean(x)

# ROOT MEAN SQUARE ERROR --------------------------------------------------------
RMSE <- function(x, y)  sqrt(mean((y - x)^2))

# MEAN PREDICTION ERROR ---------------------------------------------------------
MPE <- function(x, y)  mean((x - y))

# BRIER SCORE -------------------------------------------------------------------
Brier <- function(x, y) mean((x - y)^2)

# CONVERT NUMERIC MONTH TO ABBREVIATION -----------------------------------------
ConvertMonths <- function (x){
  tolower(month.abb[as.numeric(names(x)[-1])])
}

# NA TO 0 -----------------------------------------------------------------------
NAToZero <- function(x, colnames){
  for (j in colnames){
    set(x, which(is.na(x[[j]])), j, 0)
  }
}

# CONGRESSIONaL COMMITTEE TOTALS BY CONGRESSMAN-SESSION -------------------------
ccmtesID <- function(x){
  ccode <- dcast.data.table(x, year + id ~ ccode, value.var = "value")
  for (j in colnames(ccode)[-c(1:2)]){ 
    set(ccode, which(ccode[[j]] > 1), j, 1)
  } # some members are on the same committee more than once in the same congress
  ccode$numcc <- rowSums(ccode[, -c(1:2), with = FALSE])
  y <- x[, lapply(.SD, sum), by = .(year, id), .SDcols = c("chair", "finchair")]
  y[chair > 1, chair:= 1] # dummy variable for chair of at least one committeee
  y <- merge(y, ccode, by = c("year", "id"))
  return(y)
}

# CREATE CRP VARIABLES ----------------------------------------------------------
CRPVars <- function(x){
  x[, Date := as.Date(Date, format = "%Y-%m-%d")]
  x[, ':=' (year = year(Date))]
  x <- merge(x, cpi, by = "year", all.x = TRUE)
  x[, rAmount := Amount * (218.056 / cpi)] 
  x <- merge(x, elyear[, .(year, eldate)], by = "year", all.x = TRUE)
  x <- x[year >= min(x$Cycle)]
  x <- merge(x, ucands, by = c("CID", "Cycle"))
  x[, Cycle := as.integer(Cycle)]
  x <- merge(x, industry, by = "RealCode", all.x = TRUE)
  return(x)
}

# AVERAGE GALLUP LOW AND HIGH TURNOUT MODELS ------------------------------------
GallupEdit <- function(x){
  gallup.low <- x[poll == "Gallup (LV Lower Turnout)*"]
  gallup.high <- x[poll == "Gallup (LV Higher Turnout)*"]
  setnames(gallup.high, c("dv"), c("dv_high"))
  gallup <- cbind(gallup.low, gallup.high[, .(dv_high)])
  gallup[, dv := (dv + dv_high)/2]
  gallup[, ':=' (poll = "Gallup", dv_high = NULL)]
  x <- x[!poll %in% c("Gallup (LV Lower Turnout)*", "Gallup (LV Higher Turnout)*")]
  x <- rbind(x, gallup)
  x <- x[order(-date_mid)]
  return(x)
}

# SUM OF EXPENDITURES BY PAC PARTY TYPE -----------------------------------------
PartyExp <- function(p){
  x <- pacs[month(Date) >= 9 & yday(Date) <= yday(eldate) & party == p, 
                  .(exp = sum(rAmount)), by = c("year", "stcd", "pactype")]
  x <- dcast.data.table(x, year + stcd ~ pactype, 
                              sum, na.rm = TRUE, value.var = "exp")
  setnames(x, names(x)[-c(1:2)],
           paste0("exp", p, "_", names(x)[-c(1:2)])) 
  return(x)
}


# SUM OF EXPENDITURES BY GROUPS -------------------------------------------------
PostAugExp <- function(x, newvar){
  x <- x[month(Date) >= 9 & yday(Date) <= yday(eldate), 
         .(exp = sum(rAmount)), by = c("year", "stcd")]
  setnames(x, c("exp"), newvar)
  return(x)
}

# INDEX VARIABLE ----------------------------------------------------------------
Index <- function(x){
  # Args:
  #   x: group variable with length equal to number of observations
  #
  # Returns:
  #   x indexed from 1 to J where J is the number of groups
  uniq <- unique(x)
  J <- length(uniq)
  var <- rep (NA, J)
  for (i in 1:J){
    var[x==uniq[i]] <- i
  }
  return(var)
}

# DATA FOR STAN HIERARCHICAL MODEL ----------------------------------------------
HierarchicalData <- function(x, t){
  mod.df <- subset(x, year < t,  select = c(yvar, xvars, "int", "year", "stcd"))
  mod.df <- mod.df[complete.cases(mod.df), ]
  mod.x <-  as.matrix(mod.df[, c("int", xvars), with = FALSE])
  mod.y <- unlist(mod.df[, yvar, with = FALSE])
  standata <- list(N = length(mod.y) , K = ncol(mod.x), T = length(unique(mod.df$year)),
                      year = Index(mod.df$year), x = mod.x , y = mod.y,
                   actual_year = mod.df$year, stcd = mod.df$stcd)
  return(standata)
}

# PREDICTED VERSUS ACTUAL PLOT --------------------------------------------------
PVA <- function(y, y_pred, year){
  dat <- data.frame(y = y, y_pred = y_pred,
                       year = year)
  pva.plot <- ggplot(dat, aes(x = y, y = y_pred)) + geom_point() + 
    geom_abline(intercept = 0, slope = 1, col = "blue", size = 0.5) + 
    xlab("Actual Democratic vote") + ylab("Predicted Democratic vote") +
    facet_wrap(~year)
}

# SUMMARY FROM POSTERIOR DISTRIBUTION -------------------------------------------
PostSummary <- function(x){
  # Returns mean, 2.5%, median and 97.5% quantile
  #
  # Args:
  #   x: matrix with rows as simulations and columns as variaables
  #
  # Returns:
  #   matrix of results 
  est <- matrix(NA, ncol = 4, nrow = ncol(x))
  est[, 1] <- apply(x, 2, mean)
  est[, 2] <- apply(x, 2, quantile, probs = .025)
  est[, 3] <- apply(x, 2, quantile, probs = .5)
  est[, 4] <- apply(x, 2, quantile, probs = .975)
  return(est)
}

# TABLE OF SUMMARY OF POSTERIOR DISTRIBUTION ------------------------------------
PostTable <- function(x){
  # Returns latex table which summarized posterior distribution of parameters
  #
  # Args:
  #   x: name of stan model
  #
  # Returns:
  #   latex table format 
  coef <- PostSummary(x$beta)
  var <- PostSummary(cbind(x$sigma, x$sigma_delta))
  ps <- rbind(NA, coef, NA, NA, var)
  rownames(ps) <- c("\\emph{Coefficients}", "Intercept", xlabs, "",
                    "\\emph{Variance terms}", 
                    "District error ($\\sigma$)",
                    "National swing ($\\sigma_\\delta$)")
  return(ps)
}

# DATA FOR FORECASTING ----------------------------------------------------------
NewData <- function(t){
  dat <- subset(cd, year == t,  select = c(yvar, xvars, "int", "year", "stcd"))
  dat <- dat[complete.cases(dat), ]
  x <- as.matrix(dat[, c("int", xvars), with = FALSE])
  y <- unlist(dat[, yvar, with = FALSE]) 
  beta <- bhm.sims[[paste0("y", t)]]$beta
  sigma_delta <- bhm.sims[[paste0("y", t)]]$sigma_delta
  sigma <- bhm.sims[[paste0("y", t)]]$sigma
  return(list(x = x, y = y, beta = beta, sigma = sigma, sigma_delta = sigma_delta,
              stcd = dat$stcd, year = dat$year))
}

# FORECAST ----------------------------------------------------------------------
Forecast <- function(t){
  dat <- NewData(t) # uses NewData function created above
  y_new <- matrix(NA, nrow = length(dat$y), ncol = n.sims)
  for (s in 1:n.sims){
    delta.new <- rnorm(1, 0, dat$sigma_delta[s])
    y_new[, s] <- rnorm(nrow(y_new), delta.new + dat$x %*% dat$beta[s, ], dat$sigma[s])
  }
  return(list(y_new = y_new, y = dat$y, year = rep(t, length(dat$y)),
              stcd = dat$stcd))
}

# HISTOGRAM OF NUMBER OF SEATS --------------------------------------------------
HistSeats <- function(){
  predvote <-  data.table(year = bhm.pred$year, demvote = 1 * (bhm.pred$y_new >= 0.5))
  predseats <- predvote[, lapply(.SD, mean), by = year]
  predseats <- melt(predseats, "year", variable.name = "sim", value.name = "demseats")
  #quantiles <- predseats[, .(lower = quantile(demseats, prob = 0.01), 
  #                           upper = quantile(demseats, prob = 0.99)), 
  #                       by = year]
  #predseats <- merge(predseats, quantiles, by = "year")
  #predseats <- subset(predseats, demseats > lower & demseats < upper)
  actualvote <- data.table(year = bhm.pred$year, demvote = 1 * (bhm.pred$y >= 0.5))
  actualseats <- actualvote[, .(demseats = mean(demvote, na.rm = TRUE)), by = year]
  p <- ggplot(predseats, aes(x = demseats)) + 
    xlab("Percentage of Democratic seats") + 
    ylab("Count") + facet_wrap(~ year) +
    geom_histogram(color = "black", fill = "white", binwidth = .02) + 
    geom_vline(data = actualseats, aes(xintercept = demseats),
               color = "blue", linetype = "dashed")
  return(p)
}

# DIAGNOSTIC PLOTS NATIONAL MODEL -----------------------------------------------
diagplotNat <- function(simdata, name){
  for (i in 1:length(simdata)){
    pdf(file = paste0("figs/", name, i, ".pdf"),
        width = 8, height = 10.5)
    tmp.psi <- as.matrix(simdata[[i]]$psi)
    colnames(tmp.psi) <- "psi"
    plot(as.mcmc(
      cbind(t(simdata[[i]]$theta), simdata[[i]]$lambda, tmp.psi)
    ))
    dev.off()
  }
}

# DIAGNOSTIC PLOTS RELATIVE DISTRICT MODEL --------------------------------------
diagplotRel <- function(simdata, name){
  for (j in 1:length(simdata)){
    pdf(file = paste0("figs/", name, j, ".pdf"), width = 8, height = 10.5)
    tmp.ndistricts <- dim(simdata[[j]]$theta)[[3]]
    for (i in 1:tmp.ndistricts){
      if (i < tmp.ndistricts){
        # plot theta
        tmp.theta <- as.mcmc(simdata[[j]]$theta[,,i])
        colnames(tmp.theta) <- paste(dimnames(simdata[[j]]$theta)[[3]][i],
                                     colnames(tmp.theta), sep = "_")
        plot(tmp.theta)
      } else if (i == tmp.ndistricts){
        # plot final theta and psi
        tmp.theta <- simdata[[j]]$theta[,,i]
        colnames(tmp.theta) <- paste(dimnames(simdata[[j]]$theta)[[3]][i],
                                     colnames(tmp.theta), sep = "_")
        tmp.final <- cbind(tmp.theta, simdata[[j]]$psi)
        colnames(tmp.final)[ncol(tmp.final)] <- "psi"
        plot(as.mcmc(tmp.final))
      }
    }
    dev.off()  
  }
}
  

# BURN IN NATIONAL MODEL --------------------------------------------------------
natBurnin <- function(simdata, burn){
  for (i in 1:length(simdata)){
    simdata[[i]]$theta <- simdata[[i]]$theta[, -(1:burn)]
    simdata[[i]]$lambda <- simdata[[i]]$lambda[-(1:burn), ]
    simdata[[i]]$psi <- simdata[[i]]$psi[-(1:burn)]
  }
return(simdata)
}

# BURN IN FOR RELATIVE MODEL ----------------------------------------------------
relBurnin <- function(simdata, burn){
  for (i in 1:length(simdata)){
    simdata[[i]]$theta <- simdata[[i]]$theta[-(1:burn), , ]
    simdata[[i]]$psi <- simdata[[i]]$psi[-(1:burn)]
  }
return(simdata)
}

# NATIONAL OPINION --------------------------------------------------------------
natOpinion <- function(simdata){
  natopinion <- matrix(NA, nrow = max(gb$pte, na.rm = TRUE), 
                       ncol = length(simdata) + 1)
  natopinion[, 1] <- seq(max(gb$pte, na.rm = TRUE), 1)
  for (i in 1:(ncol(natopinion)-1)){
    tmptheta <- apply(simdata[[i]]$theta, 1, mean)
    natopinion[, i + 1] <-  tmptheta[-c(1, length(tmptheta))]
  }
  colnames(natopinion) <- c("pte", paste0("theta", seq(1, length(simdata))))
  natopinion <- data.table(natopinion)
  return(natopinion)
}

# DLM FORECAST ------------------------------------------------------------------
dlmForecast <- function(natsims, relsims, bhm){
  overall <- rel <- nat <- list()
  for (i in 1:length(natsims)){
    nat[[i]] <-  natsims[[i]]$theta[nrow(natsims[[i]]$theta), ]
    rel[[i]] <- relsims[[i]]$theta[, dim(relsims[[i]]$theta)[[2]], ]
    
    # need forecast for districts with no polls
    tmp <- bhm[!stcd %in% colnames(rel[[i]])]
    stcd <- tmp[, stcd]
    tmp[, c("year", "stcd") := NULL]
    tmp <- t(tmp)
    colnames(tmp) <- stcd
    rel[[i]] <- cbind(rel[[i]], tmp)
    
    # combine national and district prediction
    overall[[i]] <- nat[[i]] + rel[[i]]
  }
return(list(overall = overall, nat = nat, rel = rel))
}

# FORECAST EVALUATION TABLE -----------------------------------------------------
ForecastEvalTable <- function(data, outcome, predictions, FUN){
  Fun <- match.fun(FUN)
  dat <- subset(data, select = c(outcome, predictions))
  x <- melt(dat, id = outcome, variable.name = "Model")
  x <- x[, .(value = Fun(value, get(outcome))), by = "Model"] 
  x[, pte := as.numeric(sub("\\D+", "", Model))]
  x[, Model := sub("\\d+", "", Model)]
  return(x)
}

# FORECAST EVALUATION TABLE BY POLLING AVAILABILITY -----------------------------
ForecastEvalTableByPoll <- function(outcome, predictions, FUN){
  x1 <- ForecastEvalTable(predsummary2010[stcd %in% unique(polls[, stcd])], 
                            outcome = outcome, 
                            predictions = predictions, FUN = FUN)
  x1[, type := "Available polling data"]
  x2 <- ForecastEvalTable(predsummary2010[!stcd %in% unique(polls[, stcd])], 
                          outcome = outcome, 
                          predictions = predictions, FUN = FUN)
  x2[, type := "No available polling data"]
  x <- rbind(x1, x2)
  x[, month := (pte -1 ) /2]
  return(x)
}


# Q FOR MAXIMIZING SEAT SHARE (HIERARCHICAL) ------------------------------------
QMaxSeats <- function(t){
  dat <- NewData(t)
  g <- matrix(NA, nrow = length(dat$y), ncol = n.sims)
  for (s in 1:n.sims){
    delta.new <- rnorm(1, 0, dat$sigma_delta[s])
    g[, s] <- dnorm(1/2 - dat$x %*% dat$beta[s, ] - delta.new, 0, dat$sigma[s])
  }
  Q <- apply(g, 1, mean)
  qdat <- data.table(Q_maxseats = Q , stcd = dat$stcd,
                     year = dat$year)
  return(qdat)
}

# Q FOR MAXIMIZING SEAT SHARE (DLM) ---------------------------------------------
QDlmMaxseats <- function(t, prior = TRUE){
  if (prior == TRUE){
    dat <- dlm.pred
  } else {
    dat <- dlmnp.pred
  }
  nat <- dat$nat[[t]]
  rel.mean <- apply(dat$rel[[t]], 2, mean)
  rel.sd <- apply(dat$rel[[t]], 2, sd)
  sims <- length(nat)
  g <- matrix(NA, nrow = length(rel.mean), ncol = sims)
  for (s in 1:sims){
    g[, s] <- dnorm(1/2 - rel.mean - nat[s], 0, rel.sd)
  }
  Q <- apply(g, 1, mean)
  qdat <- data.table(Q = Q, stcd = names(rel.mean), 
                     pte = t)
  return(qdat)
}

# Q FOR MAXIMIZING PROBABILITY OF WINNING MAJORITY ------------------------------
QKRule <- function(t, k){
  dat <- NewData(t)
  dat.unc <- uncontested[year == t]
  Q.mean <- matrix(NA, nrow = 435, ncol = n.sims)
  Q.var <- matrix(NA, nrow = 435, ncol = n.sims)
  for (s in 1:n.sims){
    delta.new <- rnorm(1, 0, dat$sigma_delta[s])
    cpred <- 1/2 - dat$x %*% dat$beta[s, ] - delta.new
    G.unc <- dat.unc[, repwin]
    G <- c(pnorm(cpred, 0, dat$sigma[s]), G.unc)
    mu.seats <- sum(G)
    sigma.seats <- sum(G * (1 - G))
    x <- (k - mu.seats)/sigma.seats
    g <- c(dnorm(cpred, 0, dat$sigma[s]), rep(0, length(G.unc)))
    Q.mean[, s] <- (1/sigma.seats) * dnorm(x) * g
    Q.var[, s] <- (1/sigma.seats) * dnorm(x) * x * (1 - 2*G) * g
  }
  Q.mean <- apply(Q.mean, 1, mean)
  Q.var <- apply(Q.var, 1, mean)
  Q <- Q.mean + Q.var
  qdat <- data.frame(stcd = c(dat$stcd, dat.unc$stcd), year = c(dat$year, dat.unc$year), stringsAsFactors = FALSE)
  qdat[, paste0("Q_k", k)] <- Q
  #qdat[, paste0("Qshare_k", k)] <- 100 * Q / sum(Q)
  return(qdat)
}

# PREDICTING VOTE USING FIXED PARAMETERS ----------------------------------------
FixedPredict <- function(t, nsims){
  datsims <- bhm.sims[[paste0("y", t)]]
  dat <- NewData(t)
  demwin <- uncontested[year == t, 1 - repwin]
  beta <- apply(datsims$beta, 2, mean)
  sigma <- mean(datsims$sigma)
  sigma_delta <- mean(datsims$sigma_delta)
  ypred <- matrix(NA, nrow = 435, ncol = nsims)
  for (s in 1:nsims){
    delta <- rnorm(1, 0, sigma_delta)
    ypred[, s] <- c(rnorm(length(dat$y), delta + dat$x %*% beta, sigma),
                    demwin)
  }
  return(ypred)
}

# PROBABILITY OF BEING A DECISIVE SWING STATE -----------------------------------
DecisiveSwing <- function(t, ynew){
  y_new <- ynew
  decisive_sim <- apply(y_new, 2, function (x) sum(x > 0.5))
  y_decisive <- y_new[, which(decisive_sim >= 217 & decisive_sim <= 218)]
  swing <- apply(y_decisive, 1, function (x) sum(x > 0.48 & x < 0.52))
  stcd <- c(bhm.pred$stcd[which(bhm.pred$year == t)], uncontested[year == t, stcd])
  return(data.frame(swing = swing/n.sims, stcd = stcd))
}

# REGRESSION COEFFICIENTS AND STANDARD ERRORS FROM MODEL FIT--------------------
MyCoef <- function(model, vars){
  # Returns coefficients and robust standard errors from regression model
  #
  # Args:
  #   model: name of regression model
  #
  # Returns:
  #   Matrix with column of coefficients and column of standard errors
  coef <- coef(model)[vars]
  se <- sqrt(diag(vcovHC(model, type = "HC1"))[vars])
  return(cbind(coef, se))
}

# REGRESSION TABLE WITH STANDARD ERRORS IN PARENTEHSES--------------------------
RegTable <- function(model, vars, varnames){
  # Regression table with standard errors in parentheses
  #
  # Args:
  #   model: list of lm models
  #   vars: name of variables to report
  #   varnames: labels for vars to be reported in table
  #
  # Returns:
  #   Table with coefficients and se's in parentheses
  n.est <- length(model)
  coef <- matrix(NA, nrow = length(vars), ncol = n.est)
  se <- coef
  obs <- rep(NA, n.est)
  r2 <- rep(NA, n.est)
  for (i in 1:n.est){
    est <- MyCoef(model[[i]], vars)
    coef[, i] <- est[, 1]
    se[, i] <- est[, 2]
    obs[i] <- nrow(model.matrix(model[[i]]))
    r2[i] <- summary(model[[i]])$adj.r.squared
  }
  coef <- formatC(coef , format="f", digits=3)
  se <- formatC(se , format="f", digits=3)
  names <- c(as.vector(rbind(varnames, "")), "Observations", "Adjusted R-squared")
  table <-  matrix(as.vector(rbind(as.vector(coef),
                                   paste("(",as.vector(se),")", sep=""))), 
                   nrow=2*nrow(coef))
  table <- gsub(" ", "", table)
  table <- ifelse(table == "NA" | table == "(NA)", "", table)
  obs <- formatC(obs, format = "d", big.mark = ",")
  obs <- paste0("\\multicolumn{1}{c}{", obs, "}")
  table <- rbind(table, obs, formatC(r2 , format="f", digits=3))
  table <- cbind(names, table)
  rownames(table) <- rep("", length(rownames(table)))
  return(table)
}

# LAST N CHARACTERS IN A STRING ------------------------------------------------
SubstrRight <- function(x, n) {
  # Args:
  #   x: character vector
  #   n: number of characters from end of string
  #
  # Returns:
  #   last n characters of a string  
  substr(x, nchar(x)-n+1, nchar(x))
}

# SHARE A LEGEND USING MULTIPLE PLOTS WITH DIFFERENT AXES ----------------------
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


# MY GGPLOT --------------------------------------------------------------------
MyPlot <- function(data, xvar, yvar, xlab, ylab, colvar = NULL,
                     facetvar, facet.scale = "fixed", facet.to, facet.from,
                     vline, lines = FALSE, points = TRUE, point.size = 1.5,
                     point.pos = "identity", axis.size = 10, smooth = FALSE, 
                     smooth.ci = FALSE, smooth.method = "loess",
                     smooth.size = 1) {
  # Creates a ggplot with my preferred specifications
  #
  # Args:
  #   data: data for plot
  #   xvar: variable on x-axis
  #   yvar: variable on y-axis
  #   xlab: label for x-axis
  #   ylab: label for y-axis
  #   lines: should points be connected by lines. default is no. 
  #   points: should plot have points
  #   point.size: size of points. default is size 2
  #   point.pos: position of geom_point. default is identity
  #   axis.size: size of text on x and y axes
  #   colvar: make separate colors on each plot by values of this variable. 
  #           default is not to do this.
  #   facetvar: variable to crate subplots by
  #   facet.scale: should each sub-plot have its own y-axis scale
  #   facet.from: initial values of facetvar 
  #   facet.to: choose new values of facetvar
  #   default is fixed
  #   smooth: equals TRUE if a smoother is used. default is false
  #   smooth.ci: should smoothing line include a confidence interval
  #   smooth.method: method to use for smoothing. default is to have R choose
  #
  # Returns:
  #   ggplot object  
  if (!missing(facet.to)) {
    data[, facetvar] <- factor(data[, facetvar], levels = facet.from)
    data[, facetvar] <- mapvalues(data[, facetvar], from = facet.from, 
                                  to = facet.to)
  }
  p <- ggplot(data = data, aes_string(x = xvar, y = yvar,
              color = colvar)) + 
              scale_shape(solid=FALSE) +                                 
              xlab(xlab) + ylab(ylab) + 
              scale_colour_discrete(name = "") + theme_bw() +
              theme(legend.position="bottom", legend.text=element_text(size=7),
              axis.text.x = element_text(size = axis.size),
              axis.text.y = element_text(size = axis.size)) 
  if (points == TRUE) {
    p <- p + geom_point(size = point.size, position = point.pos)
  }
  if (lines == TRUE) {
    p <- p + geom_line()
  }
  if (!missing(facetvar)) {
    p <- p + facet_wrap(facetvar, scales = facet.scale)
  }
  if (!missing(vline)){
    p <- p + geom_vline(xintercept = vline, linetype = "dotted")
  }
  if (smooth == TRUE) {
    p <- p + geom_smooth(se = smooth.ci, method = smooth.method,
                         size = smooth.size)
  }
  return(p)
}

# CALCULATE FUN OF YVAR AND CONVERT TO WIDE FORMAT -----------------------------
WideVars <- function(data, yvars, byvar = NULL, widevar, timevar, FUN = mean) {
  # Calculates function of yvars by byvar, widevar, and time
  #
  # Args:
  #   data: dataframe for calcualtion
  #   yvars: variables to make calculates on
  #   timevar: unit of time for calculation
  #   FUN: function to apply to yvar. default is mean
  #
  # Returns:
  #   dataframe with mean yvar for each widevar
  #
  dat <- data[, c(yvars, byvar, widevar, timevar)]
  plot.dat <- ddply(dat, c(byvar, widevar, timevar), colwise(FUN, na.rm=TRUE))
  plot.dat <- reshape(plot.dat, timevar = widevar, 
                      idvar = c(byvar, timevar), direction = "wide")
  return(plot.dat)
}

# TIME-SERIES PLOT -------------------------------------------------------------
TsPlot <- function(data, yvars, ylab, facetnames, colvar = NULL,
                   colvar.levels, timevar, weight, smoothing = FALSE, 
                   lines = TRUE, axis.size = 10, facet.scale = "fixed") {
  # Plot of mean of yvars by colvar. plots are faceted by yvar with
  # names given by facetnames.
  #
  # Args:
  #   data: dataframe for analysis
  #   yvars: y-variables to include in plot
  #   ylab: name for y-axis in plot
  #   facetnames: user chosen names for separate plots using facet_wrap
  #   colvar: make separate colors on each plot by values of this variable. 
  #           default is not to do this.
  #   colvar.levels: character vector of levels of colvar
  #   weight: weights for weighted mean of y-variables. if not specified,
  #           non-weighted means are produced. Weights are
  #           named V1, V2, ...
  #   axis.size: size of axis text
  #   smoothings: option to add smoothed line to the plot
  #   lines: should lines connect points. default is yes
  #   facet.scale: should x and y scales be fixed or free. 
  #                default is fixed. look at options for
  #                scales in facet_wrap
  #
  # Returns:
  #   ggplot object 
  if (missing(weight)) {
    weight <- as.data.frame(matrix(1, nrow(data), ncol = length(yvars)))
    dat <- cbind(data[, c(yvars, timevar, colvar)], weight)
    weight <- names(weight)
  } else {
    dat <- data[, c(yvars, weight, colvar, timevar)]  
  }
  dat <- reshape(data = dat, 
                 varying = list(yvars, weight), 
                 v.names = c("yvar", "weight"),
                 timevar = "facetname", times = facetnames, 
                 dir = "long")
  dat <- dat[complete.cases(dat),]
  plot.dat <- ddply(dat, c(timevar, colvar, "facetname"),
                    function(x)
                    data.frame(yvar = weighted.mean(x$yvar, x$weight)))
  plot.dat$facetname <- factor(plot.dat$facetname, levels = facetnames) 
  if (!missing(colvar)) {
    plot.dat[, colvar] <- factor(plot.dat[, colvar], 
                                 levels = colvar.levels)
  }
  if (length(yvars) == 1) {
    tsplot <- MyPlot(data = plot.dat, xvar = timevar, yvar = "yvar", 
                     xlab = "Year", ylab = ylab, colvar = colvar,
                     lines = lines, smooth = smoothing, 
                     axis.size = axis.size)
  } else {
    tsplot <- MyPlot(data = plot.dat, xvar = timevar, yvar = "yvar", 
                     xlab = "Year", ylab = ylab, colvar = colvar,
                     facetvar = "facetname", 
                     facet.scale = facet.scale, axis.size = axis.size,
                     lines = lines, smooth = smoothing)
  }

  return(tsplot)
}

# SCATTERPLOTS BY YEAR ---------------------------------------------------------
ScatterByYear <- function(data, xvar, yvar, yvarlab, facetvar, smooth = TRUE,
                          colvar = NULL, facet.scale = "fixed", 
                          xlab = "1960 percent black") {
  # Creates a scatterplot of dependent variable by facetvar (i.e. year)
  # with points colored by state type
  # 
  # Args:
  #   data: dataframe for analysis
  #   xvar: x-point
  #   yvar: y-point
  #   yvarlab: y-axis label
  #   colvar: make separate colors on each plot by values of this variable. 
  #           default is not to do this.
  #   facetvar: make separate plots based on facetvar
  #
  # Returns:
  #   ggplot object  
  
  data <- data[, c(xvar, yvar, facetvar, colvar)]
  data <- data[complete.cases(data), ]
  scat <- MyPlot(data = data, smooth = smooth, smooth.ci = TRUE,
                 xvar = xvar, yvar = yvar, colvar = colvar,
                 facetvar = facetvar, facet.scale = facet.scale,
                 xlab = xlab, point.pos = "jitter",
                 ylab = yvarlab)
  return(scat)
}

# REGRESSIONS BY YEAR ----------------------------------------------------------
RegByDate <- function(data, yvar, x.time, x.main,
                      x.controls = NULL) {
  # Runs regression by year and dependent variable and returns results.
  #
  # Args:
  #   data: dataframe for running regression analyis
  #   yvar: dependent variable
  #   xvar: right-hand side regression variables + year + state type
  #   timevar: name of time variable
  #
  # Returns:
  #   dataframe of regression results 
  x <- data[, c(x.main, x.time, x.controls)]
  n.x.main <- length(x.main)
  col.names <- c("var", "date", "yvar", "coef", "se") 
  ncols <- length(col.names)
  temp <- matrix(NA, nrow = n.x.main, ncol = ncols)
  colnames(temp) <- col.names
  temp <- as.data.frame(temp)
  est <- list()
  modfit <- list()
  regsum <- list()
  y <- data[, yvar]
  dat <- cbind(y, x)
  dat <- dat[is.na(y) == FALSE, ]
  dates <- sort(unique(dat[, x.time]))
  ndates <- length(dates)
  rhs <- paste(c(x.main, x.controls), collapse = " + ")
  f <- paste("y ~ ",rhs)
  for (t in 1:ndates) {
    fit <- lm(f, data = dat[dat[, x.time] == dates[t], ])
    temp[, "var"] <- x.main
    temp[, "date"] <- dates[t]
    temp[, "yvar"] <- yvar
    temp[, "coef"] <- coef(fit)[x.main]
    temp[, "se"] <- coeftest(fit)[,2][x.main]
    est[[t]] <- temp
    modfit[[t]] <- cbind(fit$fitted.values, fit$residuals, dates[t])
    colnames(modfit[[t]]) <- c("yhat", "res", "date")
    modfit[[t]] <- cbind(fit$model, modfit[[t]]) 
    regsum[[t]] <- cbind(summary(fit)$adj.r.squared, summary(fit)$sigma, nrow(fit$model), dates[t])
    colnames(regsum[[t]]) <- c("adjR2", "rmse", "N", "date")
  }
  est <- do.call(rbind, est)
  modfit <- do.call(rbind, modfit)
  regsum <- do.call(rbind, regsum)
  l <- list(est = est, modfit = modfit, regsum = regsum)
  return(l)
} 

# REGRESSIONS BY YEAR AND TYPE -------------------------------------------------
RegByDateType <- function(data, yvar, x.controls = NULL, x.time, x.main, type,
                          labels = NULL, levels = NULL){
  # Uses regression by year function to run regressions by year and type
  #
  # Args:
  #   data: dataframe for running regression analyis
  #   yvar: vector of dependent variables
  #   x.controls: name of control variables
  #   x.time: name time variable
  #   x.main: name of main rhs variables
  #   timevar: name of time variable
  #   type: variable run separate regressions by
  #   labels: label for type variable
  #   levels: levels to match with labels for type variable
  #
  # Returns:
  #   dataframe of regression results by literacy test status with newly
  #   created literacy test factor variable
  reg <- list()
  k <- 1
  for (i in unique(data[, type])){
    reg[[k]] <- RegByDate(data = data[data[, type] == i, ], 
                          yvar = yvar,
                          x.controls = x.controls, x.time = x.time,
                          x.main = x.main)
    reg[[k]]$type <- i
    k <- k + 1
  }
  reg <- do.call(rbind, reg)
  if (!missing(labels)){
    reg$type <- factor(reg$type, levels = levels, labels = labels)
  }
  return(reg)
}


