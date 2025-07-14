#' @title Small Area Estimation using Hierarchical Bayes Twofold Subarea Level Model under Beta Distribution
#'
#' @description
#' Function `betaTF` used for estimation of subarea and area means simultaneously under Twofold Subarea Level Small Area Estimation Model Using Hierarchical Bayesian Method with Beta distribution
#' The range of data must be \eqn{0<y<1}.
#'
#' @param formula  Formula that describe the fitted model
#' @param area  Index that describes the code relating to area in each subarea.This should be defined for aggregation to get area estimator. Index start from 1 until m
#' @param weight  Vector contain proportion units or proportion of population on each subarea. \eqn{w_{ij}}
#' @param iter.update  Number of updates perform ( default = `3`)
#' @param iter.mcmc  Number of total iterations per chain (default = `1000`)
#' @param coef  Vector contains prior initial value of Coefficient of Regression Model for fixed effect with default vector of `0` with the length of the number of regression coefficients
#' @param var.coef Vector contains prior initial value of variance of Coefficient of Regression Model for fixed effect with default vector of `1` with the length of the number of regression coefficients
#' @param thin  Thinning rate, must be a positive integer
#' @param burn.in  Number of iterations to discard at the beginning
#' @param sigma2.u  Number of prior initial value of variance of subarea random effect
#' @param sigma2.v  Number of prior initial value of variance of area random effect
#' @param data  The data frame
#'
#' @return  This function returns a list with following objects:
#'   \item{Est_sub}{A dataframe contains the values, standard deviation, and quantile of Subarea mean Estimates using Twofold Subarea level model under Hierarchical Bayes method}
#'   \item{Est_area}{A dataframe contains the values, standard deviation, and quantile of Area mean Estimates using Twofold Subarea level model under Hierarchical Bayes method}
#'   \item{area_randeff}{A dataframe contains area random effect}
#'   \item{sub_randeff}{A dataframe contains subarea random effect}
#'   \item{refVar}{A dataframe that contains estimated subarea and area random effect variance \eqn{(\sigma_{u}^{2}} and \eqn{\sigma_{v}^{2})}}
#'   \item{coefficient}{A dataframe that contains the estimated model coefficient \eqn{\beta}}
#'   \item{plot}{Trace, Density, Autocorrelation Function Plot of coefficient}
#'
#' @import rstan
#' @import bayesplot
#' @import stringr
#'
#' @export betaTF
#'
#' @examples
#' fit <- betaTF(y~X1+X2,area="codearea",weight="w",data=dataBeta, iter.mcmc = 500)
#'
betaTF <- function(formula, area, weight, iter.update=3, iter.mcmc=1000, coef = NULL, var.coef = NULL, thin = 1, burn.in = floor(iter.mcmc / 2), sigma2.u = 1, sigma2.v = 1, data){

  result <- list()
  formuladata <- model.frame(formula,data,na.action=NULL)

  #check if the auxiliary variable contain NA
  if (any(is.na(formuladata[,-1]))) {
    stop("Auxiliary Variables contains NA values.")
  }

  auxVar <- as.matrix(formuladata[,-1]) # Auxiliary variable matrix
  nvar <- ncol(auxVar) + 1 # Number for regression coefficient

  formuladata <- data.frame(formuladata, codearea=data[,area], weight=data[,weight])

  #check var.coef
  if (!missing(var.coef)){
    if( length(var.coef) != nvar ){
      stop("length of vector var.coef does not match the number of regression coefficients, the length must be ",nvar)
    }
    sigma2.b.value = var.coef
  }else{
    sigma2.b.value = rep(1,nvar)
  }

  #check coef
  if (!missing(coef)){
    if( length(coef) != nvar ){
      stop("length of vector coef does not match the number of regression coefficients, the length must be ",nvar)
    }
    mu.b.value = coef
  } else {
    mu.b.value = rep(0,nvar)
  }

  #check for iter.update
  if (iter.update < 3){
    stop("the number of iteration updates at least 3 times")
  }

  for (i in 1:nrow(formuladata)) {
    if (!is.na(formuladata[i, 1])) {
      if (is.na(formuladata[i, (nvar + 1)])) {
        stop(formula[2], "[", i, "] is not NA but codearea is NA")
      }else if(is.na(formuladata[i, (nvar + 2)])){
        stop(formula[2], "[", i, "] is not NA but weight is NA")
      }
    }
  }

  # check whether ydir is not NA
  if(!any(is.na(formuladata[,1]))){
    formuladata <- as.matrix(na.omit(formuladata))

    # check 0<ydir<1
    if (any(formuladata[,1]<=0) || any(formuladata[,1]>=1)){
      stop("response variable must be 0 < " ,formula[2], " < 1")
    }

    n <- nrow(formuladata) # Number of observation
    m <- length(unique(formuladata[,(nvar+1)])) # Number of area
    mu.b = mu.b.value
    sigma2.b = sigma2.b.value
    tau.ua = tau.ub = tau.va = tau.vb = phi.aa = phi.ab = phi.ba = phi.bb = 1


    for (iter in 1:iter.update) {
      stan_data <- list(
        n = n,
        nvar = nvar,
        auxvar = nvar-1,
        m = m,
        y = formuladata[, 1],
        x = as.matrix(formuladata[, 2:nvar]),
        state2 = formuladata[, nvar + 1],
        mu_b = mu.b,
        sigma2_b = sigma2.b,
        tau_ua = tau.ua,
        tau_ub = tau.ub,
        tau_va = tau.va,
        tau_vb = tau.vb,
        phi_aa = phi.aa,
        phi_ab = phi.ab,
        phi_ba = phi.ba,
        phi_bb = phi.bb
      )

      init_values <- list(
        u = rep(0, n),
        f = rep(0, m),
        b = mu.b,
        sigma2_u = sigma2.u,
        sigma2_v = sigma2.v
      )

      fit <- rstan::sampling(
        stanmodels$saeHB_TF_beta,
        data = stan_data,
        init = list(init_values),
        iter = iter.mcmc,
        warmup = burn.in,
        chains = 1,
        thin = thin,
        control = list(adapt_delta = 0.8),
        verbose = FALSE
      )

      # Extract posterior samples
      result_samps <- rstan::summary(fit, pars = c("mu", "b", "phi_a", "phi_b", "sigma2_u", "sigma2_v"))
      result_stats <- result_samps$summary

      # Update hyperparameters for next iteration
      a_var <- result_stats["sigma2_u", "mean"]
      beta <- result_stats[grep("^b\\[", rownames(result_stats)), c("mean", "sd")]
      b_var <- result_stats["sigma2_v", "mean"]
      #
      for (i in 1:nvar) {
        mu.b[i] <- beta[i, "mean"]
        sigma2.b[i] <- beta[i, "sd"]^2
      }

      phi.aa <- result_stats["phi_a", "mean"]^2 / result_stats["phi_a", "sd"]^2
      phi.ab <- result_stats["phi_a", "mean"] / result_stats["phi_a", "sd"]^2
      phi.ba <- result_stats["phi_b", "mean"]^2 / result_stats["phi_b", "sd"]^2
      phi.bb <- result_stats["phi_b", "mean"] / result_stats["phi_b", "sd"]^2
      tau.ua <- result_stats["sigma2_u", "mean"]^2 / result_stats["sigma2_u", "sd"]^2
      tau.ub <- result_stats["sigma2_u", "mean"] / result_stats["sigma2_u", "sd"]^2
      tau.va <- result_stats["sigma2_v", "mean"]^2 / result_stats["sigma2_v", "sd"]^2
      tau.vb <- result_stats["sigma2_v", "mean"] / result_stats["sigma2_v", "sd"]^2
    }

      result_samps <- rstan::summary(fit, pars = c("mu", "b", "sigma2_u", "sigma2_v", "f", "u"))
      result_stats <- result_samps$summary

      mu <- result_stats[grep("^mu\\[", rownames(result_stats)), c("mean", "sd")]

      # Create a list of variable names for 'b'
      b.varnames <- vector("list", nvar)
      for (i in 1:nvar) {
        idx.b.varnames <- as.character(i-1)
        b.varnames[i] <- str_replace_all(paste("b[", idx.b.varnames, "]"), pattern=" ", replacement="")
      }

      # Extract MCMC samples from the Stan's sampling result
      result_mcmc <- rstan::extract(fit)
      colnames(result_mcmc$b) <- b.varnames

      # Access summary statistics
      a_var <- result_stats["sigma2_u", "mean"]
      beta <- result_stats[grep("^b\\[", rownames(result_stats)), c("mean", "sd")]
      b_var <- result_stats["sigma2_v", "mean"]

      # Extract random effect
      f_mean <- result_stats[grep("^f\\[", rownames(result_stats)), "mean"]
      area_randeff <- data.frame(f_mean)
      u_mean <- result_stats[grep("^u\\[", rownames(result_stats)), "mean"]
      sub_randeff <- data.frame(u_mean)

      refVari <- as.data.frame(cbind(b_var, a_var))
      rownames(beta) <- b.varnames

      # Subarea estimation
      Estimation <- as.data.frame(mu)

      Quantiles <- result_stats[, c("2.5%", "25%", "50%", "75%", "97.5%")]

      q_beta <- Quantiles[grep("^b\\[", rownames(result_stats)), ]  # Quantiles for beta coefficients
      q_mu <- Quantiles[1:n, ]  # Quantiles for mu
      rownames(q_beta) <- b.varnames
      beta <- data.frame(cbind(beta, q_beta))

      Estimation <- data.frame(Estimation, q_mu)
      colnames(Estimation) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")
      colnames(beta) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")

      # Area estimation
      w <- gr <- 0
      result_mcmc_area <- data.frame(t(result_mcmc$mu))

      dtarea <- data.frame(result_mcmc_area, w = data[[weight]], gr = data[[area]])
      cols_to_agg <- setdiff(names(dtarea), c("gr", "w"))
      split_dt <- split(dtarea, dtarea$gr)
      Quantilesdt <- do.call(rbind, lapply(split_dt, function(df) {
        w <- df$w
        vals <- df[, cols_to_agg, drop = FALSE]
        data.frame(gr = df$gr[1], t(colSums(vals * w)))
      }))
      rownames(Quantilesdt) <- NULL
      Quant_matrix <- Quantilesdt[ , -1]
      Quantiles_area <- apply(Quant_matrix, 1, function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      Quantiles_Mean <- rowMeans(Quant_matrix)
      Quantiles_SD <- apply(Quant_matrix, 1, sd)
      Est_area2 <- data.frame(Mean = Quantiles_Mean, SD = Quantiles_SD, t(Quantiles_area))
      colnames(Est_area2) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")

  } else {
    formuladata <- as.data.frame(formuladata)

    n <- nrow(formuladata) # Number of observation/subarea
    m <- length(unique(formuladata[,(nvar+1)])) # Number of area
    formuladata$idx <- rep(1:n)
    data_sampled <- na.omit(formuladata)
    data_nonsampled <- formuladata[-data_sampled$idx, ]
    r = data_nonsampled$idx
    n1 = nrow(data_sampled)
    n2 = nrow(data_nonsampled)
    mu.b = mu.b.value
    sigma2.b = sigma2.b.value
    tau.ub = tau.ua = phi.aa=phi.ab = phi.ba=phi.bb = tau.va=tau.vb= 1

    # check 0<ydir<1
    if (any(data_sampled[,1]<=0) || any(data_sampled[,1]>=1)){
      stop("response variable must be 0 < " ,formula[2], " < 1")
    }

    for (iter in 1:iter.update) {
      stan_data <- list(
        n1 = n1,
        n2 = n2,
        nvar = nvar,
        auxvar = nvar-1,
        m = m,
        y_sampled = data_sampled[, 1],
        x_sampled = data_sampled[,2:nvar],
        x_nonsampled=data_nonsampled[,2:nvar],
        state2_sampled = data_sampled[, nvar + 1],
        state2_nonsampled = data_nonsampled[, nvar + 1],
        mu_b = mu.b,
        sigma2_b = sigma2.b,

        tau_ua = tau.ua,
        tau_ub = tau.ub,
        tau_va = tau.va,
        tau_vb = tau.vb,
        phi_aa = phi.aa,
        phi_ab = phi.ab,
        phi_ba = phi.ba,
        phi_bb = phi.bb
      )

      init_values <- list(
        u1 = rep(0, n1),
        u2 = rep(0, n2),
        f = rep(0, m),
        b = mu.b,
        sigma2_u = sigma2.u,
        sigma2_v = sigma2.v
      )

      fit <- rstan::sampling(
        stanmodels$saeHB_TF_beta_NA,
        data = stan_data,
        init = list(init_values),
        iter = iter.mcmc,
        warmup = burn.in,
        chains = 1,
        thin = thin,
        control = list(adapt_delta = 0.8),
        verbose = FALSE
      )

      # Extract posterior samples
      result_samps <- rstan::summary(fit, pars = c("mu_sampled","mu_nonsampled", "b", "phi_a", "phi_b", "sigma2_u", "sigma2_v"))
      result_stats <- result_samps$summary

      # Update hyperparameters for next iteration
      a_var <- result_stats["sigma2_u", "mean"]
      beta <- result_stats[grep("^b\\[", rownames(result_stats)), c("mean", "sd")]
      b_var <- result_stats["sigma2_v", "mean"]

      for (i in 1:nvar) {
        mu.b[i] <- beta[i, "mean"]
        sigma2.b[i] <- beta[i, "sd"]^2
      }

      phi.aa <- result_stats["phi_a", "mean"]^2 / result_stats["phi_a", "sd"]^2
      phi.ab <- result_stats["phi_a", "mean"] / result_stats["phi_a", "sd"]^2
      phi.ba <- result_stats["phi_b", "mean"]^2 / result_stats["phi_b", "sd"]^2
      phi.bb <- result_stats["phi_b", "mean"] / result_stats["phi_b", "sd"]^2
      tau.ua <- result_stats["sigma2_u", "mean"]^2 / result_stats["sigma2_u", "sd"]^2
      tau.ub <- result_stats["sigma2_u", "mean"] / result_stats["sigma2_u", "sd"]^2
      tau.va <- result_stats["sigma2_v", "mean"]^2 / result_stats["sigma2_v", "sd"]^2
      tau.vb <- result_stats["sigma2_v", "mean"] / result_stats["sigma2_v", "sd"]^2
    }

    result_samps <- rstan::summary(fit, pars = c("mu_sampled", "mu_nonsampled", "b", "sigma2_u", "sigma2_v", "f", "u1", "u2"))
    result_stats <- result_samps$summary

    mu <- result_stats[1:n1, c("mean", "sd")]
    mu.nonsampled <- result_stats[(n1+1):n, c("mean", "sd")]

    # Create a list of variable names for 'b'
    b.varnames <- vector("list", nvar)
    for (i in 1:nvar) {
      idx.b.varnames <- as.character(i-1)
      b.varnames[i] <- str_replace_all(paste("b[", idx.b.varnames, "]"), pattern=" ", replacement="")
    }

    # Extract MCMC samples from the Stan's sampling result
    result_mcmc <- rstan::extract(fit)
    colnames(result_mcmc$b) <- b.varnames

    # Access summary statistics
    a_var <- result_stats["sigma2_u", "mean"]
    beta <- result_stats[grep("^b\\[", rownames(result_stats)), c("mean", "sd")]
    b_var <- result_stats["sigma2_v", "mean"]

    # Extract random effect
    f_mean <- result_stats[grep("^f\\[", rownames(result_stats)), "mean"]
    area_randeff <- data.frame(f_mean)
    u_mean <- matrix(rep(0,n))
    u_mean[r,] <- result_stats[grep("^u2\\[", rownames(result_stats)), "mean"]
    u_mean[-r,] <- result_stats[grep("^u1\\[", rownames(result_stats)), "mean"]
    sub_randeff <- data.frame(u_mean)

    refVari <- data.frame(b_var, a_var)
    rownames(beta) <- b.varnames

    # Subarea estimation
    Estimation <- matrix(rep(0,n),n,2)
    Estimation[r,]=mu.nonsampled
    Estimation[-r,]=mu
    Estimation = as.data.frame(Estimation)

    Quantiles <- result_stats[, c("2.5%", "25%", "50%", "75%", "97.5%")]

    q_beta <- Quantiles[grep("^b\\[", rownames(result_stats)), ]  # Quantiles for beta coefficients
    q_mu <- Quantiles[1:n1, ]  # Quantiles for mu sampled
    q_mu.nonsampled <- Quantiles[(n1+1):n, ]  # Quantiles for mu nonsampled
    rownames(q_beta) <- b.varnames
    beta <- data.frame(cbind(beta, q_beta))

    q_Estimation <- matrix(0,n,5)
    for (i in 1:5){
      q_Estimation[r,i] <- q_mu.nonsampled[,i]
      q_Estimation[-r,i] <- q_mu[,i]
    }

    Estimation <- data.frame(Estimation, q_Estimation)
    colnames(Estimation) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")

    # Area estimation
    w <- gr <- 0
    result_mcmc_area_s <- data.frame(t(result_mcmc$mu_sampled))
    result_mcmc_area_ns <- data.frame(t(result_mcmc$mu_nonsampled))
    result_mcmc_area <- matrix(0,n,ncol(result_mcmc_area_s))

    for(i in 1:ncol(result_mcmc_area_s)){
      result_mcmc_area[r,i]<-result_mcmc_area_ns[,i]
      result_mcmc_area[-r,i]<-result_mcmc_area_s[,i]
    }

    dtarea <- data.frame(result_mcmc_area, w = data[[weight]], gr = data[[area]])
    cols_to_agg <- setdiff(names(dtarea), c("gr", "w"))
    split_dt <- split(dtarea, dtarea$gr)
    Quantilesdt <- do.call(rbind, lapply(split_dt, function(df) {
      w <- df$w
      vals <- df[, cols_to_agg, drop = FALSE]
      data.frame(gr = df$gr[1], t(colSums(vals * w)))
    }))
    rownames(Quantilesdt) <- NULL
    Quant_matrix <- Quantilesdt[ , -1]
    Quantiles_area <- apply(Quant_matrix, 1, function(x) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
    Quantiles_Mean <- rowMeans(Quant_matrix)
    Quantiles_SD <- apply(Quant_matrix, 1, sd)
    Est_area2 <- data.frame(Mean = Quantiles_Mean, SD = Quantiles_SD, t(Quantiles_area))
    colnames(Est_area2) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%")

  }
  rhats <- rhat(fit, regex_pars = "^b\\[")
  result$Est_sub = Estimation
  result$Est_area = Est_area2
  result$area_randeff = area_randeff
  result$sub_randeff = sub_randeff
  result$refVar = refVari
  result$coefficient = beta
  result$plot = list(mcmc_trace(fit, regex_pars = "^b\\["), mcmc_dens(fit, regex_pars = "^b\\["), mcmc_acf_bar(fit, regex_pars = "^b\\["), mcmc_rhat(rhats[grep("^b\\[", names(rhats))]) + yaxis_text(hjust = 0))
  return(result)
}
