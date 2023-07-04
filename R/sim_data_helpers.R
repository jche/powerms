
# helper functions for sim_data()

# reorders vec such that it is correlated with cor_vec
induce_correlation <- function(vec, cor_vec, cor) {
  if (cor == 0 | sd(vec) == 0) return(vec)
  vec_temp <- vec[order(
    cor^2 * vec + (1-cor^2) * rnorm(length(vec), sd=sd(vec)) )]
  # magic hack: avoids sorting tau_j
  vec_temp[order(order(sign(cor) * cor_vec))]
}

gen_site_params <- function(J,
                            intercept_dist = c("normal"),
                            effect_dist = c("normal", "gamma"),
                            alpha=NULL, sig_alpha=NULL,
                            tau=NULL, sig_tau=NULL, rho=0,
                            a=NULL, b=NULL) {
  intercept_dist <- match.arg(intercept_dist)
  effect_dist <- match.arg(effect_dist)

  # generate intercepts
  if (intercept_dist == "normal") {
    alpha_j <- rnorm(J, mean=alpha, sd=sig_alpha)
  } else {
    stop("Non-normal intercept distributions not currently implemented")
  }

  # generate effects
  if (effect_dist == "normal") {
    # generate tau_j, potentially correlated with alpha_j
    mu <- tau + rho*sig_tau/sig_alpha * (alpha_j-alpha)
    var <- (1-rho^2) * sig_tau^2
    tau_j <- rnorm(J, mean=mu, sd=sqrt(var))
  } else if (effect_dist == "gamma") {
    stopifnot("Gamma effect distribution requires parameter a" = !is.null(a))
    stopifnot("Gamma effect distribution requires parameter b" = !is.null(b))
    if (rho != 0) {
      warning("Correlation between intercept and effect not implemented for Gamma effect distribution!")
    }
    tau_j <- rgamma(J, shape=a, rate=b)
  }
  list(alpha_j = alpha_j, tau_j = tau_j)
}


# pulled from blkvar package
#  - minimum site size: 4
#  - sd(nj) is roughly: nbar * sqrt(size_ratio)
gen_site_sizes <- function(nbar, J, size_ratio=1/3, vary=T) {
  stopifnot("Average site size (nbar) must be greater than 4" = nbar > 4)

  if (vary) {
    N <- 1 + 3 * size_ratio
    p <- (N - 1)/N
    small <- rbinom(J, 1, p)
    Y <- runif(J)
    Y <- nbar * ifelse(small, Y, Y * (N - 1) + 1)

    # ensure all sites have at least 4 observations
    nj <- round(Y)
    nj[nj < 4] <- 4
  } else {
    nj <- rep(nbar, J)
  }
  nj
}

gen_site_ps <- function(pbar, J, p_ratio=0.75, vary=T) {
  stopifnot(0 <= pbar & pbar <= 1)
  if (vary) {
    ths <- min(pbar, 1 - pbar) * p_ratio
    site_ps <- runif(J, pbar - ths, pbar + ths)
  } else {
    site_ps <- rep(pbar, J)
  }
  site_ps
}

floor_ceil <- function(x, floor=0, ceil=1) {
  p_adj <- sum(x<floor | x>ceil) / length(x)
  if (p_adj > 0.1) {
    warning(paste0(
      round(p_adj*100),
      "% of probabilities required truncation at ",
      floor,
      " or ",
      ceil,
      ". Consider adjusting probabilities to reduce truncation."))
  }
  pmax(pmin(x, ceil), floor)
}
