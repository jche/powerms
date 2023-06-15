
# functions to simulate datasets
#  - input: all parameters of dataset
#  - output: individual-level dataset

# naming: sim_x_yz
#  - x: type of outcome
#  - y: distribution for school-level intercepts
#  - z: distribution for school-level treatment effects



# pulled from blkvar package
#  - minimum site size: 4
#  - sd(nj) is roughly: nbar * sqrt(size_ratio)
gen_site_sizes <- function(nbar, J, size_ratio) {
  stopifnot("Average site size (nbar) must be greater than 4" = nbar > 4)

  N <- 1 + 3 * size_ratio
  p <- (N - 1)/N
  small <- rbinom(J, 1, p)
  Y <- runif(J)
  Y <- nbar * ifelse(small, Y, Y * (N - 1) + 1)

  # ensure all sites have at least 4 observations
  nj <- round(Y)
  nj[nj < 4] <- 4
  nj
}


# required parameters are those that ALWAYS need to exist
# optional parameters:
#  - site_sizes
sim_data <- function(
    outcome = c("continuous", "binary"),
    intercept_dist = c("normal"),
    effect_dist = c("normal", "gamma"),

    J = 30,

    # site size parameters
    site_sizes = NULL,
    nbar = 10,
    vary_site_sizes = F,
    size_ratio = 1/3,

    # site proportion treated parameters
    site_ps = NULL,
    pbar = 0.5,
    vary_site_ps = F,

    # intercept distribution parameters
    alpha = 0,
    sig_alpha = sqrt(0.2),

    # effect distribution parameters
    tau = 0.2,
    sig_tau = sqrt(0.2),
    rho = 0,
    a = NULL,
    b = NULL,

    # site size & prop. treated parameters
    cor_tau_n = 0,
    cor_tau_p = 0,

    # observation-level parameters
    ICC = 0.3
    ) {

  outcome <- match.arg(outcome)
  intercept_dist <- match.arg(intercept_dist)
  effect_dist <- match.arg(effect_dist)

  stopifnot("cor_tau_n must be in (-1,1)" = cor_tau_n >= -1 & cor_tau_n <= 1)
  stopifnot("cor_tau_p must be in (-1,1)" = cor_tau_p >= -1 & cor_tau_p <= 1)

  # if we've specified either site_sizes or site_ps,
  # need to ensure that J is correct before generating the other.

  # ensure J matches with manually specified site sizes/ps
  if (!is.null(site_sizes) | !is.null(site_ps)) {
    if (!is.null(site_sizes) & !is.null(site_ps)) {
      stopifnot(length(site_sizes == length(site_ps)))
    }
    J <- max(length(site_sizes), length(site_ps))
  }

  # generate raw vector of site sizes
  if (is.null(site_sizes)) {
    if (vary_site_sizes) {
      site_sizes <- gen_site_sizes(nbar, J, size_ratio)
    } else {
      site_sizes <- rep(nbar, J)
    }
  }

  # generate raw vector of site proportions treated
  if (is.null(site_ps)) {
    if (vary_site_ps) {
      ths <- min(pbar, 1 - pbar) * 0.75
      site_ps <- runif(J, pbar - ths, pbar + ths)
    } else {
      site_ps <- rep(pbar, J)
    }
  }

  fabricatr::fabricate(
    sid = add_level(
      N = J,
      alpha_j = if (intercept_dist == "normal") {
        rnorm(J, mean=alpha, sd=sig_alpha)
      } else {
        stop("Non-normal intercept distributions not currently implemented")
      },

      tau_j = if (effect_dist == "normal") {
        # generate tau_j, potentially correlated with alpha_j
        mu <- tau + rho*sig_tau/sig_alpha * (alpha_j-alpha)
        var <- (1-rho^2) * sig_tau^2
        rnorm(J, mean=mu, sd=sqrt(var))
      } else if (effect_dist == "gamma") {
        stopifnot("Gamma effect distribution requires parameter a" = !is.null(a))
        stopifnot("Gamma effect distribution requires parameter b" = !is.null(b))
        if (rho != 0) {
          warning("Correlation between intercept and effect not implemented for Gamma effect distribution!")
        }
        rgamma(J, shape=a, rate=b)
      },

      n_j = {
        if (sd(site_sizes) > 0) {
          # induce correlation between tau_j and n_j
          n_j_temp <- site_sizes[order(
            cor_tau_n^2 * site_sizes + (1-cor_tau_n^2) * rnorm(J, sd=sd(site_sizes)) )]
          # magic hack: avoids sorting tau_j
          n_j_temp[order(order(sign(cor_tau_n) * tau_j))]
        } else {
          site_sizes
        }
      },
      p_j = {
        if (sd(site_ps) > 0) {
          # induce correlation between tau_j and p_j
          p_j_temp <- site_ps[order(
            cor_tau_p^2 * site_ps + (1-cor_tau_p^2) * rnorm(J, sd=sd(site_ps)) )]
          p_j_temp[order(order(sign(cor_tau_p) * tau_j))]
        } else {
          site_ps
        }
      }
    ),

    i = add_level(
      N = n_j,
      Y0 = if (outcome == "continuous") {
        draw_normal_icc(
          mean = alpha_j,
          clusters = sid,
          total_sd = 1,
          ICC = ICC)
      } else {   # binary outcome
        draw_binary_icc(
          prob = alpha_j,
          clusters = sid,
          ICC = ICC
        )
      },
      Y1 = if (outcome == "continuous") {
        Y0 + tau_j
      } else {
        draw_binary_icc(
          prob = alpha_j + tau_j,
          clusters = sid,
          ICC = ICC
        )
      },
      Z = rbinom(n_j, size=1, p=p_j),
      Yobs = ifelse(Z, Y1, Y0)
    )
  )
}

if (F) {
  map_dbl(1:100, function(x) {
    sim_data(outcome = "continuous",
             intercept_dist = "normal",
             effect_dist = "normal") %>%
      filter(Z==0) %>%
      summarize(sd = sd(Y0)) %>%
      pull(sd)
  }) %>%
    mean()

  sim_data(outcome = "continuous",
           intercept_dist = "normal",
           effect_dist = "normal",
           site_ps = seq(0.1,0.5,by=0.1),

           alpha = 0,
           sig_alpha = 1,
           tau = 0.2,
           sig_tau = 0.1,
           rho = 0.47,

           cor_tau_p = 0.3,
           ICC = 0.2
  )
}

sim_b_nn <- function(
    site_sizes = rep(50, 10),
    alpha = 0,
    sig_alpha = 0.2,
    tau = 0.2,
    sig_tau = 0.2,
    rho = 0) {
  # simulate site-level parameters
  site_pars <- mvtnorm::rmvnorm(
    n = length(site_sizes),
    mean = c(alpha, tau),
    sigma = matrix(c(sig_alpha^2,
                     rho*sig_alpha*sig_tau,
                     rho*sig_alpha*sig_tau,
                     sig_tau^2),
                   ncol = 2)) %>%
    dplyr::as_tibble(.name_repair = "minimal")
  colnames(site_pars) <- c("alpha_j", "tau_j")

  site_pars <- site_pars %>%
    dplyr::mutate(
      sid = 1:length(site_sizes),
      .before=alpha_j)

  # simulate individual-level data
  dplyr::tibble(
    sid = rep(1:length(site_sizes), site_sizes)
  ) %>%
    dplyr::left_join(site_pars, by="sid") %>%
    dplyr::mutate(
      z = rbinom(dplyr::n(), size=1, prob=0.5),
      p = alpha_j + tau_j*z,
      p = ifelse(p <= 0, 0, p),
      p = ifelse(p >= 1, 1, p),
      y = rbinom(dplyr::n(), size=1, prob=p))

  # # make site-level summaries
  # sim %>%
  #   group_by(sid) %>%
  #   summarize(n1 = sum(z),
  #             n0 = sum(1-z),
  #             p1 = sum(z*y)/n1,
  #             p0 = sum((1-z)*y)/n0,
  #             tau_j = first(tau_j),
  #             tau_j_hat = p1-p0,
  #             se_j = sqrt(p1*(1-p1)/n1 + p0*(1-p0)/n0))
}

sim_b_ng <- function(
    site_sizes,
    alpha, sig_alpha,
    tau, b) {

  site_pars <- dplyr::tibble(
    sid = 1:length(site_sizes),
    tau_j = rgamma(length(site_sizes), shape=tau*b, rate=b),
    alpha_j = rnorm(length(site_sizes), mean=alpha, sd=sig_alpha),
  )

  # simulate individual-level data
  dplyr::tibble(
    sid = rep(1:length(site_sizes), site_sizes)
  ) %>%
    dplyr::left_join(site_pars, by="sid") %>%
    dplyr::mutate(
      p = alpha_j + tau_j*z,
      p = ifelse(p <= 0, 0, p),
      p = ifelse(p >= 1, 1, p),
      z = rbinom(dplyr::n(), size=1, prob=p),
      y = rbinom(dplyr::n(), size=1, prob=p))   # might break...

  # # make site-level summaries
  # sim %>%
  #   group_by(sid) %>%
  #   summarize(n1 = sum(z),
  #             n0 = sum(1-z),
  #             p1 = sum(z*y)/n1,
  #             p0 = sum((1-z)*y)/n0,
  #             tau_j = first(tau_j),
  #             tau_j_hat = p1-p0,
  #             se_j = sqrt(p1*(1-p1)/n1 + p0*(1-p0)/n0))
}






