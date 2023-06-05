
# functions to simulate datasets
#  - input: all parameters of dataset
#  - output: individual-level dataset

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



