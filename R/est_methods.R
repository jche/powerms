

#' Method of analysis
#'
#' These functions are the planned analysis functions used by the
#' power simulator.
#'
#' @name estimation_methods
#'
#' @param sdat Site-statistics fit to a multisite dataset to analyze
#'   (as dataframe, one row per site)
#'
NULL




#' Individual t-test method for analysis
#'
#' @rdname estimation_methods
#' @export
run_t_test <- function(sdat, alpha=0.05) {
  stopifnot(dplyr::between(alpha, 0, 1))

  sdat %>%
    dplyr::mutate(
      ci_l = tau_hat + qnorm(alpha) * se,
      ci_r = tau_hat + qnorm(1-alpha) * se)
}




#' Multilevel modeling for analysis
#'
#' @rdname estimation_methods
#'
#' @export
run_mlm <- function(sdat, alpha=0.05, ncp=T) {
  # make dataset for bayesian models
  stan_list <- list(
    J = nrow(sdat),
    tau_j_hat = sdat$tau_hat,
    se_j = sdat$se)

  # fit normal model (non-centered parameterization or centered parameterization)
  if (ncp) {
    stan_model <- stanmodels$normal_mlm
  } else {
    stan_model <- stanmodels$normal_mlm_centered
  }
  fit_norm <- rstan::sampling(
    stan_model,
    data = stan_list,
    iter = 2000,
    chains = 4,
    # control = list(max_treedepth = 12,
    #               adapt_delta = 0.95),
    verbose = F,
    show_messages = F,
    refresh = 0)

  # # https://discourse.mc-stan.org/t/divergent-transitions-a-primer/17099
  # if (rstan::get_num_divergent(fit_norm) > 0) {
  #   browser()
  #
  #   shinystan::launch_shinystan(fit_norm)
  #
  #   rstan::stan_diag(fit_norm)
  #   print(fit_norm)
  #
  #   bayesplot::mcmc_pairs(fit_norm, pars=c("tau", "sig_tau"))
  # }

  samples_norm <- rstan::extract(fit_norm)
  site_effects_norm <- samples_norm$tau_j

  sdat %>%
    dplyr::select(-se) %>%
    dplyr::mutate(
      tau_j_hat = apply(site_effects_norm, 2, mean),
      se_j = apply(site_effects_norm, 2, sd),
      ci_l = apply(site_effects_norm, 2, function(x) quantile(x, alpha)),
      ci_r = apply(site_effects_norm, 2, function(x) quantile(x, 1-alpha)))
}

