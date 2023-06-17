
# functions to produce table of site-level estimates (and standard errors),
#  given an individual-level dataset
#  - input: individual-level dataset
#  - output: input dataset, with:
#     - site-level ests
#     - site-level ses
#     - site-level alpha% interval estimates

# input df: needs sid, z, y
# output df: sid, tau_hat, se
# TODO: confirm that this works fine with binary outcomes
summarize_sites <- function(df,
                            tx_var,
                            outcome_var,
                            site_id,
                            se=c("pooled", "individual")) {
  se <- match.arg(se)

  # output: tibble with J rows
  #  - cols: sid & (ybar, n, sd) for tx/co units
  if (se == "pooled") {
    sds <- df %>%
      dplyr::group_by({{site_id}}, {{tx_var}}) %>%
      dplyr::mutate({{outcome_var}} := {{outcome_var}}-mean({{outcome_var}})) %>%
      dplyr::ungroup() %>%
      dplyr::group_by({{tx_var}}) %>%
      dplyr::summarize(sigma = sd({{outcome_var}}))
    sd1 <- sds %>%
      dplyr::filter({{tx_var}}==1) %>%
      dplyr::pull(sigma)
    sd0 <- sds %>%
      dplyr::filter({{tx_var}}==0) %>%
      dplyr::pull(sigma)

    df_full <- df %>%
      dplyr::group_by({{site_id}}, {{tx_var}}) %>%
      dplyr::summarize(
        ybar = mean({{outcome_var}}),
        n = dplyr::n(),
        .groups = "drop_last") %>%
      tidyr::pivot_wider(
        names_from = {{tx_var}},
        values_from = c("ybar", "n"),
        names_sep = "") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sd1 = sd1, sd0 = sd0)

  } else if (se == "individual") {
    df_full <- df %>%
      dplyr::group_by({{site_id}}, {{tx_var}}) %>%
      dplyr::summarize(
        ybar = mean({{outcome_var}}),
        n = dplyr::n(),
        sd = sd({{outcome_var}})) %>%
      tidyr::pivot_wider(
        names_from = {{tx_var}},
        values_from = c("ybar", "n", "sd"),
        names_sep = "") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tau_hat = ybar1 - ybar0,
        se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
      dplyr::select({{site_id}}, tau_hat, se)
  }

  df_full %>%
    dplyr::mutate(
      n = n1 + n0,
      tau_hat = ybar1 - ybar0,
      se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
    dplyr::select({{site_id}}, n, tau_hat, se)
}

# same as summarize_sites, but uses:
#  - site id sid
#  - treatment Z
#  - outcome Y
summarize_sites_fixed <- function(df, se=c("pooled", "individual")) {
  se <- match.arg(se)

  # output: tibble with J rows
  #  - cols: sid & (ybar, n, sd) for tx/co units
  if (se == "pooled") {
    sds <- df %>%
      dplyr::group_by(sid, Z) %>%
      dplyr::mutate(Y = Y-mean(Y)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Z) %>%
      dplyr::summarize(sigma = sd(Y))
    sd1 <- sds %>%
      dplyr::filter(Z==1) %>%
      dplyr::pull(sigma)
    sd0 <- sds %>%
      dplyr::filter(Z==0) %>%
      dplyr::pull(sigma)

    df_full <- df %>%
      dplyr::group_by(sid, Z) %>%
      dplyr::summarize(
        ybar = mean(Y),
        n = dplyr::n(),
        .groups = "drop_last") %>%
      tidyr::pivot_wider(
        names_from = Z,
        values_from = c("ybar", "n"),
        names_sep = "") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(sd1 = sd1, sd0 = sd0)

  } else if (se == "individual") {
    df_full <- df %>%
      dplyr::group_by(sid, Z) %>%
      dplyr::summarize(
        ybar = mean(Y),
        n = dplyr::n(),
        sd = sd(Y)) %>%
      tidyr::pivot_wider(
        names_from = Z,
        values_from = c("ybar", "n", "sd"),
        names_sep = "") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tau_hat = ybar1 - ybar0,
        se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
      dplyr::select(sid, tau_hat, se)
  }

  df_full %>%
    dplyr::mutate(
      n = n1 + n0,
      tau_hat = ybar1 - ybar0,
      se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
    dplyr::select(sid, n, tau_hat, se)
}


run_t_test <- function(sdat, alpha=0.05) {
  stopifnot(dplyr::between(alpha, 0, 1))
  sdat %>%
    dplyr::mutate(
      ci_l = tau_hat + qnorm(alpha) * se,
      ci_r = tau_hat + qnorm(1-alpha) * se)
}

run_mlm <- function(sdat, alpha=0.05) {
  # make dataset for bayesian models
  stan_list <- list(
    J = nrow(sdat),
    tau_j_hat = sdat$tau_hat,
    se_j = sdat$se)

  # fit model
  fit_norm <- rstan::sampling(
    stanmodels$normal_mlm,
    data = stan_list,
    iter = 2000,
    chains = 4,
    # control = list(max_treedepth = 12,
    #               adapt_delta = 0.95),
    verbose = F,
    show_messages = F,
    refresh = 0)

  # https://discourse.mc-stan.org/t/divergent-transitions-a-primer/17099
  if (rstan::get_num_divergent(fit_norm) > 0) {
    browser()

    shinystan::launch_shinystan(fit_norm)

    rstan::stan_diag(fit_norm)
    print(fit_norm)

    bayesplot::mcmc_pairs(fit_norm, pars=c("tau", "sig_tau"))
  }


  samples_norm <- rstan::extract(fit_norm)
  site_effects_norm <- samples_norm$tau_j

  sdat %>%
    dplyr::select(sid, n, tau_hat) %>%
    dplyr::mutate(
      tau_j_hat = apply(site_effects_norm, 2, mean),
      se_j = apply(site_effects_norm, 2, sd),
      ci_l = apply(site_effects_norm, 2, function(x) quantile(x, alpha)),
      ci_r = apply(site_effects_norm, 2, function(x) quantile(x, 1-alpha)))
}

