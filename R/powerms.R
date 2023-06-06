
# conduct power analyses for a user-specified grid of settings

# sim_data_args is list of all settings to gridsearch over
powerms <- function(
    sim_data_method,
    sim_data_args,
    se_method = "pooled",
    est_method,
    tx_var = z,
    outcome_var = y,
    site_id = sid,
    num_sims = 100) {
  stopifnot(is.function(sim_data_method))
  stopifnot(is.function(est_method))

  args_df <- expand.grid(sim_data_args)

  res_list <- purrr::map(
    1:nrow(args_df),
    function(i) {
      powerms_single(
        sim_data_method = sim_data_method,
        sim_data_args = as.list(args_df[i,]),
        se_method = se_method,
        est_method = est_method,
        tx_var = {{tx_var}},
        outcome_var = {{outcome_var}},
        site_id = {{site_id}},
        num_sims = num_sims) %>%
        dplyr::mutate(sim_id = i, .before = rep_id)
    })

  res <- purrr::list_rbind(res_list)
  attr(res, "sim_params") <- args_df %>%
    dplyr::mutate(sim_id = 1:dplyr::n(), .before=dplyr::everything())

  res
}


if (F) {
  library(dplyr)
  foo <- powerms(
    sim_data_method = blkvar::generate_multilevel_data,
    sim_data_args = list(
      n.bar = c(25, 50),
      J = 25,
      p = 0.5,
      tau.11.star = 0.3,   # Total amount of cross-site treatment variation
      rho2.0W = 0.1,  # Explanatory power of W for control outcomes
      rho2.1W = 0.5,  # Explanatory power of W for average treatment impact
      ICC = 0.7,   # ICC; 1-sigma2.e
      gamma.00 = 0,
      gamma.10 = 0.2,
      zero.corr = F,
      variable.n = T),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    tx_var = Z,
    outcome_var = Yobs,
    site_id = sid,
    num_sims = 10
  )

  foo
  attr(foo, "sim_params")
  add_sim_params(foo)
}
