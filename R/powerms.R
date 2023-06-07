
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

