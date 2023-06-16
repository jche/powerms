
# conduct power analyses for a user-specified grid of settings

# NOTE: to use powerms locally while developing, need to devtools::install()
#  the full package! (furrr doesn't play nicely, some globals issue...)
#  - see https://github.com/DavisVaughan/furrr/issues/95

# sim_data_args is list of all settings to gridsearch over
powerms <- function(
    sim_data_method,
    se_method = "pooled",
    est_method,
    tx_var = "Z",
    outcome_var = "Y",
    site_id = "sid",
    num_sims = 100,
    parallel = F,
    ...) {
  stopifnot(is.function(sim_data_method))
  stopifnot(is.function(est_method))

  sim_data_args <- list(...)

  # check if user manually specified args_df
  if ("args_df" %in% names(sim_data_args)) {
    args_df <- sim_data_args$args_df
  } else {
    args_df <- expand.grid(sim_data_args, stringsAsFactors = F)
  }

  # if no arguments are supplied in sim_data_args,
  # manually generate args_df using default arguments to sim_data_method
  if (nrow(args_df) == 0) {
    args_df <- formals(sim_data_method) %>%
      as.list() %>%
      purrr::map(function(x) {
        if (is.call(x)) {
          # for vector-valued default arguments,
          # only grab first (default) element of vector as argument
          eval(x)[1]
        } else {
          x
        }
      }) %>%
      purrr::discard(is.null) %>%
      as.data.frame()
  }

  if (exists("DEBUGGING")) { browser() }

  res_list <- purrr::map(
    1:nrow(args_df),
    function(i) {
      powerms_single(
        sim_data_method = sim_data_method,
        sim_data_args = as.list(args_df[i,]),
        se_method = se_method,
        est_method = est_method,
        tx_var = tx_var,
        outcome_var = outcome_var,
        site_id = site_id,
        num_sims = num_sims,
        parallel = parallel) %>%
        dplyr::mutate(sim_id = i, .before = rep_id)
    },
    .progress = paste("Running", nrow(args_df), "simulation settings..."))

  res <- purrr::list_rbind(res_list)

  attr(res, "sim_params") <- args_df %>%
    dplyr::mutate(sim_id = 1:dplyr::n(), .before=dplyr::everything())

  res
}

