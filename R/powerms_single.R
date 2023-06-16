
# conduct power analysis for a single setting

### sim_data_method:
# input: sim_data_args (unlisted)
# output: individual-level dataset

### est_method:
# input: site-level dataset
# output: site-level dataset, with:
#   - estimated tau_j_hat
#   - lower & upper bounds of alpha% CI
powerms_single <- function(
    sim_data_method,
    sim_data_args,
    se_method = "pooled",
    est_method,
    tx_var = "Z",
    outcome_var = "Y",
    site_id = "sid",
    num_sims = 100,
    parallel = F) {
  stopifnot(is.function(sim_data_method))
  stopifnot(is.function(est_method))

  # hacky workaround: expand.grid() in powerms ends up nesting
  # vector parameters (e.g., site.sizes) one layer too deep in the list,
  # so we unlist them here.
  sim_data_args <- purrr::map(sim_data_args,
                              function(a) {
                                if (is.list(a)) {return(a[[1]])}
                                a
                              })

  if (exists("DEBUGGING")) { browser() }

  # if (parallel) {
  #   ncores <- future::availableCores() - 1
  #   future::plan(future::multisession, workers = ncores)
  # } else {
  #   future::plan(future::sequential)
  # }
  #
  # res <- furrr::future_map(
  #   1:num_sims,
  #   function(x) {
  #     sim_data_method %>%
  #       do.call(sim_data_args) %>%
  #       summarize_sites_fixed(se = se_method) %>%
  #       # # !!! curly-curly doesn't work with furrr !!!
  #       # summarize_sites(se = se_method,
  #       #                 tx_var = {{tx_var}},
  #       #                 outcome_var = {{outcome_var}},
  #       #                 site_id = {{site_id}}) %>%
  #       est_method() %>%
  #       dplyr::mutate(rep_id = x) %>%
  #       dplyr::select(rep_id, everything())
  #   },
  #   .progress=T,
  #   .options = furrr::furrr_options(
  #     seed = T,
  #     globals = c("sim_data_method", "sim_data_args", "se_method", "est_method"))
  # ) %>%
  #   purrr::list_rbind()

  # res <- purrr::map(
  #   1:num_sims,
  #   function(x) {
  #     sim_data_method %>%
  #       do.call(sim_data_args) %>%
  #       summarize_sites_fixed(se = se_method) %>%
  #       # summarize_sites(se = se_method,
  #       #                 tx_var = {{tx_var}},
  #       #                 outcome_var = {{outcome_var}},
  #       #                 site_id = {{site_id}}) %>%
  #       est_method() %>%
  #       dplyr::mutate(rep_id = x) %>%
  #       dplyr::select(rep_id, everything())
  #   },
  #   .progress=T
  # ) %>%
  #   purrr::list_rbind()

  if (parallel) {
    ncores <- future::availableCores() - 1
    future::plan(future::multisession, workers = ncores)
  } else {
    future::plan(future::sequential)
  }

  res <- furrr::future_map(
    1:num_sims,
    function(x) {
      sim_data_method %>%
        do.call(sim_data_args) %>%

        # curly-curly doesn't work with furrr
        # workaround: manually rename columns to Z, Y, sid
        #  and use function that assumes use of these names
        dplyr::rename(
          "Z" = dplyr::all_of(tx_var),
          "Y" = dplyr::all_of(outcome_var),
          "sid" = dplyr::all_of(site_id)
        ) %>%
        summarize_sites_fixed(se = se_method) %>%

        est_method() %>%
        dplyr::mutate(rep_id = x, .before="sid")
    },
    .progress=T,
    .options = furrr::furrr_options(seed = T)
  ) %>%
    purrr::list_rbind()

  attr(res, "sim_data_method") <- paste0(deparse(substitute(sim_data_method)), "()")
  attr(res, "est_method") <- paste0(deparse(substitute(est_method)), "()")
  attr(res, "sim_data_args") <- sim_data_args

  # if (parallel) {
  #   parallel::stopCluster(cl)
  # }

  res
}

