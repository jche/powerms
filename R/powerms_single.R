
### est_method:
# input: site-level dataset
# output: site-level dataset, with:
#   - estimated tau_j_hat
#   - lower & upper bounds of alpha% CI


#' conduct power analysis for a single setting
#'
#' @param sim_data_args (unlisted)
#' @return individual-level dataset
#'
#' @export
#'
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

        # !!! curly-curly doesn't work with furrr !!!
        #  - see https://furrr.futureverse.org/articles/gotchas.html
        # workaround: rename cols to Z, Y, sid, use function assuming these names
        dplyr::rename(
          "Z" = dplyr::all_of(tx_var),
          "Y" = dplyr::all_of(outcome_var),
          "sid" = dplyr::all_of(site_id)
        ) %>%
        summarize_sites_fixed(se = se_method) %>%

        est_method() %>%
        dplyr::mutate(rep_id = x, .before="sid") %>%
        dplyr::rename(site_id = "sid")   # set sid back to user-specified site_id
    },
    # .progress=T,
    .options = furrr::furrr_options(seed = T)
  ) %>%
    purrr::list_rbind()

  attr(res, "sim_data_method") <- paste0(deparse(substitute(sim_data_method)), "()")
  attr(res, "est_method") <- paste0(deparse(substitute(est_method)), "()")
  attr(res, "sim_data_args") <- sim_data_args

  res
}

