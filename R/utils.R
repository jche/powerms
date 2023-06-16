
# input: powerms_single() output
# output: useful results and description of powerms_single() run
summary_powerms_single <- function(p) {
  cat(paste(
    "Average margin of error:",
    p %>%
      dplyr::summarize(avg_moe = mean(ci_r-ci_l)) %>%
      round(3),
    "\n"))
  cat(paste(
    "Method for data simulation:",
    attr(p, "sim_data_method"),
    "\n"
  ))
  cat(paste(
    "Method for data analysis:",
    attr(p, "est_method"),
    "\n"
  ))
  cat(paste(
    "Data-generating parameters: \n ",
    paste(names(attr(p, "sim_data_args")),
          attr(p, "sim_data_args"),
          sep=": ", collapse="\n  ")))
}



# input: powerms() output
# output: output with simulation settings as columns
add_sim_params <- function(p) {
  p %>%
    dplyr::left_join(attr(p, "sim_params"),
                     by = "sim_id")
}




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

# helper function: forces each sid to correspond to single site size
#  assumes a bunch of things, e.g., distinct n for each site,
#   same n values for all reps and sims, etc.
force_sid_n_match <- function(p, sid=sid) {
  sid_key <- p %>%
    dplyr::filter(sim_id == 1, rep_id == 1) %>%
    dplyr::select({{sid}}, n)

  p %>%
    dplyr::select(-{{sid}}) %>%
    dplyr::left_join(sid_key, by="n") %>%
    dplyr::relocate({{sid}}, .after="rep_id") %>%
    dplyr::group_by(sim_id, rep_id) %>%
    dplyr::arrange({{sid}}, .by_group=T) %>%
    dplyr::ungroup()
}






