
# pulled from blkvar package
#  - minimum site size: 4
#  - sd(nj) is roughly: nbar * sqrt(size_ratio)
gen_site_sizes <- function(nbar, J, size_ratio=1/3, vary=T) {
  stopifnot("Average site size (nbar) must be greater than 4" = nbar > 4)

  if (vary) {
    N <- 1 + 3 * size_ratio
    p <- (N - 1)/N
    small <- rbinom(J, 1, p)
    Y <- runif(J)
    Y <- nbar * ifelse(small, Y, Y * (N - 1) + 1)

    # ensure all sites have at least 4 observations
    nj <- round(Y)
    nj[nj < 4] <- 4
  } else {
    nj <- rep(nbar, J)
  }
  nj
}

gen_site_ps <- function(pbar, J, p_ratio=0.75, vary=T) {
  stopifnot(0 <= pbar & pbar <= 1)
  if (vary) {
    ths <- min(pbar, 1 - pbar) * p_ratio
    site_ps <- runif(J, pbar - ths, pbar + ths)
  } else {
    site_ps <- rep(pbar, J)
  }
  site_ps
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






