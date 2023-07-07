
#' functions to produce table of site-level estimates (and standard errors),
#'
#' @param df individual-level dataset
#' @param formula formula indicating outcome, treatment, and site ID
#'   column names from dataset, in the following form:
#'   `outcome ~ treatment | site ID`. Default `Y ~ Z | sid`.
#' @param se method for computing site-level standard errors,
#'   options "pooled" or "individual"
#'
#' @return site-level dataset with estimated treatment effects `tau_hat`,
#'   standard errors `se`
#'
#' @export
summarize_sites <- function(df,
                            formula = NULL,
                            se = c("pooled", "individual")) {
  se <- match.arg(se)

  # set up treatment, outcome, and site ID variables
  if (is.null(formula)) {
    tx_var <- "Z"
    outcome_var <- "Y"
    site_id <- "sid"
  } else {
    # see https://stackoverflow.com/questions/65720598/
    tx_var <- as.character(formula[[c(3,2)]])
    outcome_var <- as.character(formula[[2]])
    site_id <- as.character(formula[[c(3,3)]])
  }

  df <- df %>%
    dplyr::rename(
      "Z" = dplyr::all_of(tx_var),
      "Y" = dplyr::all_of(outcome_var),
      "sid" = dplyr::all_of(site_id))

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

  stopifnot("Some sites have no treated units or no control units; use a different dataset" =
              sum(is.na(df_full$n0) + is.na(df_full$n1)) == 0 )

  df_full %>%
    dplyr::mutate(
      n = n1 + n0,
      tau_hat = ybar1 - ybar0,
      se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
    dplyr::select(!!site_id := sid,   # rename `sid` to site_id given by formula
                  n, tau_hat, se) %>%
    as.data.frame()
}





