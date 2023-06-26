
#' functions to produce table of site-level estimates (and standard errors),
#'
#' @param df  an individual-level dataset.
#'
#'  @return  - input: individual-level dataset
#  - output: input dataset, with:
#     - site-level ests
#     - site-level ses
#     - site-level alpha% interval estimates

#' Summarize sites
#'
#' input df: needs sid, z, y
#' output df: sid, tau_hat, se
#'
#' TODO: confirm that this works fine with binary outcomes
#'
#' @export
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

  stopifnot("Some sites have no treated units or no control units; use a different dataset" =
              sum(is.na(df_full$n0) + is.na(df_full$n1)) == 0 )

  df_full %>%
    dplyr::mutate(
      n = n1 + n0,
      tau_hat = ybar1 - ybar0,
      se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
    dplyr::select({{site_id}}, n, tau_hat, se)
}


#' same as summarize_sites, but uses:
#'  - site id sid
#'  - treatment Z
#'  - outcome Y
#'
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

  stopifnot("Some sites have no treated units or no control units; use a different dataset" =
              sum(is.na(df_full$n0) + is.na(df_full$n1)) == 0 )

  df_full %>%
    dplyr::mutate(
      n = n1 + n0,
      tau_hat = ybar1 - ybar0,
      se = sqrt(sd1^2 / n1 + sd0^2 / n0)) %>%
    dplyr::select(sid, n, tau_hat, se)
}





