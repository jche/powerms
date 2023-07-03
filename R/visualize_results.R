
# functions to aid visualization of results


#' Plot results of powerms
#'
#' Given powerms() output, generate reasonable plot that shows average
#' margin of error, averaging over various simulation factors.
#'
#' @param p Results from a powerms call
#' @param x_axis Unquoted name of simulation factor to plot on x-axis, e.g., `nbar`.
#'   moe_plot() will marginalize over the remaining factors.
#' @param grouping Second factor to make a color, for multiple lines on the plot.
#'
#' @return ggplot object of desired plot
#'
#' @importFrom ggplot2 expand_limits
#'
#' @export
moe_plot <- function(p, x_axis, grouping=NULL) {
  agg <- add_sim_params(p) %>%
      dplyr::group_by({{x_axis}}, {{grouping}}) %>%
      dplyr::summarize(avg_moe = mean(ci_r - ci_l, na.rm=T) / 2)

  ggplot2::ggplot( agg, ggplot2::aes(x={{x_axis}}, y=avg_moe,
                                 color={{grouping}}, group={{grouping}})) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::expand_limits(y=0)
}


#' plot results
#'
#'  note: requires unique site sizes!!!
#'
#' @export
moe_plot_indiv <- function(p, sid=sid, grouping=NULL) {

  # # if a grouping is assigned, color by grouping
  # if (!rlang::quo_is_null(rlang::enquo(grouping)) ) {
  #   a <- ggplot2::aes(x=n, y=avg_moe, group=sim_id, color=as.factor({{grouping}}))
  # } else {
  #   a <- ggplot2::aes(x=n, y=avg_moe, group=sim_id)
  # }

  # same as above, without rlang package
  a <- tryCatch(
    if (is.null(grouping)) { ggplot2::aes(x=n, y=avg_moe) },
    error = function(e) {
      ggplot2::aes(x=n, y=avg_moe, group={{grouping}}, color=as.factor({{grouping}}))
    }
  )

  agg <- p %>%
    add_sim_params() %>%
    force_sid_n_match(sid={{sid}}) %>%
    dplyr::group_by({{grouping}}, {{sid}}) %>%
    dplyr::summarize(
      n = dplyr::first(n),
      avg_moe = mean(ci_r - ci_l, na.rm=T) / 2)

  ggplot2::ggplot(agg, a) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line("black"),
      axis.line.y = ggplot2::element_line("black")
    )
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


