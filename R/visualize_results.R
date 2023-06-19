
# functions to aid visualization of results


# given powerms() output, generate reasonable plot
moe_plot <- function(p, x_axis, grouping=NULL) {
  add_sim_params(p) %>%
    dplyr::group_by({{x_axis}}, {{grouping}}, sim_id) %>%
    dplyr::summarize(avg_moe = mean(ci_r - ci_l, na.rm=T) / 2) %>%
    ggplot2::ggplot(ggplot2::aes(x={{x_axis}}, y=avg_moe,
                                 color={{grouping}}, group={{grouping}})) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}

# note: requires unique site sizes
moe_plot_indiv <- function(p, sid=sid, grouping=NULL) {

  # # if a grouping is assigned, color by grouping
  # #  - hacky workaround to needing to use `:=` operator
  # if (!rlang::quo_is_null(rlang::enquo(grouping)) ) {
  #   a <- ggplot2::aes(x=n, y=avg_moe, group=sim_id, color=as.factor({{grouping}}))
  # } else {
  #   a <- ggplot2::aes(x=n, y=avg_moe, group=sim_id)
  # }

  # same as above, without rlang package
  #  - idea: if we can refer to `grouping` and it is NULL, there is no grouping
  #    otherwise, it's a name that neds to be passed into curly-curly
  a <- tryCatch(
    {if (is.null(grouping)) {
      ggplot2::aes(x=n, y=avg_moe, group=sim_id)
    } else {stop()}},
    error = function(e) {
      ggplot2::aes(x=n, y=avg_moe, group=sim_id, color=as.factor({{grouping}}))
    }
  )

  p %>%
    add_sim_params() %>%
    force_sid_n_match(sid={{sid}}) %>%
    dplyr::group_by(sim_id, {{grouping}}, {{sid}}) %>%
    dplyr::summarize(
      n = dplyr::first(n),
      avg_moe = mean(ci_r - ci_l, na.rm=T) / 2) %>%
    ggplot2::ggplot(a) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line("black"),
      axis.line.y = ggplot2::element_line("black")
    )
}


