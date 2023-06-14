
# functions to aid visualization of results


# given powerms() output, generate reasonable plot
moe_plot <- function(p, x_axis, grouping=NULL) {
  add_sim_params(p) %>%
    dplyr::group_by({{x_axis}}, {{grouping}}, sim_id) %>%
    dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
    ggplot2::ggplot(ggplot2::aes(x={{x_axis}}, y=avg_moe,
                                 color={{grouping}}, group={{grouping}})) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}


