
summary_powms <- function(p) {
  sprintf("Average margin of error: %s",
          p %>%
            dplyr::summarize(avg_moe = mean(ci_r-ci_l)) %>%
            round(3))
}
