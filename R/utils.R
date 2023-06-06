
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
  p_list <- split(p, ~sim_id)
  attr(p, "sim_params") %>%
    dplyr::mutate(
      res = p_list,
      .before = dplyr::everything()) %>%
    dplyr::select(-sim_id) %>%
    tidyr::unnest(res)
}


