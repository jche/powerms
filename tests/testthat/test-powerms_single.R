test_that("function runs", {
  foo <- powerms_single(
    sim_data_method = sim_data,
    sim_data_args = list(),
    se_method = "pooled",
    est_method = run_t_test,
    # est_method = run_mlm,
    num_sims = 10
  )

  expect_equal(nrow(foo), 300)
  expect_type(foo %>%
                dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
                dplyr::pull(avg_moe),
              "double")
})
