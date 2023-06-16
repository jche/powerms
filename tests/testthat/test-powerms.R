test_that("powerms function runs", {
  foo <- powerms(
    sim_data_method = sim_data,
    se_method = "pooled",
    est_method = run_t_test,
    num_sims = 2)

  expect_equal(length(unique(foo$rep_id)), 2)
  expect_equal(length(unique(foo$sim_id)), 1)
  expect_type(foo %>%
                dplyr::summarize(avg_moe = mean(ci_r - ci_l)) %>%
                dplyr::pull(avg_moe),
              "double")
})
