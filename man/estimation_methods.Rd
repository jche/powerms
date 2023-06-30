% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_methods.R
\name{estimation_methods}
\alias{estimation_methods}
\alias{run_t_test}
\alias{run_mlm}
\title{Method of analysis}
\usage{
run_t_test(sdat, alpha = 0.05)

run_mlm(sdat, alpha = 0.05, ncp = T)
}
\arguments{
\item{sdat}{Site-statistics fit to a multisite dataset to analyze
(as dataframe, one row per site)}
}
\description{
These functions are the planned analysis functions used by the
power simulator.
}