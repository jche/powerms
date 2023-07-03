



#' @title Result object for results of powerms call
#'
#' @name powermsresult
#'
#' @description
#' The powermsresult object is an S3 class that holds the results from `powerms()`.
#'
#' It has several methods that pull different information from this object, and
#' some printing methods for getting nicely formatted results.
#'
#'
#' @param x a powermsresult object
#' (except for is.powermsresult, where it is a generic object to check).
#'
#' @rdname powermsresult
NULL


#' @return The simulation parameters of a given simulation
#'
#' @rdname powermsresult
#' @export
sim_params <- function( x ) {
  stopifnot( is.powermsresult(x) )

  attr( x, "sim_params" )
}



#' @return is.powermsresult: TRUE if object is a powermsresult object.
#'
#' @rdname powermsresult
#'
#' @export
is.powermsresult <- function(x) {
  inherits(x, "powermsresult")
}


scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}



#' @title Pretty print powerms result
#'
#' @param ... extra options passed.
#' @rdname powermsresult
#'
#' @return print: No return value; prints results.
#'
#' @export
print.powermsresult <- function(x, ...)
{

  ss = sim_params(x)

  reps = length( unique( x$rep_id ) )

  scat( "Multi-site power results\n\tSimulations: %d\n\tScenarios: %d\n\tReps / Sim: %d\n",
        reps * nrow(ss),
        nrow(ss),
        reps )

  if (!is.null(ss$J)) {
    scat( "\tAvg number of sites: %.1f (%d -- %d)\n",
          mean( ss$J ), min(ss$J), max(ss$J) )
  }

  scat( "\nSample data-generating parameters:\n" )
  print( head( ss, 5 ), row.names = FALSE )

  scat( "\nSample simulation results:\n" )
  print( head( x, 5 ), row.names = FALSE )

  invisible( x )
}




#' @title Get top few rows of simulation
#'
#' @description
#' Calculate some summary statistics on the result object.
#'
#' @param x object to take rows from
#' @param n Number of rows to take
#' @param ... extra options passed
#' @rdname powermsresult
#'
#' @return Data.frame of top rows
#'
#' @export
head.powermsresult <- function( x, n = 6L, ... ) {
  head( as.data.frame( x, n = n, ... ) )
}





#' Add simulation parameters to results of powerms
#'
#' @param p A powerms_result object.
#'
#' @return The result as a dataframe with new columns corresponding to
#'   the simulation factors (previously stored as an attributed).
#'
#' @export
add_sim_params <- function(p) {
  p %>%
    dplyr::left_join(attr(p, "sim_params"),
                     by = "sim_id")
}





#' @title Pretty print powerms result
#'
#' @description
#' Calculate some summary statistics on the result object.
#'
#' @param object object to summarize.
#' @param ... extra options passed to print.powermsresult
#' @rdname powermsresult
#'
#' @return summary: No return value; prints results.
#'
#' @export
summary.powermsresult <- function(object, ...) {


  ss = sim_params(object)

  result <- sapply(ss, function(column) length(unique(column)) > 1)
  result = which( result )

  factors = names(result) #setdiff( names(result), c( "sim_id" ) )

  sums <- add_sim_params(object) %>%
    group_by( across( all_of( factors ) ) ) %>%
    summarise( mean_se = mean( se_j ),
               .groups =)

  print( sums )


  invisible( sums )
}


