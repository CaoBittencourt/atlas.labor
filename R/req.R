# minimum productivity requirements distribution
#' @export
kde_req <- function(Tmin, wtilde_q, w_q = 1024){

  # assert args
  stopifnot(
    "'Tmin' and 'wtilde_q' must be the same length." =
      c(length(Tmin) == length(wtilde_q))
  )

  stopifnot(
    "'Tmin' must be a numeric vector of minimum productivity requirements in the unit interval." =
      all(
        is.numeric(Tmin),
        Tmin[[1]] <= 1,
        Tmin[[1]] >= 0
      ))

  stopifnot(
    "'wtilde_q' must be a non-negative numeric vector of relative employment levels." =
      all(
        is.numeric(wtilde_q),
        wtilde_q[[1]] <= 1,
        wtilde_q[[1]] >= 0
        # , sum(wtilde_q) == 1
      ))

  stopifnot(
    "'w_q' must be a non-negative number indicating the workforce size." = is.numeric(w_q)
  )

  # return kernel density estimation of productivity requirements
  return(density(Tmin, weights = wtilde_q, from = 0, to = 1, n = w_q))

}
