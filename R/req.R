# minimum productivity requirements distribution
kde_req <- function(Tmin, wtilde_q, w_q = 1000){

  # assert args
  stopifnot(
    "'Tmin' and 'w' must be the same length." =
      c(length(Tmin) == length(w))
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
        wtilde_q[[1]] >= 0,
        sum(wtilde_q) == 1
      ))

  stopifnot(
    "'w_q' must be a non-negative number indicating the workforce size." = is.numeric(w_q)
  )

  # return kernel density estimation of productivity requirements
  return(density(Tmin, weights = w, from = 0, to = 1))

}
