# minimum productivity requirements distribution
dist_Tmin <- function(Tmin, w, absolute = T){

  # assert args
  stopifnot("'Tmin' and 'w' must be the same length.") = c(length(Tmin) == length(w))

  stopifnot("'Tmin' must be a numeric vector of minimum productivity requirements in the unit interval.") = all(
    is.numeric(Tmin),
    Tmin[[1]] <= 1,
    Tmin[[1]] >= 0
  )

  stopifnot("'w' must be a non-negative numeric vector of employment levels.") = all(
    is.numeric(w),
    w >= 0
  )

  if(absolute){

    w <- ceiling(w)

    # minimum productivity distribution
    rep(Tmin, times = w) ->

  }

}
