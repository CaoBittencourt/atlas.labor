# aggregate time allocation
TA <- function(lmin, lmax, ttc = function(l){l}){

  # args validation
  stopifnot("'lmin' <= 'lmax' must be responsability bounds defined in the unit interval." = all(
    lmin <= lmax,
    lmax <= 1,
    lmin >= 0
  ))

  stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # time allocation = normalized task duration
  return(integrate(ttc, lmin, lmax)$value / integrate(ttc, 0, 1)$value)

}

TA <- Vectorize(TA, 'lmax')
