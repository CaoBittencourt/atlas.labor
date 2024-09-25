# aggregate time allocation
Omega <- function(lmin, lmax, ttc = function(l){l}){

  # args validation
  stopifnot("'lmin' <= 'lmax' must be responsability bounds defined in the unit interval." = all(
    lmin[[1]] <= lmax[[1]],
    lmax[[1]] <= 1,
    lmin[[1]] >= 0
  ))

  stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # return aggregate normalized task duration (time allocation)
  return(integrate(ttc, lmin[[1]], lmax[[1]])$value / integrate(ttc, 0, 1)$value)

}

# vectorized aggregate time allocation
Omega <- Vectorize(Omega, vectorize.args = c('lmin', 'lmax', 'ttc') )
