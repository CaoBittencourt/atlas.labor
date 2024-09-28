# aggregate time allocation
Omega <- function(lmin, lmax, ttc = function(l){l}){

  # args validation
  stopifnot("'lmin' <= 'lmax' must be responsibility bounds defined in the unit interval." = all(
    lmin[[1]] <= lmax[[1]],
    lmax[[1]] <= 1,
    lmin[[1]] >= 0
  ))

  stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # return aggregate normalized task duration (time allocation of responsibility bound = Omega)
  return(
    integrate(
      function(l){ta(l, ttc)}
      , lower = lmin
      , upper = lmax
    )$value
  )

}

# vectorized aggregate time allocation
Omega <- Vectorize(Omega, vectorize.args = c('lmin', 'lmax', 'ttc') )
