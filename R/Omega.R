# aggregate time allocation
Omega <- function(lmin = 0, lmax = 1, ttc = function(l){l}){

  # # args validation
  # stopifnot("'lmin' <= 'lmax' must be responsibility bounds defined in the unit interval." = all(
  #   lmin[[1]] <= lmax[[1]],
  #   lmax[[1]] <= 1,
  #   lmin[[1]] >= 0
  # ))
  #
  # stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

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
#' @export
Omega_vec <- Vectorize(Omega, vectorize.args = c('lmin', 'lmax', 'ttc'))

# vectorized aggregate time allocation
#' @export
Omega_vec2 <- function(lbounds, ttc = function(l){l}){

  # assert args

  if(lbounds[1] != 0){

    c(0, lbounds) -> lbounds

  }

  if(lbounds[length(lbounds)] != 1){

    c(lbounds, 1) -> lbounds

  }

  # return vector of aggregate time allocations
  return(
    mapply(
      function(l_min, l_max){

        Omega(l_min, l_max, ttc)

      }
      , l_min = lbounds[-length(lbounds)]
      , l_max = lbounds[-1]
    )
  )

}
