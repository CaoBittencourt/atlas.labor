# proportional employment condition (pec)
# given the pec and a vector of employment levels w, determine optimal responsibility bounds l
pec_l <- function(
    lmin = 0,
    wtilde = 1,
    ttc = function(l){l}
){

  # assert args
  stopifnot(
    "'lmin' must be a lower responsability bound in the unit interval" =
      all(
        is.numeric(lmin)
        , lmin >= 0
        , lmin < 1
      )
  )

  stopifnot(
    "'wtilde' must be a relative employment level in the unit interval" =
      all(
        is.numeric(wtilde)
        , lmin >= 0
        , lmin <= 1
      )
  )

  stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # solve integral equation for lmax
  uniroot(
    function(lmax){

      integrate(
        function(l){ta(l, ttc)}
        , lower = lmin
        , upper = lmax
      )$value - wtilde

    }
    , interval = c(-0.1,1.1)
  )$root -> lmax

  pmax(lmax, 0) -> lmax
  pmin(lmax, 1) -> lmax

  # return optimal responsibility upper bound
  return(lmax)

}

# given the pec and a vector of responsibility bounds l, determine optimal employment levels w
# by the proportional employment condition, relative employment levels for all job subtypes are the aggregate time allocation of their respective tasks
pec_w <- Omega
