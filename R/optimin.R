# labor market optimization by minimizing productivity requirements
# given a stratification level p_q
#' @export
optimize_wtilde <- function(p_q, ttc = function(l){l}, w_q = 1000L){

  # assert args
  stopifnot(
    "By the Maximum Stratification Axiom (MSA), the number of job subtypes 'p_q' must be an integer between 1 and the 'w_q' number of positions in the labor market." = all(
      is.numeric(p_q),
      is.numeric(w_q),
      p_q <= w_q,
      p_q >= 1
    )
  )

  as.integer(p_q) -> p_q
  as.integer(w_q) -> w_q

  # return optimal vector of relative employment levels
  return(
    solnp(
      # starting values
      pars = rep(1 / p_q, p_q)

      # minimize expected value of required productivity given the pec
      , fun = function(wtilde_q){sum(wtilde_q * pec_lvec(wtilde_q, ttc))}

      # equality constraint: relative employment levels must sum to 1
      , eqfun = function(wtilde_q){sum(wtilde_q)}
      , eqB = 1

      # inequality constraints: relative employment levels are in the unit interval
      , ineqfun = function(wtilde_q){wtilde_q}
      , ineqLB = rep(1 / w_q, p_q) # implied by msa
      , ineqUB = rep(1 - (p_q - 1) / w_q, p_q) # implied by msa

      , LB = rep(1 / w_q, p_q) # implied by msa
      , UB = rep(1 - (p_q - 1) / w_q, p_q) # implied by msa

      # , LB = rep(0, p_q)
      # , UB = rep(1, p_q)
    )$pars
  )

}
