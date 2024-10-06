# labor market optimization by minimizing productivity requirements
# given a stratification level p_q

# install.packages('cubature')
# install.packages('Rsolnp')
# # install.packages('pracma') #chebApprox() # can greatly reduce optim time
library(cubature)
library(Rsolnp)

ttc <- function(l){exp(l)}
ttc <- function(l){l}
ttc <- function(l){l ^ (1 / l)}
ttc <- function(l){l ^ 2}
ttc <- function(l){sqrt(l)}
ttc <- function(l){l ^ (1/4)}
ttc <- function(l){-l^2 + l}

# ttc <- function(l){l ^ l}
# ttc <- function(l){1 - l}
plot(ttc)

optimize_wtilde <- function(p_q, ttc = function(l){l}, w_q = 1000){

  return(
    solnp(
      # starting values = wtilde in maximally stratified labor market
      pars = rep(1 / p_q, p_q)

      # minimize expected value of required productivity given the pec
      , fun = function(wtilde_q){sum(wtilde_q * pec_lvec(wtilde_q, ttc))}

      # equality constraint: relative employment levels must sum to 1
      , eqfun = function(wtilde_q){sum(wtilde_q)}
      , eqB = 1

      # inequality constraints: relative employment levels are in the unit interval
      , ineqfun = function(wtilde_q){wtilde_q}
      , ineqLB = rep(1 / w_q, p_q)
      , ineqUB = rep(1 - 1 / w_q, p_q)
      , LB = rep(0, p_q)
      , UB = rep(1, p_q)
    )$pars
  )

}

# optimize_wtilde(4, ttc) -> dsds
# optimize_wtilde(50, ttc) -> dsds
# optimize_wtilde(10, ttc) -> dsds
optimize_wtilde(50, ttc) -> dsds

dsds #relative employment levels
dsds |> length() #number of job subtypes
dsds |> sum() #relative employment levels sum to 1
dsds |> pec_lvec(ttc) #optimal responsability bounds per job subtype
dsds |> pec_lvec(ttc) |> plot(ylim = c(0,1)) #optimal responsability bounds per job subtype
dsds |> pec_lvec(ttc) |> weighted.mean(dsds) #expected value of required productivity

dsds |>
  pec_lvec(ttc) |>
  kde_req(dsds) |>
  plot(col = 'red')

plot(function(l){ta(l, ttc)}, add = T)
# kernel density estimation of productivity requirements distribution converges to the time allocation function as the number of job subtypes, p_q, approaches infinity.
# i.e. relative employment levels are determined by the time allocation function.
# which is the definition of the proportional employment condition.
