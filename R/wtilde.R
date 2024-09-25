# employability
wtilde <- function(h_k, T_k, list_ttc, w, p = Inf, agg = T){

  # employability in infinitely stratified labor market
  if(p == Inf){

    h_k * Omega(0, T_k, list_ttc) -> employability

  }

  # aggregate employability
  if(agg){

    weighted.mean(
      x = employability,
      w = w
    ) -> employability

  }

  # return employability
  return(employability)

}

# # employability in infinitely stratified labor market
# tildew_inf <- function(Tk_q, hk_q, ttc_q){
#
#   # args validation
#   stopifnot("'Tk_q' must be numeric and defined in the unit interval." = all(is.numeric(Tk_q), Tk_q <= 1, Tk_q >= 0))
#   stopifnot("'hk_q' must be numeric and defined in the unit interval." = all(is.numeric(hk_q), hk_q <= 1, hk_q >= 0))
#   stopifnot("'ttc_q' must be a task duration function defined in the unit interval." = is.function(ttc_q))
#
#   # return employability
#   return(hk_q[[1]] * TA(0, Tk_q[[1]], ttc_q))
#
# }
#
# tildew_inf <- function(Tk, hk, ttc){
#
#   # args validation
#   stopifnot("All arguments must be the same length." = all(lengths(list(Tk, hk, ttc)) == min(lengths(list(Tk, hk, ttc)))))
#
#   vec_tildew_inf <- Vectorize(tildew_inf, vectorize.args = c('Tk_q', 'hk_q', 'ttc_q'))
#
#   # return employability
#   return(hk_q[[1]] * TA(0, Tk_q[[1]], ttc_q))
#
# }
