# employability
wtilde <- function(h_k, T_k, ttc, w, p = Inf, agg = T){

  if(is.function(ttc)){

    list(ttc) -> ttc

  }

  # employability in infinitely stratified labor market
  if(p == Inf){

    h_k * Omega(0, T_k, ttc) -> employability

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
