# competitiveness
vstilde <- function(h_q, T_q, u_qk, u_qq, ttc, w, p = Inf, agg = T){

  # competitiveness in infinitely stratified labor market
  if(p == Inf){

    (u_qk >= u_qq) * h_q * Omega(0, T_q, list(ttc)) -> competitiveness

  }

  # aggregate competitiveness
  if(agg){

    weighted.mean(
      x = competitiveness,
      w = w
    ) -> competitiveness

  }

  # return competitiveness
  return(competitiveness)

}
