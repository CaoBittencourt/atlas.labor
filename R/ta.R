# time allocation (ta) = normalized task duration (ttc)
ta <- function(l, ttc){

  # return normalized task duration
  return(ttc(l) / integrate(ttc, 0, 1)$value)

}
