# time allocation (ta) = normalized task duration (ttc)
#' @export
ta <- function(l, ttc = function(l){l}){

  # return normalized task duration
  return(ttc(l) / integrate(ttc, 0, 1)$value)

}
