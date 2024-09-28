# proportional employment condition (pec)
# given the pec and a vector of employment levels w, determine optimal responsability bounds l
pec_l <- function(w, ttc){

  # assert args
  stopifnot("'w' must be a non-negative numeric vector of employment levels.") = all(
    is.numeric(w),
    w >= 0
  )

  # # solve pec for l
  # pec_solve_l <- function(w,){
  #
  #   # wtilde
  #   w <- w / sum(w)
  #
  #
  #
  # }

  # responsability bounds are defined in the unit interval
  l <- 0

  for 1:length(w) - 1

  integrate(ttc, from = l, to = l)

  -> lbounds

  # last job subtype is by defintion perfectly qualified
  c(lbounds, 1) -> lbounds


  # # apply pec
  # sapply(
  #   1:length(w)
  # )



}

# # given the pec and a vector of responsability bounds l, determine optimal employment levels w
# pec_w
