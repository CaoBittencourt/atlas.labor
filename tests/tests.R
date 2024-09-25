# data
n <- 873
W <- 167000000
w <- rlnorm(873)
w <- W * w / sum(w)
w <- ceiling(w)
T_k <- runif(n)
h_k <- runif(n)

# task duration function
ttc <- function(l){exp(l)}
list_ttc <- replicate(n, ttc)

plot(ttc, 0, 1)

wtilde(h_k = h_k, T_k = T_k, list_ttc = list_ttc, w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, list_ttc = list_ttc, w = w, agg = F)

wtilde(h_k = h_k, T_k = T_k, list_ttc = list_ttc[1], w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, list_ttc = list_ttc[1], w = w, agg = F)

wtilde(
  h_k = h_k,
  T_k = T_k,
  list_ttc = list_ttc,
  w = w,
  agg = F
) |>
  density() |>
  plot()

# aggregate time allocation in maximally stratified labor market
