# setup
setwd('/home/Cao/Storage/github/atlas.labor/')
source('tests/install.R')
source('tests/data.R')
source('R/ta.R')
source('R/Omega.R')
source('R/wtilde.R')
source('R/vstilde.R')
source('R/pec.R')
source('R/req.R')
source('R/optimin.R')

# plot data
ggplot2::qplot(T_k, geom = 'density')
ggplot2::qplot(T_q, geom = 'density')

ggplot2::qplot(h_k, geom = 'density')
ggplot2::qplot(h_q, geom = 'density')

ggplot2::qplot(u_qq, geom = 'density')
ggplot2::qplot(u_qk, geom = 'density')

# plot task duration
plot(ttc, 0, 1)

# estimate employability
wtilde(h_k = h_k, T_k = T_k, ttc = ttc, w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc, w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[1], w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[[1]], w = w, agg = T)

wtilde(h_k = h_k, T_k = T_k, ttc = ttc, w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc, w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[1], w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[[1]], w = w, agg = F)

# plot employability
wtilde(
  h_k = h_k,
  T_k = T_k,
  # ttc = list_ttc,
  ttc = ttc,
  w = w,
  agg = F
) |>
  density() |>
  plot(
    xlim = c(0,1)
  )

# estimate competitiveness
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = F)

# plot competitiveness
vstilde(
  h_q = h_q,
  T_q = T_q,
  u_qk = u_qk,
  u_qq = u_qq,
  ttc = ttc,
  w = w,
  agg = F
) |>
  density() |>
  plot(
    xlim = c(0,1)
  )

# proportional employment condition
pec_l(lmin = 0, wtilde = 1, ttc)
pec_l(lmin = 0, wtilde = 0.67, ttc)

# optimal responsability bounds vec
pec_lvec(wtilde_q, ttc)
ggplot2::qplot(pec_lvec(wtilde_q, ttc), weight= wtilde_q, geom = 'density')

# minimum required productivity kde
kde_req(pec_lvec(wtilde_q, ttc), wtilde_q, w_q = 2 ^ 19) -> dist_req
plot(dist_req)
head(dist_req$x)
tail(dist_req$x)

# labor market optimization
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

# optimize_wtilde(2, ttc) -> dsds
# optimize_wtilde(4, ttc) -> dsds
# optimize_wtilde(10, ttc) -> dsds
# optimize_wtilde(20, ttc) -> dsds
optimize_wtilde(40, ttc) -> dsds

# time to task completion function
plot(ttc)
# number of job subtypes
dsds |> length()
# relative employment levels
dsds
# relative employment levels sum to 1
dsds |> sum()
# optimal responsability bounds per job subtype
dsds |> pec_lvec(ttc)
# optimal responsability bounds' time allocation
dsds |> pec_lvec(ttc) |> Omega_vec2()
# optimal responsability bounds' time allocation integrate to 1
dsds |> pec_lvec(ttc) |> Omega_vec2() |> sum()
# optimal responsability bounds per job subtype
dsds |> pec_lvec(ttc) |> plot(ylim = c(0,1), type = 'line')
# expected value of required productivity
dsds |> pec_lvec(ttc) |> weighted.mean(dsds)

dsds |>
  pec_lvec(ttc) |>
  kde_req(dsds) |>
  plot(col = 'red')

plot(function(l){ta(l, ttc)}, add = T)
# kernel density estimation of productivity requirements distribution converges to the time allocation function as the number of job subtypes, p_q, approaches infinity.
# i.e. relative employment levels are determined by the time allocation function.
# which is the definition of the proportional employment condition.

