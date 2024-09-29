# imports
setwd('/home/Cao/Storage/github/atlas.labor/')
source('tests/data.R')
source('R/ta.R')
source('R/Omega.R')
source('R/wtilde.R')
source('R/vstilde.R')
source('R/pec.R')
source('R/req.R')

# library(devtools)
# c(
#   'CaoBittencourt' = 'atlas.labor'
# ) -> git_pkgs
#
# Map(
#   function(git, profile){
#
#     if(!require(git, character.only = T)){
#
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
#
#     }
#
#     require(git, character.only = T)
#
#   }
#   , git = git_pkgs
#   , profile = names(git_pkgs)
# )



ggplot2::qplot(T_k, geom = 'density')
ggplot2::qplot(T_q, geom = 'density')

ggplot2::qplot(h_k, geom = 'density')
ggplot2::qplot(h_q, geom = 'density')

ggplot2::qplot(u_qq, geom = 'density')
ggplot2::qplot(u_qk, geom = 'density')

plot(ttc, 0, 1)

# employability
wtilde(h_k = h_k, T_k = T_k, ttc = ttc, w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc, w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[1], w = w, agg = T)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[[1]], w = w, agg = T)

wtilde(h_k = h_k, T_k = T_k, ttc = ttc, w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc, w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[1], w = w, agg = F)
wtilde(h_k = h_k, T_k = T_k, ttc = list_ttc[[1]], w = w, agg = F)

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

# competitiveness
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = F)

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

pec_lvec(wtilde_q, ttc)
ggplot2::qplot(pec_lvec(wtilde_q, ttc), weight= wtilde_q, geom = 'density')

# minimum required productivity distribution
dist_req(Tmin = pec_lvec(wtilde_q, ttc), w = wtilde_q)
density(pec_lvec(wtilde_q, ttc), weights = wtilde_q, from = 0, to = 1, n = sample(w, 1)) -> dsdds
