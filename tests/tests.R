# imports
setwd('/home/Cao/Storage/github/atlas.labor/R')
source('./ta.R')
source('./Omega.R')
source('./wtilde.R')
source('./vstilde.R')
source('./pec.R')
# source('./require.R')

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


# data
set.seed(777)

n <- 873
W <- 167000000
w <- rlnorm(873)
w <- W * w / sum(w)
w <- ceiling(w)

T_k <- rlogis(n)
T_k <- T_k - min(T_k)
T_k <- T_k / max(T_k)

h_k <- rlogis(n)
h_k <- h_k - min(h_k)
h_k <- h_k / max(h_k)

T_q <- rlogis(n)
T_q <- T_q - min(T_q)
T_q <- T_q / max(T_q)

h_q <- rlogis(n)
h_q <- h_q - min(h_q)
h_q <- h_q / max(h_q)

u_qq <- runif(n)
u_qk <- runif(n)

ggplot2::qplot(T_k, geom = 'density')
ggplot2::qplot(T_q, geom = 'density')

ggplot2::qplot(h_k, geom = 'density')
ggplot2::qplot(h_q, geom = 'density')

ggplot2::qplot(u_qq, geom = 'density')
ggplot2::qplot(u_qk, geom = 'density')

# task duration function
ttc <- function(l){exp(l)}
list_ttc <- replicate(n, ttc)
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

pec_w(0, 1, ttc)


# minimum required productivity distribution
l_bounds <- seq(0, 1, length.out = sample(seq(1, n + 1), 1))[-1]

dist_Tmin()

