# imports
library(devtools)

c(
  'CaoBittencourt' = 'atlas.labor'
) -> git_pkgs

Map(
  function(git, profile){

    if(!require(git, character.only = T)){

      install_github(
        paste0(profile, '/', git)
        , upgrade = F
        , force = T
      )

    }

    require(git, character.only = T)

  }
  , git = git_pkgs
  , profile = names(git_pkgs)
)


# data
n <- 873
W <- 167000000
w <- rlnorm(873)
w <- W * w / sum(w)
w <- ceiling(w)
T_k <- runif(n)
h_k <- runif(n)

T_q <- runif(n)
h_q <- runif(n)
u_qq <- runif(n)
u_qk <- runif(n)

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
  ttc = list_ttc,
  w = w,
  agg = F
) |>
  density() |>
  plot()

# competitiveness
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = ttc, w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[[1]], w = w, agg = F)

vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = T)
vstilde(h_q = h_q, T_q = T_q, u_qk = u_qk, u_qq = u_qq, ttc = list_ttc[1], w = w, agg = F)
