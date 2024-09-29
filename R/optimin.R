# labor market optimization by minimizing productivity requirements
# given a stratification level p_q

function(p_q, ttc = function(l){l}){

  constrOptim(
    theta = seq(0, 1, length.out = p_q) #initial guess of length p_q
    , f = function(wtilde_q){wtilde_q * pec_lvec(wtilde_q, ttc)}
    , grad = grad
    , ui = ui #for all v in {1, ..., p_q}
    , ci = ci #for all v in {1, ..., p_q}
  )
  # constrOptim(
  #   theta = theta #initial guess of length p_q
  #   # , f = function(wtilde_q){wtilde_q * pec_lvec(wtilde_q, ttc)}
  #   , f = function(wtilde_q){wtilde_q[1:p_q] * pec_lvec(wtilde_q[1:p_q], ttc)}
  #   , grad = grad
  #   , ui = ui
  #   , ci = ci
  # )

}


# optimize p_q as well (choose wtilde_q <=> lambda_q and p_q)
constrOptim()
optim()
optimize()

kde_req(
  Tmin = pec_lvec(wtilde_q, ttc)
  , wtilde_q = wtilde_q
  , w_q = w[1] / min(w)
)$x |> mean()
