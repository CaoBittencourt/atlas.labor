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

# task duration function
ttc <- function(l){exp(l)}
list_ttc <- replicate(n, ttc)

wtilde_q <- c(rlnorm(7), rlogis(7), runif(7))
wtilde_q <- wtilde_q - min(wtilde_q)
wtilde_q <- wtilde_q / sum(wtilde_q)