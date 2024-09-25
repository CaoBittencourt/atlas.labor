# data
n <- 873
W <- 167000000
w_q <- runif(873)
w_q <- W * w_q / sum(w_q)
w_q <- ceiling(w_q)
T_kq <- runif(n)
h_kq <- runif(n)

# task duration function
ttc <- function(l){exp(l)}

# aggregate time allocation in infinitely stratified labor market
wtildeq <- function(ttc_q, Tk_q, p_q = Inf){}

TA(lmin = 0, lmax = T_kq, ttc)

integrate(ttc, 0, 1) -> dsds

dsds$value

# aggregate time allocation in maximally stratified labor market
