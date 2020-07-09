sim_data <- function(n, m, sigma2, delta, K = 2, gamma = 2) {
  s <- seq(0,10,length.out = m)
  meanFunct <- function(s) s + 10*exp(-(s-5)^2)

  efnfn <- function(K) {
    function(s) {
      (K %% 2)*cos(2*s*pi/10/ceiling(K/2))/ sqrt(5) +
        ((K+1) %% 2)*(-sin(2*s*pi/10/ceiling(K/2)))/ sqrt(5)
    }
  }

  efns <- purrr::map(1:K, efnfn)

  efn_mat <- dplyr::bind_cols(purrr::map(efns, ~.(s)))

  Ksi <- matrix(rnorm(n*K), ncol=K);
  Ksi <- apply(Ksi, 2, scale)
  Ksi <- Ksi %*% diag(1/(1:K)^gamma)*5

  # Create Y_true
  yTrue <- Ksi %*% t(efn_mat) + t(matrix(rep(meanFunct(s),n), nrow=m)) + rnorm(n*m, sd = sigma2) %>% matrix(n, m)

  ds <- tibble(
    id = rep(1:n, each=m),
    trt = rep(1:0, c(m, m*(n-1))),
    tt = rep(s,n),
    tn = rep(1:m,n),
    y = c(t(yTrue))
  ) %>%
    mutate(y = ifelse(tt == 10 & id == 1, y + delta, y))
  ds
}