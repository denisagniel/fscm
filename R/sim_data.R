sim_data <- function(n, m, sigma2, delta) {
  s <- seq(0,10,length.out = m)
  meanFunct <- function(s) s + 10*exp(-(s-5)^2)
  eigFunct1 <- function(s) +cos(2*s*pi/10) / sqrt(5)
  eigFunct2 <- function(s) -sin(2*s*pi/10) / sqrt(5)
  # eigFunct3 <- function(s) +cos(2*s*pi/20) / sqrt(5)
  # eigFunct4 <- function(s) -sin(2*s*pi/20) / sqrt(5)

  Ksi <- matrix(rnorm(n*2), ncol=2);
  Ksi <- apply(Ksi, 2, scale)
  Ksi <- Ksi %*% diag(c(5,2))

  # Create Y_true
  yTrue <- Ksi %*% t(matrix(c(eigFunct1(s),eigFunct2(s)#,eigFunct3(s),eigFunct4(s)
                              ), ncol=2)) + t(matrix(rep(meanFunct(s),n), nrow=m)) + rnorm(n*m, sd = sigma2) %>% matrix(n, m)

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