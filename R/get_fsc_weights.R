get_fsc_weights <- function(fpca_fit, trt_ds, weighting = NULL, y_ds = NULL) {
  xi_ds <- get_fpcs(fpca_fit) %>%
    inner_join(trt_ds)
  KK <- ncol(fpca_fit$xiEst)

  if (KK == 1) {
    if (is.null(weighting)) {
      vv <- matrix(1, 1, 1)
    } else {
      vv <- matrix(fpca_fit$lambda, 1, 1)
    }
  } else {
    if (is.null(weighting)) {
      vv <- diag(KK)
    } else {
      vv <- diag(fpca_fit$lambda)
    }
  }
  if (is.null(y_ds)) {
    x_1 <- xi_ds %>%
      filter(trt == 1) %>%
      select(-id, -trt)
    x_0 <- xi_ds %>%
      filter(trt == 0) %>%
      select(-id, -trt) %>%
      as.matrix
    n1 <- nrow(x_1)
    x_1 <- x_1 %>%
      unlist %>%
      matrix(KK, n1)
  } else {
    xiy <- xi_ds %>%
      inner_join(y_ds)
    x_1 <- xiy %>%
      filter(trt == 1) %>%
      select(-id, -trt)
    x_0 <- xiy %>%
      filter(trt == 0) %>%
      select(-id, -trt) %>%
      as.matrix
    kstar <- ncol(x_0)
    ky <- kstar - KK
    vvd <- c(rep(1/KK, KK), rep(1/ky, ky))
    vv <- diag(vvd)
    n1 <- nrow(x_1)
    x_1 <- x_1 %>%
      unlist %>%
      matrix(kstar, n1)
  }


  wts <- augsynth:::synth_qp(X1 = x_1,
                              X0 = x_0,
                              V = vv)
  wt_ds <- xi_ds %>%
    select(id, trt) %>%
    mutate(wt = ifelse(trt == 1, 1/sum(trt), wts))
  wt_ds
}