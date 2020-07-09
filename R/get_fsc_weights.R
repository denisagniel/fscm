get_fsc_weights <- function(fpca_fit, trt_ds, weighting = NULL) {
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
xi_1 <- xi_ds %>%
  filter(trt == 1) %>%
  select(-id, -trt)
n1 <- nrow(xi_1)
xi_1 <- xi_1 %>%
  unlist %>%
  matrix(KK, n1)

  wts <- augsynth:::synth_qp(X1 = xi_1,
                              X0 = xi_ds %>%
                                filter(trt == 0) %>%
                                select(-id, -trt) %>%
                                as.matrix,
                              V = vv)
  wt_ds <- xi_ds %>%
    select(id, trt) %>%
    mutate(wt = ifelse(trt == 1, 1/sum(trt), wts))
  wt_ds
}