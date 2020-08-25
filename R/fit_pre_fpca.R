fit_pre_fpca <- function(id, trt, time, y, pre_data, ops, weights = NULL) {

  idn <- enquo(id)
  trtn <- enquo(trt)
  timen <- enquo(time)
  yn <- enquo(y)
  pre_y <- pre_data %>%
    select(!!idn, !!trtn, !!timen, !!yn) %>%
    spread(!!timen, !!yn)
  ## might need to do some work to make sure that the times are ordered correctly after spread...

  L3 <- MakeFPCAInputs(IDs = pull(pre_data, !!idn),
                       tVec = pull(pre_data, !!timen),
                       t(pre_y %>%
                           select(-!!idn, -!!trtn) %>%
                           as.matrix))
  fpc_fit <- FPCA(L3$Ly, L3$Lt, optns = ops, weights = weights)
  fpc_fit
}

get_fpcs <- function(fpca_fit) {
  ids <- as.numeric(names(fpca_fit$inputData[[1]])) ## fix this?
  xi_ds <- tibble(id = ids,
                  xi = fpca_fit$xiEst)
  xi_ds
}