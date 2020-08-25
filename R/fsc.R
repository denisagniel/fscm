fsc <- function(id, trt, time, y, pre_data, post_data, linear = FALSE, include_y = FALSE, fpc_optns = NULL, wts = NULL) {
  idn <- enquo(id)
  trtn <- enquo(trt)
  timen <- enquo(time)
  yn <- enquo(y)

  trt_ds <- unique(select(post_data, !!idn, !!trtn)) ## check this
  fpc_fit <- fit_pre_fpca(id = !!idn,
                          trt = !!trtn,
                          time = !!timen,
                          y = !!yn,
                          pre_data = pre_data,
                          ops = fpc_optns,
                          weights = wts)

  if (include_y) {
    pre_y <- pre_data %>%
      select(!!idn, !!trtn, !!timen, !!yn) %>%
      spread(!!timen, !!yn)
    wt_ds <- get_fsc_weights(fpc_fit, trt_ds, y_ds = pre_y)
  } else {
    wt_ds <- get_fsc_weights(fpc_fit, trt_ds)
  }

  m_ds <- get_mhat(id = !!idn,
                   trt = !!trtn,
                   time = !!timen,
                   y = !!yn,
                   pre_data = pre_data,
                   post_data = post_data,
                   linear = linear)

  updated_post_data <- post_data %>%
    inner_join(m_ds) %>%
    inner_join(wt_ds)
  updated_post_data %>%
    group_by(!!trtn) %>%
    summarise(y0 = sum(wt*!!yn),
              m0 = sum(wt*yhat_0)) %>%
    ungroup %>%
    summarise(sc_est = y0[trt == 1] - y0[trt == 0],
              asc_est = y0[trt == 1] - (y0[trt == 0] + m0[trt == 1] - m0[trt == 0]))
}