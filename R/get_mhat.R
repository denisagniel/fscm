get_mhat <- function(id, trt, time, y, pre_data, post_data, linear = FALSE) {

  # this assumes only one post-timepoint
  idn <- enquo(id)
  trtn <- enquo(trt)
  timen <- enquo(time)
  yn <- enquo(y)
  pre_y <- pre_data %>%
    select(!!idn, !!trtn, !!timen, !!yn) %>%
    spread(!!timen, !!yn)
  pre_y0 <- pre_y %>%
    filter(!!trtn == 0) %>%
    arrange(!!idn) %>%
    select(-!!idn, -!!trtn) %>%
    as.matrix
  post_y0 <- post_data %>%
    filter(!!trtn == 0) %>%
    arrange(!!idn) %>%
    pull(!!yn)

  if (linear) {
    fgam_fit <- fgam(post_y0 ~ lf(pre_y0))
  } else {
    fgam_fit <- fgam(post_y0 ~ af(pre_y0))
  }

  yhat_ds <- pre_y %>%
    select(!!idn, !!trtn) %>%
    mutate(yhat_0 = predict(fgam_fit,
                            newdata= list(pre_y0 =
                                            pre_y %>%
                                            select(-!!idn, -!!trtn) %>%
                                                      as.matrix)))
  yhat_ds
}