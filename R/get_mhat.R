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

  n <- length(post_y0)
  lk <- min(c(n-1, floor(n/2), 20))
  gk <- min(floor(sqrt(c(n-1, n/2, 40))))

  if (linear) {
    fgam_fit <- try(fgam(post_y0 ~ lf(pre_y0)), silent = TRUE)
  } else {
    fgam_fit <- try(fgam(post_y0 ~ af(pre_y0, k = gk)), silent = TRUE)
  }
  if (inherits(fgam_fit, 'try-error')) {
    yhat_ds <- pre_y %>%
      select(!!idn, !!trtn) %>%
      mutate(yhat_0 = NA)
  } else {
    yhat_ds <- pre_y %>%
      select(!!idn, !!trtn) %>%
      mutate(yhat_0 = predict(fgam_fit,
                              newdata= list(pre_y0 =
                                              pre_y %>%
                                              select(-!!idn, -!!trtn) %>%
                                              as.matrix)))
  }
  yhat_ds
}