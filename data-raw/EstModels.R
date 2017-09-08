library(hydroGOF)
library(tidyverse)

#' trim from model object objects with large memory/space footprint and unnecessary for
#' model prediction (predict(model, data) call)
trim_model <- function(model) {
  if ("zeroinfl" %in% class(model)) {
    model$model <- NULL
    environment(model$formula) <- new.env()
    environment(model$terms$full) <- new.env()
    environment(model$terms$zero) <- new.env()
    environment(model$terms$count) <- new.env()
  }

  if ("hurdle" %in% class(model)) {
    model$model <- NULL
    environment(model$formula) <- new.env()
    environment(model$terms$full) <- new.env()
    environment(model$terms$zero) <- new.env()
    environment(model$terms$count) <- new.env()
  }

  if ("polr" %in% class(model)) {
    model$model <- NULL
    environment(model$terms) <- new.env()
    model$fitted.values <- 0.0
    #model$na.action <- NULL
    model$lp <- NULL
    #model$qr$qr <- NULL
  }

  if ("lm" %in% class(model)) {
    model$model <- NULL
    environment(model$terms) <- new.env()
    model$fitted.values <- NULL
    model$residuals <- NULL
    model$qr$qr <- NULL
  }

  model
}

est_model_with <- function(data, fmla_df)
  data %>%
  left_join(fmla_df) %>%
  mutate(model = map2(train, fmla, est_model),
         # #y_train = map(train, resample_get, col_name="DVMT"),
         # #preds_train = map2(model, train, predict, type="response"),
         # #bias.adj = map(y_train, preds_train, ~mean(y_train/preds_train, na.rm=TRUE)),
         preds = map2(model, test, ~predict(.x, .y)),
         yhat = map2(preds, post_func, `.y(.x)`),
         y_name = map_chr(model, ~all.vars(terms(.))[1]),
         y = map2(test, y_name, ~.x[[.y]]),
         rmse = map2_dbl(yhat, y, rmse),
         nrmse = map2_dbl(yhat, y, nrmse),
         AIC=map_dbl(model, AIC),
         BIC=map_dbl(model, BIC)
         # compute McFadden's R2
         #r2_model0 = map2(model, train, ~update(.x, .~1, data=.y)),
         #r2_ll0 = map_dbl(r2_model0, logLik),
         #r2_ll1 = map_dbl(model, logLik),
         #pseudo.r2 = 1 - r2_ll1/r2_ll0
  ) %>%
  #add_pseudo_r2() %>%
  #dplyr::select(-c(test, train)) %>%
  dplyr::select(-starts_with("r2_"))

#' estimate model parameters with data and formula
#' needed for estimating models for corresponding formula within purrr::map2() call
est_model <- function(data, formula, ...) {
  #at_depth(data, 0, formula, ...)
  map(list(data), formula, ...)[[1]]
}

`.y(.x)` <- est_model

#' calculate rmse
calc_nrmse <- function(y.hat, y, na.rm=TRUE, ...) {
  ## Convert y.hat and y to vectors as hydroGOF::rmse only accepts vectors
  if ('data.frame' %in% class(y.hat)) {
    y.hat <- y.hat[[1]]
  }
  if ('data.frame' %in% class(y)) {
    y <- y[[1]]
  }
  nrmse(y.hat, y, na.rm=na.rm, ...)
  #hydroGOF::rmse(y.hat, y, na.rm=na.rm, ...)
}

#' calculate rmse
calc_rmse <- function(y.hat, y, na.rm=TRUE, ...) {
  ## Convert y.hat and y to vectors as hydroGOF::rmse only accepts vectors
  if ('data.frame' %in% class(y.hat)) {
    y.hat <- y.hat[[1]]
  }
  if ('data.frame' %in% class(y)) {
    y <- y[[1]]
  }
  rmse(y.hat, y, na.rm=na.rm, ...)
  #hydroGOF::rmse(y.hat, y, na.rm=na.rm, ...)
}
