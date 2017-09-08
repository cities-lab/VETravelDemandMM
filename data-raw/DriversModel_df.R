library(MASS)
library(tidyverse)
library(splines)
library(hydroGOF)

source("data-raw/EstModels.R")
if (!exists("Hh_df"))
  source("data-raw/LoadDataforModelEst.R")

Hh_df <- Hh_df %>%
  mutate(
    Drivers_f = as.factor(Drivers),
    Vehicles_f = as.factor(Vehicles)
  )

mm_df <- tibble(train=list(Hh_df),
                test=train)

Drivers_fmlas <- tribble(
  ~model_name, ~step, ~post_func, ~fmla,
  "ologit",  1, function(y) as.integer(as.character(y)),   ~polr(Drivers_f ~ DrvAgePop + Workers + LogIncome + Vehicles + LifeCycle,
                                                                   data=., weights=.$hhwgt, na.action=na.exclude, Hess=TRUE)
)

Drivers_ologit <- mm_df %>%
  crossing(Drivers_fmlas) %>%
  mutate(model = map2(train, fmla, est_model),
         # #y_train = map(train, resample_get, col_name="DVMT"),
         # #preds_train = map2(model, train, predict, type="response"),
         # #bias.adj = map(y_train, preds_train, ~mean(y_train/preds_train, na.rm=TRUE)),
         preds = map2(model, test, ~predict(.x, .y)),
         yhat = map2(preds, post_func, `.y(.x)`),
         y = map(test, "Drivers"),
         rmse = map2_dbl(yhat, y, rmse),
         nrmse = map2_dbl(yhat, y, nrmse),
         AIC=map_dbl(model, AIC),
         BIC=map_dbl(model, BIC),
         # compute McFadden's R2
         r2_model0 = map2(model, train, ~update(.x, .~1, data=.y)),
         r2_ll0 = map_dbl(r2_model0, logLik),
         r2_ll1 = map_dbl(model, logLik),
         pseudo.r2 = 1 - r2_ll1/r2_ll0
  ) %>%
  #add_pseudo_r2() %>%
  dplyr::select(-c(test, train)) %>%
  dplyr::select(-starts_with("r2_"))

Drivers_ologit$model %>% map(summary)

DriversModel_df <- Drivers_ologit %>%
  dplyr::select(model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(DriversModel_df, overwrite = TRUE)
