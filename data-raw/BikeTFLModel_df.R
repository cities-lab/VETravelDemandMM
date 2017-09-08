library(tidyverse)
library(splines)
library(hydroGOF)

source("data-raw/EstModels.R")
if (!exists("Hh_df"))
  source("data-raw/LoadDataforModelEst.R")

mm_df <- Hh_df %>%
  filter(AADVMT<quantile(AADVMT, .99, na.rm=T)) %>%
  nest(-metro) %>%
  rename(train=data) %>%
  mutate(test=train) # use the same data for train & test

BikeTFL_fmlas <- tribble(
  ~metro,  ~step, ~post_func,      ~fmla,
  "metro",    1,  function(y) y,   ~pscl::hurdle(ntrips.Bike ~ AADVMT + Age0to14 + Age65Plus + D1C + D3bpo4 + Workers + LogIncome |
                                                   log1p(AADVMT) + HhSize + LifeCycle + Age0to14 + Age65Plus + D2A_EPHHM + D3bpo4 +
                                                   Workers + FwyLaneMiPC + TranRevMiPC + LogIncome,
                                                   data= ., weights=.$hhwgt, na.action=na.exclude),
  "metro",    2,  function(y) exp(y), ~lm(log(atd.miles.Bike) ~ AADVMT + VehPerDriver + Age0to14 +
                                            Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                            D1B + D3bmm4 + TranRevMiPC + TranRevMiPC:D4c,
                                          data= ., weights=.$hhwgt, subset=(atd.miles.Bike > 0), na.action=na.exclude),
  "non_metro",1,  function(y) y,   ~pscl::hurdle(ntrips.Bike ~  AADVMT + VehPerDriver + HhSize + LifeCycle + Age0to14 + Age65Plus + D1D +
                                                   Workers + LogIncome + D3apo |
                                                   AADVMT + VehPerDriver + LifeCycle + Age0to14 + Age65Plus + D1A + D2A_EPHHM +
                                                   D5ar + Workers + LogIncome + D3apo,
                                                         data= ., weights=.$hhwgt, na.action=na.exclude),
  "non_metro",2,  function(y) exp(y),  ~lm(log(atd.miles.Bike) ~ AADVMT + Age0to14 +
                                             Age65Plus + LogIncome + LifeCycle + D2A_JPHH + D1B + D5,
                                           data= ., weights=.$hhwgt, subset=(atd.miles.Bike > 0), na.action=na.exclude)
)

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

m1cv <- mm_df %>%
  est_model_with(BikeTFL_fmlas)

m1cv %>%
  dplyr::select(metro, ends_with("rmse"), ends_with("r2")) %>%
  #group_by(model_name) %>%
  summarize_each(funs(mean))

BikeTFLModel_df <- m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(BikeTFLModel_df, overwrite = TRUE)
