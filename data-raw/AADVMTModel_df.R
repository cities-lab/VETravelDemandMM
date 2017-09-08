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

AADVMT_fmlas <- tribble(
  ~metro,  ~step, ~post_func,                                    ~fmla,
  "metro",    1,  function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers + Workers+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver) + LifeCycle+
                                                                       CENSUS_R+FwyLaneMiPC+TranRevMiPC:D4c+D1B+D2A_WRKEMP+D3bpo4,
                                                                     data= ., weights=.$WTHHFIN, na.action=na.exclude),
  "non_metro",1,  function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers+HhSize+Workers+CENSUS_R+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver)+LifeCycle+
                                                                       D1B*D2A_EPHHM,
                                                                     data= ., weights=.$WTHHFIN, na.action=na.exclude)
)

AADVMT_lm <- mm_df %>%
  left_join(AADVMT_fmlas, by="metro") %>%
  mutate(model = map2(train, fmla, est_model),
         preds = map2(model, test, ~predict(.x, .y)),
         yhat = map2(preds, post_func, `.y(.x)`),
         y = map(test, "AADVMT"),
         rmse = map2_dbl(yhat, y, rmse),
         nrmse = map2_dbl(yhat, y, nrmse),
         AIC=map_dbl(model, AIC),
         BIC=map_dbl(model, BIC),
         r.squared = map_dbl(map(model, summary), "r.squared")
  ) %>%
  #add_pseudo_r2() %>%
  dplyr::select(-c(test, train)) %>%
  dplyr::select(-starts_with("r2_"))

AADVMT_lm$model %>% map(summary)
AADVMT_lm

AADVMTModel_df <- AADVMT_lm %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(AADVMTModel_df, overwrite = TRUE)
