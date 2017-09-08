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

int_round <- function(x) as.integer(round(x))
int_cround <- function(x) as.integer(ifelse(x<1, ceiling(x), round(x)))
fctr_round1 <- function(x) as.factor(round(x, digits=1))

WalkPMT_fmlas <- tribble(
  ~name, ~metro,  ~step, ~post_func,      ~fmla,
  "hurdle", "metro",    1,  function(y) y,   ~pscl::hurdle(int_cround(td.miles.Walk) ~ AADVMT + Workers + VehPerDriver +
                                                          LifeCycle + Age0to14 + CENSUS_R + D1B+D2A_EPHHM + FwyLaneMiPC + TranRevMiPC:D4c +
                                                            D5 + D3apo |
                                                            AADVMT + Workers + LifeCycle + Age0to14 + CENSUS_R +  D1B:D2A_EPHHM + D3apo
                                                          + D5 + TranRevMiPC,
                                                        data= ., weights=.$hhwgt, na.action=na.exclude),
  "hurdle", "non_metro", 1,  function(y) y,   ~pscl::hurdle(int_cround(td.miles.Walk) ~ AADVMT + HhSize + VehPerDriver +
                                                              LifeCycle + Age0to14 + Age65Plus + CENSUS_R + D1B + D1B:D2A_EPHHM + D3bpo4 |
                                                              AADVMT + Workers + LogIncome + HhSize +
                                                              Age0to14 + CENSUS_R + D3apo + D5,
                                                            data= ., weights=.$hhwgt, na.action=na.exclude)
)

m1cv <- est_model_with(mm_df, WalkPMT_fmlas)
m1cv$model %>% map(summary)
m1cv %>% dplyr::select(name, metro, rmse, nrmse, preds, yhat, y)

WalkPMTModel_df <-  m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(WalkPMTModel_df, overwrite = TRUE)
