library(tidyverse)
library(splines)
library(hydroGOF)

source("data-raw/EstModels.R")
if (!exists("Hh_df"))
  source("data-raw/LoadDataforModelEst.R")

mm_df <- Hh_df %>%
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
                                                   AADVMT + VehPerDriver + LifeCycle + Age0to14 + Age65Plus + D2A_EPHHM +
                                                   D5 + Workers + LogIncome + D3apo,
                                                         data= ., weights=.$hhwgt, na.action=na.exclude),
  "non_metro",2,  function(y) exp(y),  ~lm(log(atd.miles.Bike) ~ AADVMT + Age0to14 +
                                             Age65Plus + LogIncome + LifeCycle + D2A_JPHH + D1B + D5,
                                           data= ., weights=.$hhwgt, subset=(atd.miles.Bike > 0), na.action=na.exclude)
)

m1cv <- mm_df %>%
  EstModelWith(BikeTFL_fmlas)   %>%
  name_list.cols(name_cols=c("metro", "step"))

m1cv$model %>% map(summary)

m1cv %>%
  dplyr::select(metro, ends_with("rmse"), ends_with("r2")) %>%
  group_by(metro) %>%
  summarize_all(funs(mean))

BikeTFLModel_df <- m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(BikeTFLModel_df, overwrite = TRUE)
