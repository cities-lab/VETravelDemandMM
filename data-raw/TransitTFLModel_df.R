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
  "metro",    1,  function(y) y,   ~pscl::hurdle(ntrips.Transit ~ AADVMT+HhSize+LifeCycle+
                                                   Age0to14+D1D+TranRevMiPC+D4c |
                                                   AADVMT+VehPerDriver+HhSize+Workers+LifeCycle+Age0to14+D1D+
                                                   FwyLaneMiPC+TranRevMiPC:D4c,
                                                 data=., weights=.$hhwgt, na.action=na.omit),
  "metro",    2,  function(y) exp(y), ~lm(log(atd.miles.Transit) ~ AADVMT + VehPerDriver + Age0to14 +
                                            Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                            D1B + D3bmm4 + D5cri + TranRevMiPC + TranRevMiPC:D4c,
                                          data=., subset=(atd.miles.Transit > 0), weights=.$hhwgt, na.action=na.omit),
  "non_metro",1,  function(y) y,   ~pscl::hurdle(ntrips.Transit ~  log1p(AADVMT)+log1p(VehPerDriver)+HhSize+LifeCycle+
                                                   Age0to14+LogIncome+D1D |
                                                   log1p(AADVMT)+log1p(VehPerDriver)+Workers+LifeCycle+Age0to14+D1B+D3bmm4+
                                                   LogIncome,
                                                 data=., weights=.$hhwgt, na.action=na.omit),
  "non_metro",2,  function(y) exp(y),  ~lm(log(atd.miles.Transit) ~ AADVMT + Age0to14 +
                                             Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                             D1B + D5cri,
                                           data=., subset=(atd.miles.Transit > 0), weights=.$hhwgt, na.action=na.omit)
)

m1cv <- mm_df %>%
  EstModelWith(BikeTFL_fmlas) %>%
  name_list.cols(name_cols=c("metro", "step"))

m1cv$model %>% map(summary)

m1cv %>%
  dplyr::select(metro, ends_with("rmse"), ends_with("r2")) %>%
  group_by(metro) %>%
  summarize_all(funs(mean))

TransitTFLModel_df <- m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(TransitTFLModel_df, overwrite = TRUE)
