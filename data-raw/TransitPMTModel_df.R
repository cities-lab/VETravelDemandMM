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

int_round <- function(x) as.integer(round(x))
int_cround <- function(x) as.integer(ifelse(x<1, ceiling(x), round(x)))
fctr_round1 <- function(x) as.factor(round(x, digits=1))

TransitPMT_fmlas <- tribble(
  ~name, ~metro,  ~step, ~post_func,      ~fmla,
  "hurdle", "metro",    1,  function(y) y,   ~pscl::hurdle(int_cround(td.miles.Transit) ~ AADVMT + Workers + VehPerDriver +
                                                             LifeCycle + Age0to14 + CENSUS_R + D1B+D2A_EPHHM + FwyLaneMiPC + TranRevMiPC+D4c +
                                                             D5 + D3bpo4 |
                                                             AADVMT + Workers + LifeCycle + Age0to14 + CENSUS_R +  D1B:D2A_EPHHM + D3bpo4
                                                           + D5 + TranRevMiPC + TranRevMiPC:D4c,
                                                           data= ., weights=.$hhwgt, na.action=na.exclude),
  "hurdle", "non_metro", 1,  function(y) y,   ~pscl::hurdle(int_cround(td.miles.Transit) ~ AADVMT + HhSize + VehPerDriver +
                                                              LifeCycle + Age0to14 + Age65Plus + CENSUS_R + D1B + D1B:D2A_EPHHM + D3bmm4 |
                                                              AADVMT + Workers + HhSize +
                                                              Age0to14 + CENSUS_R + D3bmm4 + D1B + D1B:D2A_EPHHM,
                                                            data= ., weights=.$hhwgt, na.action=na.exclude)
)

m1cv <- mm_df %>%
  EstModelWith(TransitPMT_fmlas)    %>%
  name_list.cols(name_cols=c("metro", "step"))

m1cv$model %>% map(summary)
m1cv %>% dplyr::select(name, metro, preds, yhat, y)

TransitPMTModel_df <-  m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(TransitPMTModel_df, overwrite = TRUE)
