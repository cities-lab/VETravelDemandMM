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

BikePMT_fmlas <- tribble(
  ~name, ~metro,        ~post_func,      ~fmla,
  "hurdle", "metro",    function(y) y,   ~pscl::hurdle(int_cround(td.miles.Bike) ~ AADVMT + Workers + VehPerDriver +
                                                          LifeCycle + Age0to14 + CENSUS_R + D1B*D2A_EPHHM + FwyLaneMiPC + D4c + TranRevMiPC:D4c |
                                                            AADVMT + Workers + LifeCycle + Age0to14 + CENSUS_R +  D1B + D1B:D2A_EPHHM
                                                          + D5 + FwyLaneMiPC + TranRevMiPC,
                                                        data= ., weights=.$hhwgt, na.action=na.exclude),
  "hurdle", "non_metro",function(y) y,   ~pscl::hurdle(int_cround(td.miles.Bike) ~ AADVMT + HhSize +
                                                              HhSize + LifeCycle + Age0to14 + Age65Plus + D1B + D1B:D2A_EPHHM + D3bpo4 |
                                                              AADVMT + Workers +
                                                              LifeCycle + Age0to14 + D1B + D2A_EPHHM + D3bpo4 + D5,
                                                            data= ., weights=.$hhwgt, na.action=na.exclude)
)

m1cv <- mm_df %>%
  EstModelWith(BikePMT_fmlas)   %>%
  name_list.cols(name_cols=c("metro"))

m1cv$model %>% map(summary)
m1cv %>% dplyr::select(name, metro, preds, yhat, y)

BikePMTModel_df <-  m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(BikePMTModel_df, overwrite = TRUE)
