#' Estimate WalkTFL (trip frequency and length) Models for households
#'
library(tidyverse)
library(splines)
library(hydroGOF)

source("data-raw/EstModels.R")
if (!exists("Hh_df"))
  source("data-raw/LoadDataforModelEst.R")

#' converting household data.frame to a list-column data frame segmented by
#' metro ("metro" and "non-metro")
mm_df <- Hh_df %>%
  filter(AADVMT<quantile(AADVMT, .99, na.rm=T)) %>%
  nest(-metro) %>%
  rename(train=data) %>%
  mutate(test=train) # use the same data for train & test

#' model formula for each segment as a tibble (data.frame), also include a
#' `post_func` column with functions de-transforming predictions to the original
#' scale of the dependent variable
BikeTFL_fmlas <- tribble(
  ~metro,  ~step, ~post_func,        ~fmla,
  "metro",    1,  function(y) y,      ~pscl::hurdle(ntrips.Walk ~ AADVMT + VehPerDriver+HhSize+LifeCycle+Age0to14+D1D+D2A_EPHHM + D3bmm4 + D3bpo4 +
                                                   D5 + FwyLaneMiPC + TranRevMiPC + LogIncome + D3apo + D4c |
                                                   AADVMT + VehPerDriver+HhSize+LifeCycle+Age0to14+D1D+D2A_EPHHM + D3bmm4 + D3bpo4 +
                                                   D5 + Workers + FwyLaneMiPC + TranRevMiPC + LogIncome + D3apo + D4c,
                                                 data=., weights=.$hhwgt, na.action=na.omit),
  "metro",    2,  function(y) exp(y), ~lm(log(atd.miles.Walk) ~ AADVMT + VehPerDriver + Age0to14 +
                                            Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                            D1B + D3bmm4 + D5cri + TranRevMiPC + TranRevMiPC:D4c,
                                          data=., subset=(atd.miles.Walk > 0), weights=.$hhwgt, na.action=na.omit),
  "non_metro",1,  function(y) y,      ~pscl::hurdle(ntrips.Walk ~  AADVMT + VehPerDriver+HhSize+LifeCycle+Age0to14+D1D+D2A_EPHHM + D3bpo4 +
                                                   D5 + Workers + LogIncome |
                                                   AADVMT + VehPerDriver + HhSize + LifeCycle + Age0to14 + D1D + D2A_EPHHM + D3bpo4 +
                                                   Workers + LogIncome + D3apo,
                                                 data=., weights=.$hhwgt, na.action=na.omit),
  "non_metro",2,  function(y) exp(y), ~lm(log(atd.miles.Walk) ~ AADVMT + Age0to14 +
                                             Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                             D1B + D5,
                                           data=., subset=(atd.miles.Walk > 0), weights=.$hhwgt, na.action=na.omit)
)

#' call function to estimate models for each segment and add name for each
#' segment
m1cv <- mm_df %>%
  EstModelWith(BikeTFL_fmlas) %>%
  name_list.cols(name_cols=c("metro", "step"))

#' print model summary and goodness of fit
m1cv$model %>% map(summary)
m1cv #%>%
  #dplyr::select(metro, ends_with("rmse"), ends_with("r2")) %>%
  #group_by(metro) %>%
  #summarize_all(funs(mean))

#' trim model object to save space
WalkTFLModel_df <- m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#' save model_df to `data/`
devtools::use_data(WalkTFLModel_df, overwrite = TRUE)
