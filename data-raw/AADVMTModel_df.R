#' Estimate AADVMT Models for households
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
AADVMT_fmlas <- tribble(
  ~metro,    ~post_func,                                    ~fmla,
  "metro",    function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers + Workers+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver) + LifeCycle+
                                                                       CENSUS_R+FwyLaneMiPC+TranRevMiPC:D4c+D1B+D2A_WRKEMP+D3bpo4,
                                                                     data= ., weights=.$WTHHFIN, na.action=na.exclude),
  "non_metro",function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers+HhSize+Workers+CENSUS_R+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver)+LifeCycle+
                                                                       D1B*D2A_EPHHM,
                                                                     data= ., weights=.$WTHHFIN, na.action=na.exclude)
)

#' call function to estimate models for each segment and add name for each
#' segment
AADVMT_lm <- mm_df %>%
  EstModelWith(AADVMT_fmlas) %>%
  name_list.cols(name_cols="metro")

#' print model summary and goodness of fit
AADVMT_lm$model %>% map(summary)
AADVMT_lm

#' trim model object to save space
AADVMTModel_df <- AADVMT_lm %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, TrimModel))

#' save model_df to `data/`
devtools::use_data(AADVMTModel_df, overwrite = TRUE)
