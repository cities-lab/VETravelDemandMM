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

mm_df <- tibble(.id=1,
                train=list(Hh_df),
                test=train)

Vehicles_fmlas <- tribble(
  ~.id, ~model_name, ~post_func, ~fmla,
  1,       "ologit",  function(y) as.integer(as.character(y)),   ~polr(Vehicles_f ~ DrvAgePop + Workers + LogIncome + LifeCycle,
                                                                   data=., weights=.$hhwgt, na.action=na.exclude, Hess=TRUE)
)

Vehicles_ologit <- mm_df %>%
  EstModelWith(Vehicles_fmlas)

Vehicles_ologit$model %>% map(summary)

VehiclesModel_df <- Vehicles_ologit %>%
  dplyr::select(model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(VehiclesModel_df, overwrite = TRUE)
