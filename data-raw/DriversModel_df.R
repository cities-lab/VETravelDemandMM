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

Drivers_fmlas <- tribble(
  ~.id,  ~model_name, ~post_func, ~fmla,
    1,    "ologit",   function(y) as.integer(as.character(y)),   ~polr(Drivers_f ~ DrvAgePop + Workers + LogIncome + Vehicles + LifeCycle,
                                                                       data=., weights=.$hhwgt, na.action=na.exclude, Hess=TRUE)
)

Drivers_ologit <- mm_df %>%
  EstModelWith(Drivers_fmlas)

Drivers_ologit$model %>% map(summary)

DriversModel_df <- Drivers_ologit %>%
  dplyr::select(model, post_func) %>%
  mutate(model=map(model, TrimModel))

#devtools::use_data(DriversModel_df, overwrite = TRUE)
