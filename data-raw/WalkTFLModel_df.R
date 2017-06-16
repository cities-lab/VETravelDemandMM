library(tidyverse)
library(splines)

source('../code/functions.R')
source('../code/load_data.R')
source('../code/comp_dependencies.R')
source('../code/partition_data.R')
source('../code/est_models.R')

hh_df_file <- "../output/intermediate/hh_df.rda"
hh_df <- load_or_source(hh_df_file, "hh.df")

hh_df <- compute_dependencies(hh_df)

mm_df <- hh_df %>%
  segment_data("metro") %>%
  name_list.cols(name_cols=c("metro")) %>%
  as_data_frame()

fmlas1 <- list(metro    =~pscl::hurdle(ntrips.Walk ~ AADVMT + VehPerDriver+HHSIZE+LIF_CYC+Age0to14+D1D+D2A_EPHHM + D3bmm4 + D3bpo4 +
                                        log1p(D5ar) + Fwylnmicap + Tranmilescap + LogIncome + D3apo + D4c |
                                        AADVMT + VehPerDriver+HHSIZE+LIF_CYC+Age0to14+D1D+D2A_EPHHM + D3bmm4 + D3bpo4 +
                                        log1p(D5ar) + WRKCOUNT + Fwylnmicap + Tranmilescap + LogIncome + D3apo + D4c,
                                      data=., na.action=na.omit),
              non_metro=~pscl::hurdle(ntrips.Walk ~  AADVMT + VehPerDriver+HHSIZE+LIF_CYC+Age0to14+D1D+D2A_EPHHM + D3bpo4 +
                                        log1p(D5ar) + WRKCOUNT + LogIncome |
                                        AADVMT + VehPerDriver + HHSIZE + LIF_CYC + Age0to14 + D1D + D2A_EPHHM + D3bpo4 +
                                        WRKCOUNT + LogIncome + D3apo,
                                      data=., na.action=na.omit))

inv_fun <- function(x) x

WalkTFModel_df <- mm_df %>%
  add_formulas(fmlas1) %>%
  mutate(
    model = map2(train, formula, est_model),
    pseudo.r2 = map_dbl(model, calc_pseudo.r2),
    preds = map2(model, test, predict, type="response"),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="ntrips.Walk"), calc_rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I()

WalkTFModel_df$model %>% map(summary)
WalkTFModel_df$post_func <- list(inv_fun)

fmlas2 <- list(    metro   =~lm(log(atd.miles.Walk) ~ AADVMT + VehPerDriver + Age0to14 +
                                Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                D1B + D3bmm4 + D5cri + Tranmilescap + Tranmilescap:D4c,
                                data=., subset=(atd.miles.Walk > 0), na.action=na.omit),
                   non_metro=~lm(log(atd.miles.Walk) ~ AADVMT + Age0to14 +
                                 Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                 D1B + D5cri,
                                 data=., subset=(atd.miles.Walk > 0), na.action=na.omit)
)

inv_fun <- function(x) exp(x)

WalkTLModel_df <- mm_df %>%
  add_formulas(fmlas2) %>%
  mutate(
    model = map2(train, formula, est_model),
    preds = map2(model, test, predict, type="response"),
    preds = map(preds, inv_fun),
    r2 = map_dbl(map(model, summary), "r.squared"),
    #nrmse = map2_dbl(preds, map(test, resample_get, col_name="td.miles.Walk"), calc_nrmse),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="atd.miles.Walk"), calc_rmse)
    #rmse = map2_dbl(model, test, modelr::rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I() #name_list.cols()

WalkTLModel_df$model %>% map(summary)
WalkTLModel_df$post_func <- list(inv_fun)

WalkTFLModel_df <- bind_rows(WalkTFModel_df %>% mutate(step=1),
                             WalkTLModel_df %>% mutate(step=2)) %>%
  select(metro, step, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(WalkTFLModel_df, overwrite = TRUE)
