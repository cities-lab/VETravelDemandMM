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
  name_list.cols(name_cols=c("metro"))

fmlas1 <- list(metro    =~pscl::hurdle(ntrips.Transit ~ AADVMT+HHSIZE+LIF_CYC+
                                        Age0to14+D1D+Tranmilescap+D4c |
                                        AADVMT+VehPerDriver+HHSIZE+WRKCOUNT+LIF_CYC+Age0to14+D1D+
                                        Fwylnmicap+Tranmilescap:D4c,
                                      data=., na.action=na.omit),
              non_metro=~pscl::hurdle(ntrips.Transit ~  log1p(AADVMT)+log1p(VehPerDriver)+HHSIZE+LIF_CYC+
                                        Age0to14+LogIncome+D1D |
                                        log1p(AADVMT)+log1p(VehPerDriver)+WRKCOUNT+LIF_CYC+Age0to14+D1B+D3bmm4+
                                        LogIncome,
                                      data=., na.action=na.omit))



inv_fun <- function(x) x

TransitTFModel_df <- mm_df %>%
  add_formulas(fmlas1) %>%
  mutate(
    model = map2(train, formula, est_model),
    pseudo.r2 = map_dbl(model, calc_pseudo.r2),
    preds = map2(model, test, predict, type="response"),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="ntrips.Transit"), calc_rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I()

TransitTFModel_df$model %>% map(summary)
TransitTFModel_df$post_func <- list(inv_fun)

fmlas2 <- list(    metro   =~lm(log(atd.miles.Transit) ~ AADVMT + VehPerDriver + Age0to14 +
                                  Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                  D1B + D3bmm4 + D5cri + Tranmilescap + Tranmilescap:D4c,
                                data=., subset=(atd.miles.Transit > 0), na.action=na.omit),
                   non_metro=~lm(log(atd.miles.Transit) ~ AADVMT + Age0to14 +
                                   Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                   D1B + D5cri,
                                 data=., subset=(atd.miles.Transit > 0), na.action=na.omit)
)

inv_fun <- function(x) exp(x)

TransitTLModel_df <- mm_df %>%
  add_formulas(fmlas2) %>%
  mutate(
    model = map2(train, formula, est_model),
    preds = map2(model, test, predict, type="response"),
    preds = map(preds, inv_fun),
    r2 = map_dbl(map(model, summary), "r.squared"),
    #nrmse = map2_dbl(preds, map(test, resample_get, col_name="td.miles.Transit"), calc_nrmse),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="atd.miles.Transit"), calc_rmse)
    #rmse = map2_dbl(model, test, modelr::rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I() #name_list.cols()

TransitTLModel_df$model %>% map(summary)
TransitTLModel_df$post_func <- list(inv_fun)

TransitTFLModel_df <- bind_rows(TransitTFModel_df %>% mutate(step=1),
                             TransitTLModel_df %>% mutate(step=2)) %>%
  select(metro, step, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(TransitTFLModel_df, overwrite = TRUE)
