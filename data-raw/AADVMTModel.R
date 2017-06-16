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
  mutate(train = map(train, ~resample_filter(.x, AADVMT<quantile(AADVMT, .99, na.rm=T))),
         test = map(test, ~resample_filter(.x, AADVMT<quantile(AADVMT, .99, na.rm=T))))

fmlas <- list(metro=~lm(I(AADVMT^0.38) ~ DRVRCNT+HHSIZE+WRKCOUNT+LogIncome+Age0to14+Age65Plus+ns(log1p(VehPerDriver), 3) + LIF_CYC+
                          log1p(TRPOPDEN)+log1p(EMPTOT_5)+CENSUS_D+Fwylnmicap+Tranmilescap+Tranmilescap:D4c+D1B+D2A_WRKEMP+
                          D3bpo4+D5cr, data= .),
              non_metro=~lm(I(AADVMT^0.38) ~ DRVRCNT+HHSIZE+WRKCOUNT+CENSUS_D+LogIncome+Age0to14+Age65Plus+ns(log1p(VehPerDriver),3)+LIF_CYC+
                              log1p(TRPOPDEN)+log1p(EMPTOT_5)+D1B+D2A_EPHHM+D1B:D2A_EPHHM+
                              I(D5ar/1000)
                            , data= .))

inv_fun <- function(x, pow=0.38) x^(1/pow)

aadvmt_lm <- mm_df %>%
  add_formulas(fmlas, colname="formula") %>%
  mutate(model = map2(train, formula, est_model),
         preds=map2(model, test, predict, type="response"),
         preds=map(preds, inv_fun),
         rmse = map2_dbl(preds, map(test, resample_get, col_name="AADVMT"), calc_rmse),
         nrmse = map2_dbl(preds, map(test, resample_get, col_name="AADVMT"), calc_nrmse),
         r.squared = map_dbl(map(model, summary), "r.squared")
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I() #name_list.cols(name_col=c("metro", ".id"))

aadvmt_lm$post_func <- list(inv_fun)

aadvmt_lm$model %>% map(summary)

AADVMTModel_df <- aadvmt_lm %>%
  select(metro, model) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(AADVMTModel_df, overwrite = TRUE)
