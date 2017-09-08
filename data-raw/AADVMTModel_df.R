library(tidyverse)
library(splines)

#source('../code/functions.R')
#source('../code/load_data.R')
#source('../code/comp_dependencies.R')
#source('../code/rename_variables.R')
#source('../code/partition_data.R')
#source('../code/est_models.R')

hh_df_file <- "../output/intermediate/hh_df.rda"
hh_df <- load_or_source(hh_df_file, "hh.df")

hh_df <- compute_dependencies(hh_df)
hh_df <- rename_variables(hh_df)

mm_df <- hh_df %>%
  segment_data("metro") %>%
  name_list.cols(name_cols=c("metro")) %>%
  mutate(train = map(train, ~resample_filter(.x, AADVMT<quantile(AADVMT, .99, na.rm=T))),
         test = map(test, ~resample_filter(.x, AADVMT<quantile(AADVMT, .99, na.rm=T))))

AADVMT_fmlas <- tribble(
  ~metro,  ~step, ~post_func,                                    ~fmla,
  "metro",    1,  function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers + Workers+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver) + LifeCycle+
                                                                       CENSUS_R+FwyLaneMiPC+TranRevMiPC:D4c+D1B+D2A_WRKEMP+D3bpo4,
                                                                     data= ., weights=.$hhwgt, na.action=na.exclude),
  "non_metro",1,  function(x, pow=0.38) x^(1/pow),   ~lm(I(AADVMT^0.38) ~ Drivers+HhSize+Workers+CENSUS_R+LogIncome+Age0to14+Age65Plus+log1p(VehPerDriver)+LifeCycle+
                                                                       D1B*D2A_EPHHM,
                                                                     data= ., weights=.$hhwgt, na.action=na.exclude)
)

AADVMT_lm <- mm_df %>%
  left_join(AADVMT_fmlas, by="metro") %>%
  mutate(model = map2(train, fmla, est_model),
         preds = map2(model, test, ~predict(.x, .y)),
         yhat = map2(preds, post_func, `.y(.x)`),
         y = map(test, resample_get, col_name="AADVMT"),
         rmse = map2_dbl(yhat, y, calc_rmse),
         nrmse = map2_dbl(yhat, y, calc_nrmse),
         AIC=map_dbl(model, AIC),
         BIC=map_dbl(model, BIC),
         r.squared = map_dbl(map(model, summary), "r.squared")
  ) %>%
  #add_pseudo_r2() %>%
  dplyr::select(-c(test, train)) %>%
  dplyr::select(-starts_with("r2_"))

AADVMT_lm$model %>% map(summary)
AADVMT_lm

AADVMTModel_df <- AADVMT_lm %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(AADVMTModel_df, overwrite = TRUE)
