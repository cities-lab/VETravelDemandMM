library(tidyverse)
library(splines)
library(mhurdle)

source('../code/functions.R')
source('../code/load_data.R')
source('../code/comp_dependencies.R')
source('../code/rename_variables.R')
source('../code/partition_data.R')
source('../code/est_models.R')

hh_df_file <- "../output/intermediate/hh_df.rda"
hh_df <- load_or_source(hh_df_file, "hh.df")

hh_df <- compute_dependencies(hh_df)
hh_df <- rename_variables(hh_df)

hh_df <- hh_df %>%
  mutate(
    hhwgt=WTHHFIN * n()/sum(WTHHFIN),
    td.miles.Transit_f = as.factor(round(td.miles.Transit, digits=1))
  )

mm_df <- hh_df %>%
  segment_data("metro") %>%
  name_list.cols(name_cols=c("metro")) %>%
  mutate(train=map(train, as.data.frame),
         test=map(test, as.data.frame))

int_round <- function(x) as.integer(round(x))
int_cround <- function(x) as.integer(ifelse(x<1, ceiling(x), round(x)))
fctr_round1 <- function(x) as.factor(round(x, digits=1))

est_model_with <- function(data, fmla_df)
  data %>%
  left_join(fmla_df) %>%
  mutate(model = map2(train, fmla, est_model),
         # #y_train = map(train, resample_get, col_name="DVMT"),
         # #preds_train = map2(model, train, predict, type="response"),
         # #bias.adj = map(y_train, preds_train, ~mean(y_train/preds_train, na.rm=TRUE)),
         preds = map2(model, test, ~predict(.x, .y)),
         yhat = map2(preds, post_func, `.y(.x)`),
         y_name = map_chr(model, ~all.vars(terms(.))[1]),
         y = map2(test, y_name, ~.x[[.y]]),
         rmse = map2_dbl(yhat, y, calc_rmse),
         nrmse = map2_dbl(yhat, y, calc_nrmse),
         AIC=map_dbl(model, AIC),
         BIC=map_dbl(model, BIC)
         # compute McFadden's R2
         #r2_model0 = map2(model, train, ~update(.x, .~1, data=.y)),
         #r2_ll0 = map_dbl(r2_model0, logLik),
         #r2_ll1 = map_dbl(model, logLik),
         #pseudo.r2 = 1 - r2_ll1/r2_ll0
  ) %>%
  #add_pseudo_r2() %>%
  #dplyr::select(-c(test, train)) %>%
  dplyr::select(-starts_with("r2_"))

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

m1cv <- est_model_with(mm_df, TransitPMT_fmlas)
m1cv$model %>% map(summary)
m1cv %>% dplyr::select(name, metro, rmse, nrmse, preds, yhat, y)

ybars <- ybar_by_dx(hh_df, x_col="D1B", model_df=m1cv,
                    y_col="td.miles.Transit",
                    x_args=seq(.1, 6, by=0.4), x_func = `*`,
                    segment_col="metro",
                    update_dependencies=FALSE,
                    update_func=NULL,
                    predict_type="response"
                    #predict_type="class"
)

TransitPMTModel_df <-  m1cv %>%
  dplyr::select(metro, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(TransitPMTModel_df, overwrite = TRUE)
