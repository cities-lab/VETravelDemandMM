library(tidyverse)
library(splines)

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

mm_df <- hh_df %>%
  segment_data("metro") %>%
  name_list.cols(name_cols=c("metro")) %>%
  mutate(train=map(train, as.data.frame),
         test=map(test, as.data.frame))

BikeTFL_fmlas <- tribble(
  ~metro,  ~step, ~post_func,      ~fmla,
  "metro",    1,  function(y) y,   ~pscl::hurdle(ntrips.Bike ~ AADVMT + Age0to14 + Age65Plus + D1C + D3bpo4 + Workers + LogIncome |
                                                   log1p(AADVMT) + HhSize + LifeCycle + Age0to14 + Age65Plus + D2A_EPHHM + D3bpo4 +
                                                   Workers + FwyLaneMiPC + TranRevMiPC + LogIncome,
                                                   data= ., weights=.$hhwgt, na.action=na.exclude),
  "metro",    2,  function(y) exp(y), ~lm(log(atd.miles.Bike) ~ AADVMT + VehPerDriver + Age0to14 +
                                            Age65Plus + LogIncome + LifeCycle + D2A_JPHH +
                                            D1B + D3bmm4 + TranRevMiPC + TranRevMiPC:D4c,
                                          data= ., weights=.$hhwgt, subset=(atd.miles.Bike > 0), na.action=na.exclude),
  "non_metro",1,  function(y) y,   ~pscl::hurdle(ntrips.Bike ~  AADVMT + VehPerDriver + HhSize + LifeCycle + Age0to14 + Age65Plus + D1D +
                                                   Workers + LogIncome + D3apo |
                                                   AADVMT + VehPerDriver + LifeCycle + Age0to14 + Age65Plus + D1A + D2A_EPHHM +
                                                   D5ar + Workers + LogIncome + D3apo,
                                                         data= ., weights=.$hhwgt, na.action=na.exclude),
  "non_metro",2,  function(y) exp(y),  ~lm(log(atd.miles.Bike) ~ AADVMT + Age0to14 +
                                             Age65Plus + LogIncome + LifeCycle + D2A_JPHH + D1B + D5,
                                           data= ., weights=.$hhwgt, subset=(atd.miles.Bike > 0), na.action=na.exclude)
)

est_model_with_mc <- function(data, fmla_df) {
  require(multidplyr)
  cluster <- create_cluster(10)
  cluster_df <- data %>%
    left_join(fmla_df) %>%
    partition(cluster = cluster)

  cluster_df %>%
    cluster_library("pscl") %>%
    cluster_library("purrr") %>%
    cluster_assign_value("est_model", est_model) %>%
    mutate(model = map2(train, fmla, est_model)
           # #y_train = map(train, resample_get, col_name="DVMT"),
           # #preds_train = map2(model, train, predict, type="response"),
           # #bias.adj = map(y_train, preds_train, ~mean(y_train/preds_train, na.rm=TRUE)),
    ) %>%
    collect() %>% # Special collect() function to recombine partitions
    ungroup() %>%
    as_tibble() %>%
    mutate(
      preds = map2(model, test, ~predict(.x, .y)),
      yhat = map2(preds, post_func, `.y(.x)`),
      y_name = map_chr(model, ~all.vars(terms(.))[1]),
      y = map2(test, y_name, ~.x[[.y]]),
      rmse = map2_dbl(yhat, y, calc_rmse),
      nrmse = map2_dbl(yhat, y, calc_nrmse),
      AIC=map_dbl(model, AIC),
      BIC=map_dbl(model, BIC),
      # compute McFadden's R2
      r2_model0 = map2(model, train, ~update(.x, .~1, data=.y)),
      r2_ll0 = map_dbl(r2_model0, logLik),
      r2_ll1 = map_dbl(model, logLik),
      pseudo.r2 = 1 - r2_ll1/r2_ll0
    ) %>%
    dplyr::select(-starts_with("r2_"))

}

m1cv <- mm_df %>%
  est_model_with_mc(BikeTFL_fmlas)

m1cv %>%
  dplyr::select(metro, ends_with("rmse"), ends_with("r2")) %>%
  #group_by(model_name) %>%
  summarize_each(funs(mean))

fmlas1 <- list(metro    =~pscl::hurdle(ntrips.Bike ~ AADVMT + Age0to14 + Age65Plus + D1C + D3bpo4 +
                                        Workers + LogIncome |
                                        log1p(AADVMT) + HhSize + LIF_CYC + Age0to14 + Age65Plus + D2A_EPHHM + D3bpo4 +
                                        WRKCOUNT + Fwylnmicap + Tranmilescap + LogIncome,
                                      data=., na.action=na.omit),
              non_metro=~pscl::hurdle(ntrips.Bike ~  AADVMT + VehPerDriver + HhSize + LIF_CYC + Age0to14 + Age65Plus + D1D +
                                        log1p(D5ar) + WRKCOUNT + LogIncome + D3apo |
                                        AADVMT + VehPerDriver + LIF_CYC + Age0to14 + Age65Plus + D1A + D2A_EPHHM +
                                        log1p(D5ar) + WRKCOUNT + LogIncome + D3apo,
                                      data=., na.action=na.omit))


inv_fun <- function(x) x

BikeTFModel_df <- mm_df %>%
  add_formulas(fmlas1) %>%
  mutate(
    model = map2(train, formula, est_model),
    pseudo.r2 = map_dbl(model, calc_pseudo.r2),
    preds = map2(model, test, predict, type="response"),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="ntrips.Bike"), calc_rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I()

BikeTFModel_df$model %>% map(summary)
BikeTFModel_df$post_func <- list(inv_fun)

fmlas2 <- list(    metro   =~lm(log(atd.miles.Bike) ~ AADVMT + VehPerDriver + Age0to14 +
                                  Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                  D1B + D3bmm4 + D5cri + Tranmilescap + Tranmilescap:D4c,
                                data=., subset=(atd.miles.Bike > 0), na.action=na.omit),
                   non_metro=~lm(log(atd.miles.Bike) ~ AADVMT + Age0to14 +
                                   Age65Plus + LogIncome + LIF_CYC + D2A_JPHH +
                                   D1B + D5cri,
                                 data=., subset=(atd.miles.Bike > 0), na.action=na.omit)
)

inv_fun <- function(x) exp(x)

BikeTLModel_df <- mm_df %>%
  add_formulas(fmlas2) %>%
  mutate(
    model = map2(train, formula, est_model),
    preds = map2(model, test, predict, type="response"),
    preds = map(preds, inv_fun),
    r2 = map_dbl(map(model, summary), "r.squared"),
    #nrmse = map2_dbl(preds, map(test, resample_get, col_name="td.miles.Bike"), calc_nrmse),
    rmse = map2_dbl(preds, map(test, resample_get, col_name="atd.miles.Bike"), calc_rmse)
    #rmse = map2_dbl(model, test, modelr::rmse)
  ) %>%
  dplyr::select(-c(test, train)) %>%
  I() #name_list.cols()

BikeTLModel_df$model %>% map(summary)
BikeTLModel_df$post_func <- list(inv_fun)

BikeTFLModel_df <- bind_rows(BikeTFModel_df %>% mutate(step=1),
                             BikeTLModel_df %>% mutate(step=2)) %>%
  select(metro, step, model, post_func) %>%
  mutate(model=map(model, trim_model))

devtools::use_data(BikeTFLModel_df, overwrite = TRUE)
