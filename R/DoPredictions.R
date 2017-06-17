#------------------------------------------------------
#' run predictions on Dataset_df with model objects in Model_df
#'
#' \code{do_predictions} predicts outcomes (y) for each observation in the Dataset_df
#' dataset using independent variables included in Dataset_df and model objects saved
#' in list-column data_frame Model_df.
#'
#' @param Model_df A list-column data_frame containing a 'model' list-column, column(s) used
#' for segmenting data, and, optionally, a 'step' column and a 'post_func' for multi-step models
#' and post-processing function for predictions.
#' @param Dataset_df A data frame providing independent variables and segmentation column for predictions
#' included in the specification of the model objects.
#' @param dataset_name A character string for name of the Dataset_df
#' @param id_name A character string for the id column of Dataset_df
#' @param y_name A character string for name of the outcome variable
#' @param SegmentCol_vc A vector for columns used for segmentation; if NULL assuming columns other
#' than c("model", "step", "post_func", "bias.adj") in Model_df is used for segmentation
#' @return A list containing the components specified in the Set
#' specifications for the module along with:
#' LENGTH: A named integer vector having a single named element, "Household",
#' which identifies the length (number of rows) of the Household table to be
#' created in the datastore.
#' SIZE: A named integer vector having two elements. The first element, "Azone",
#' identifies the size of the longest Azone name. The second element, "HhId",
#' identifies the size of the longest HhId.
#' @import tidyverse
#' @importFrom splines ns
#' @export
#'
do_predictions <- function(Model_df, Dataset_df,
                           dataset_name, id_name, y_name, SegmentCol_vc=NULL) {
  #create household list-column data_frame and join with Model_df
  if (is.null(SegmentCol_vc)) {
    SegmentCol_vc <- setdiff(names(Model_df), c("model", "step", "post_func", "bias_adj"))
    if (length(SegmentCol_vc)==0) SegmentCol_vc <- NULL
  }

  if (!is.null(SegmentCol_vc)) {
    Dataset_lcdf <- Dataset_df %>%
      group_by_(SegmentCol_vc) %>%
      nest() %>%
      left_join(Model_df, by=SegmentCol_vc)
  } else { # if there is no segmentation in model(s)
    Dataset_lcdf <- tibble(data=list(Dataset_df)) %>%
      crossing(Model_df)
  }

  Preds_lcdf <- Dataset_lcdf %>%
    mutate(y=map2(model, data, predict, type="response"))

  # call post_func(y) if post_func column exists
  if ("post_func" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, post_func, `.y(.x)`))
  }

  if ("bias_adj" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, bias_adj, `*`))
  }

  # combine predictions if a model has more than 1 modeling steps (e.g. 1.
  # zero VMT model + 2. non-zero VMT regression model). For now,
  # combine_preds multiplies predictions from each step, it's possible to pass
  # other functions to it
  if ("step" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      arrange(step) %>%
      group_by_(SegmentCol_vc) %>%
      summarize(data=list(first(data)),
                y=combine_preds(y)) %>%
      ungroup()
  }

  Preds_lcdf %>%
    mutate(id=map(data, id_name)) %>%
    unnest(id, y)

}

#' internal function that handles pass a list column of a data frame to another
#' list column as argument.
#' Example:
#' lcdf <- mtcars %>% nest(-cyl)
#' lcdf$fmla <- list(~lm(mpg~wt, data=.x))
#' lcdf <- lcdf %>% mutate(fit=map2(data, fmla, `.y(.x)`))
#' lcdf$fit %>% map(summary)
#' @import tidyverse
`.y(.x)` <- function(.x, .y, ...) {
  at_depth(.x, 0, .y, ...)
  #map(list(.x), .y, ...)[[1]]
}

#'
#' Used to combine predictions from multiple models:
#'  for example combine GreenSTEP zero dvmt glm model and dvmt lm model
#'  Example (a clumsy way to square mpg):
#'  mtcars %>%
#'    nest(-cyl) %>%
#'    crossing(tibble(x=c(1, 2))) %>%
#'    mutate(mpg=map(data, "mpg")) %>%
#'    group_by(cyl) %>%
#'    summarise(mpg1 = list(mpg[[1]]),
#'              mpg2 = combine_preds(mpg)) %>%
#'    mutate(test=map2_lgl(mpg1, mpg2, ~all(.y==.x^2)))
#'
combine_preds <- function(preds_ls, func=`*`, init=1.0, ...) {
  list( reduce(preds_ls, `*`, ..., .init=init) )
}
