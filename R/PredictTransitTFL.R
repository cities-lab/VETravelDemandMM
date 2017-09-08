#================== PredictTransitTFL.R ==================
#This module predicts Transit trip frequency and average trip length for
#households with households & locational characteristics (5Ds)

library(visioneval)

# model specification (model object) is to be read from RData

## TODO: revise data specifications
#================================================
#SECTION 1: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
PredictTransitTFLSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify input data
  Inp = items(),

  #Specify data to be loaded from data store
  Get = items(
    item(
      NAME =
        items("HHSIZE",
              "WRKCOUNT",
              "Age65Plus"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "persons",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME =
        items("Income"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      #UNITS = "persons",   #?
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = items("CENSUS_R"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = items("TRPOPDEN"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      #UNITS = "persons",   #?
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = items("ZeroVeh"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      #UNITS = "persons",   #?
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    )
  ),

  #Specify data to saved in the data store
  Set = items(
    item(
      NAME = "ntrips_Transit",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "trips",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    item(
      NAME = "TripDistance_Transit",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      UNITS = "mile",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    item(
      NAME = "HhId",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "integer",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    )
  )
)

#Save the data specifications list
#---------------------------------
#' Specifications list for PredictTransitTFL module
#'
#' A list containing specifications for the PredictTransitTFL module.
#'
#' @format A list containing 4 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Inp}{scenario input data to be loaded into the datastore for this
#'  module}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source PredictTransitTFL.R script.
"PredictTransitTFLSpecifications"
devtools::use_data(PredictTransitTFLSpecifications, overwrite = TRUE)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================

# Main module function that predicts total person miles traveled by Transit
# for households
#------------------------------------------------------
#' Main module function
#'
#' \code{PredictTransitTFL} predicts person miles traveled by Transit for each
#' household in the households dataset using independent variables including
#' household characteristics and 5D built environment variables.
#'
#' This function predicts person miles traveled by Transit for each hosuehold in
#' the model region. The model objects as a part of the inputs are stored in
#' data frame with two columns: a column for segmentation (e.g., metro,
#' non-metro) and a 'model' column for model object (list-column data
#' structure). The function "nests" the households data frame into a list-column
#' data frame by segments and applies the generic predict() function for each
#' segment to predict trip frequency and length for each household. The vectors
#' of HhId and PMTTransit produced by the PredictTransitTFL function are to
#' be stored in the "Household" table.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module along with:
#' LENGTH: A named integer vector having a single named element, "Household",
#' which identifies the length (number of rows) of the Household table to be
#' created in the datastore.
#' SIZE: A named integer vector having two elements. The first element, "Azone",
#' identifies the size of the longest Azone name. The second element, "HhId",
#' identifies the size of the longest HhId.
#' @import tidyverse
#' @export
PredictTransitTFL <- function(L) {
  #TODO: get id_name from L or specification?
  dataset_name <- "Household"
  id_name <- "HhId"
  y_name <- "PMTTransit"

  D_df <- data.frame(L$Year[[dataset_name]])
  stopifnot("data.frame" %in% class(D_df))

  load("data/TransitTFLModel_df.rda")
  Model_df <- TransitTFLModel_df

  # find cols used for segmenting households ("metro" by default)
  SegmentCol_vc <- setdiff(names(Model_df), c("model", "step", "post_func", "bias_adj"))

  # segmenting columns must appear in D_df
  stopifnot(all(SegmentCol_vc %in% names(D_df)))

  DoPredictions(Model_df, D_df,
                 dataset_name, id_name, y_name, SegmentCol_vc)
}
