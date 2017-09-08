#================ PredictVehicles.R ================

#This module predicts Vehicles for households. It uses the model object in
#data/PredictVehicles_df.rda (ordered logit model by default) and variables and
#coefficients therein to predict Vehicles.

# Copyright [2017] [AASHTO]
# Based in part on works previously copyrighted by the Oregon Department of
# Transportation and made available under the Apache License, Version 2.0 and
# compatible open-source licenses.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#=============================================
#SECTION 1: ESTIMATE AND SAVE MODEL PARAMETERS
#=============================================
#The worker model predicts workers as a function of the household type:
#i.e. the number of persons in each of 6 age groups in the household. It is a
#simple stochastic model where the probability that a person in an age group in
#a household type is a Driver is the ratio of Vehicles to persons in that age
#group and household type. For the noninstitutionalized group quarters
#population, the probability is the ratio of Vehicles to persons by age group.


#================================================
#SECTION 2: DEFINE THE MODULE DATA SPECIFICATIONS
#================================================

#Define the data specifications
#------------------------------
PredictVehiclesSpecifications <- list(
  #Level of geography module is applied at
  RunBy = "Region",
  #Specify data to be loaded from data store
  Get = items(
    item(
      NAME =
        items("Age0to14",
              "HhSize",
              "Workers"),
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "people",
      UNITS = "PRSN",
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = ""
    ),
    item(
      NAME = "Income",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "currency",
      UNITS = "USD.1999",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    ),
    item(
      NAME = "LifeCycle",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "category",
      NAVALUE = -1,
      PROHIBIT = "",
      ISELEMENTOF = c("00", "01", "02", "03", "04", "09", "10"),
      SIZE = 2
    ),
    item(
      NAME = "HhId",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    ),
    item(
      NAME = "Azone",
      TABLE = "Azone",
      GROUP = "Year",
      TYPE = "character",
      UNITS = "ID",
      PROHIBIT = "",
      ISELEMENTOF = ""
    )
  ),
  #Specify data to saved in the data store
  Set = items(
    item(
      NAME = "Vehicles",
      TABLE = "Household",
      GROUP = "Year",
      TYPE = "vehicles",
      UNITS = "VEH",
      NAVALUE = -1,
      PROHIBIT = c("NA", "< 0"),
      ISELEMENTOF = "",
      SIZE = 0
    )
  )
)

#Save the data specifications list
#---------------------------------
#' Specifications list for PredictVehicles module
#'
#' A list containing specifications for the PredictVehicles module.
#'
#' @format A list containing 3 components:
#' \describe{
#'  \item{RunBy}{the level of geography that the module is run at}
#'  \item{Get}{module inputs to be read from the datastore}
#'  \item{Set}{module outputs to be written to the datastore}
#' }
#' @source PredictVehicles.R script.
"PredictVehiclesSpecifications"
devtools::use_data(PredictVehiclesSpecifications, overwrite = TRUE)
rm(PredictVehiclesSpecifications)


#=======================================================
#SECTION 3: DEFINE FUNCTIONS THAT IMPLEMENT THE SUBMODEL
#=======================================================
#This function predicts the number of Vehicles in each of 6 age groups in each
#household and tallies the total number of Vehicles in the household. It uses
#the matrix of Driver proportions by household type and age group as choice
#probabilities for determining the number of Vehicles by age group for each
#household.

#Main module function that predicts Vehicles by age for each household
#--------------------------------------------------------------------
#' Main module function to predict Vehicles by age for each household
#'
#' \code{PredictVehicles} predicts the number of Vehicles by age group for each
#' household and tallies the total number of Vehicles for each household.
#'
#' This function predicts the number of Vehicles for each household. Household
#' Vehicles are assigned to 5 age groups: 15-19, 20-29, 30-54, 55-64, and 65Plus.
#'
#' @param L A list containing the components listed in the Get specifications
#' for the module.
#' @return A list containing the components specified in the Set
#' specifications for the module.
#' @import visioneval
#' @import tidyverse
#' @importFrom MASS polr
#' @export
PredictVehicles <- function(L) {
  dataset_name <- "Household"
  id_name <- "HhId"
  y_name <- "Vehicles"

  D_df <- data.frame(L$Year[[dataset_name]])
  stopifnot("data.frame" %in% class(D_df))

  D_df <- D_df %>%
    mutate(LogIncome=log1p(Income),
           DrvAgePop=HhSize - Age0to14,
           LifeCycle = as.character(LifeCycle),
           LifeCycle = ifelse(LifeCycle=="01", "Single", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("02"), "Couple w/o children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("00", "03", "04", "05", "06", "07", "08"), "Couple w/ children", LifeCycle),
           LifeCycle = ifelse(LifeCycle %in% c("09", "10"), "Empty Nester", LifeCycle)
           )

  #load("data/VehiclesModel_df.rda")
  Preds_lcdf <- VehiclesModel_df

  Preds_lcdf$data <- list(D_df)

  Preds_lcdf <- Preds_lcdf %>%
    mutate(y = map2(model, data, ~predict(.x, .y)))

  # call post_func(y) if post_func column exists
  if ("post_func" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, post_func, `.y(.x)`))
  }

  if ("bias_adj" %in% names(Preds_lcdf)) {
    Preds_lcdf <- Preds_lcdf %>%
      mutate(y=map2(y, bias_adj, `*`))
  }

  Preds_df <- Preds_lcdf %>%
    mutate(id=map(data, id_name)) %>%
    unnest(id, y)

  Out_ls <- initDataList()
  Out_ls$Year$Household <-
    list(
      Vehicles = -1
  )
  Out_ls$Year$Household$Vehicles <- Preds_df[["y"]]

  #Return the list
  Out_ls
}


#====================
#SECTION 4: TEST CODE
#====================
#The following code is useful for testing and module function development. The
#first part initializes a datastore, loads inputs, and checks that the datastore
#contains the data needed to run the module. The second part produces a list of
#the data the module function will be provided by the framework when it is run.
#This is useful to have when developing the module function. The third part
#runs the whole module to check that everything runs correctly and that the
#module outputs are consistent with specifications. Note that if a module
#requires data produced by another module, the test code for the other module
#must be run first so that the datastore contains the requisite data. Also note
#that it is important that all of the test code is commented out when the
#the package is built.

#1) Test code to set up datastore and return module specifications
#-----------------------------------------------------------------
#The following commented-out code can be run to initialize a datastore, load
#inputs, and check that the datastore contains the data needed to run the
#module. It return the processed module specifications which can be used in
#conjunction with the getFromDatastore function to fetch the list of data needed
#by the module. Note that the following code assumes that all the data required
#to set up a datastore are in the defs and inputs directories in the tests
#directory. All files in the defs directory must have the default names.
#
# Specs_ls <- testModule(
#   ModuleName = "PredictVehicles",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = FALSE
# )
#
#2) Test code to create a list of module inputs to use in module function
#------------------------------------------------------------------------
#The following commented-out code can be run to create a list of module inputs
#that may be used in the development of module functions. Note that the data
#will be returned for the first year in the run years specified in the
#run_parameters.json file. Also note that if the RunBy specification is not
#Region, the code will by default return the data for the first geographic area
#in the datastore.
#
# setwd("tests")
# Year <- getYears()[1]
# if (Specs_ls$RunBy == "Region") {
#   L <- getFromDatastore(Specs_ls, RunYear = Year, Geo = NULL)
# } else {
#   GeoCategory <- Specs_ls$RunBy
#   Geo_ <- readFromTable(GeoCategory, GeoCategory, Year)
#   L <- getFromDatastore(Specs_ls, RunYear = Year, Geo = Geo_[1])
#   rm(GeoCategory, Geo_)
# }
# rm(Year)
# setwd("..")

#3) Test code to run full module tests
#-------------------------------------
#Run the following commented-out code after the module functions have been
#written to test all aspects of the module including whether the module can be
#run and whether the module will produce results that are consistent with the
#module's Set specifications. It is also important to run this code if one or
#more other modules in the package need the dataset(s) produced by this module.
#
# testModule(
#   ModuleName = "PredictVehicles",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )

