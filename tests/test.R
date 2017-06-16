#Test VETravelDemand module
library(visioneval)

TestDir <- normalizePath(".")
if (!endsWith(TestDir, 'tests'))
  TestDir <- file.path(TestDir, 'tests')

#Test PredictVehicles module
testModule(
  ModuleName = "VETravelDemand::PredictVehicles",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

#Test PredictDrivers module
testModule(
  ModuleName = "VETravelDemand::PredictDrivers",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

# #Test PredictAADVMT module
# testModule(
#   ModuleName = "VETravelDemand::PredictAADVMT",
#   ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
#
# #Test PredictBikeTFL module
# testModule(
#   ModuleName = "VETravelDemand::PredictBikeTFL",
#   ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
#
# #Test PredictWalkTFL module
# testModule(
#   ModuleName = "VETravelDemand::PredictWalkTFL",
#   ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
#
# #Test PredictTransitTFL module
# testModule(
#   ModuleName = "VETravelDemand::PredictTransitTFL",
#   ProjectDir = TestDir,
#   ParamDir = "defs",
#   LoadDatastore = TRUE,
#   SaveDatastore = TRUE,
#   DoRun = TRUE
# )
