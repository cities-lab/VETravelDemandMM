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

#Test PredictAADVMT module
testModule(
  ModuleName = "VETravelDemand::PredictAADVMT",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

#Test PredictBikePMT module
testModule(
  ModuleName = "VETravelDemand::PredictBikePMT",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

#Test PredictWalkPMT module
testModule(
  ModuleName = "VETravelDemand::PredictWalkPMT",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)

#Test PredictTransitPMT module
testModule(
  ModuleName = "VETravelDemand::PredictTransitPMT",
  ProjectDir = TestDir,
  ParamDir = "defs",
  LoadDatastore = TRUE,
  SaveDatastore = TRUE,
  DoRun = TRUE
)
