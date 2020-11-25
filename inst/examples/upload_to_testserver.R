install.packages("remotes")
library(remotes)
install.packages("MolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R")
install.packages("DSMolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R")
install_github("lifecycle-project/ds-upload")
library(dsUpload)

login_data <- data.frame(server = "https://armadillo.test.molgenis.org", storage = "https://armadillo-minio.test.molgenis.org", driver = "ArmadilloDriver")

du.login(login_data = login_data)

du.upload(
  cohort_id = "cohort1",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort1-data/core_1_0.csv",
  run_mode = "test"
)

du.upload(
  cohort_id = "cohort1",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort1-data/outcome_1_0.csv",
  run_mode = "test"
)

du.upload(
  cohort_id = "cohort2",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort1-data/core_1_0.csv",
  run_mode = "test"
)

du.upload(
  cohort_id = "cohort2",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort1-data/outcome_1_0.csv",
  run_mode = "test"
)

du.upload(
  cohort_id = "cohort3",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort3-data/core_1_0.csv",
  run_mode = "test"
)

du.upload(
  cohort_id = "cohort3",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort3-data/outcome_1_0.csv",
  run_mode = "test"
)
