install.packages("remotes")
library(remotes)
install_github("lifecycle-project/ds-upload")
library(dsUpload)

login_data <- data.frame(server = "https://armadillo.test.molgenis.org", storage = "https://armadillo-minio.test.molgenis.org")

du.login(login_data = login_data)

# based on dates
du.upload(
  cohort_id = "cohort1",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort1-data/core_1_0.csv",
  )

du.upload(
  cohort_id = "cohort1",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort1-data/outcome_1_0.csv",
)

du.upload(
  cohort_id = "cohort2",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort1-data/core_1_0.csv",
)

du.upload(
  cohort_id = "cohort2",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort1-data/outcome_1_0.csv",
)

du.upload(
  cohort_id = "cohort3",
  dict_kind = "core",
  data_version = "1_0",
  dict_version = "2_1", 
  data_input_path = "~/cohort3-data/core_1_0.csv",
)

du.upload(
  cohort_id = "cohort3",
  dict_kind = "outcome",
  data_version = "1_0",
  dict_version = "1_1", 
  data_input_path = "~/cohort3-data/outcome_1_0.csv",
)
