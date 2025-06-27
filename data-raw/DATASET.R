## code to prepare `DATASET` dataset goes here
pip_off_folder_v2 <- "//w1wbgencifs01/pip/PIP_ingestion_pipeline_v2"

pip_off_v2_pass <- new_secret(Sys.getenv("PIP_OFF_FOLDER_v2_PASS"))


usethis::use_data(
  pip_off_folder_v2,
  pip_off_v2_pass,
  internal = TRUE,
  overwrite = TRUE
)

