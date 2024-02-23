library(here)
library(quarto)
library(rmarkdown)
library(sensorstrings)

path <- file.path("R:/data_branches/water_quality/processed_data/deployment_data")

county <- "inverness"

depls <- list.files(
  paste(path, county, sep = "/"),
  pattern = ".rds",
  full.names = TRUE
)


# export html file for each county showing the flagged observations
sapply(depls, function(x) {

  quarto_render(
    input = here("1_apply_qc_tests.qmd"),
    output_file = paste0(
      sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
      ".html"
    ),
    execute_params = list(county = county, depl_file = x))

})



x <- depls[1]
quarto_render(
  input = here("1_apply_qc_tests.qmd"),
  output_file = paste0(
    sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
    ".html"
  ),
  execute_params = list(county = county, depl_file = x))



