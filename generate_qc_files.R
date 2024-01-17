library(here)
library(quarto)
library(rmarkdown)
library(sensorstrings)

path <- file.path("R:/data_branches/water_quality/processed_data/deployment_data")

county <- "annapolis"

depls <- list.files(
  paste(path, county, sep = "/"),
  full.names = TRUE
)

# export html file for each county showing the flagged observations
sapply(depls, function(x) {

  rmarkdown::render(
    input = here("R/apply_qc_tests.rmd"),
    output_file = paste0(
      here("html_output"), "/", county, "/",
      sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
      ".html"
    ),
    params = list(county = county, depl_file = x))

})

x <- depls[6]
rmarkdown::render(
  input = here("R/apply_qc_tests.rmd"),
  output_file = paste0(
    here("html_output"), "/", county, "/",
    sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
    ".html"
  ),
  params = list(county = county, depl_file = x))





# export html file for each county showing the flagged observations
sapply(depls, function(x) {

  quarto_render(
    input = here("R/apply_qc_tests.qmd"),
    output_file = paste0(
      sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
      ".html"
    ),
    params = list(county = county, depl_file = x))

})

x <- depls[6]
quarto_render(
  input = here("apply_qc_tests.qmd"),
  output_file = paste0(
    sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
    ".html"
  ),
  execute_params = list(county = county, depl_file = x))



