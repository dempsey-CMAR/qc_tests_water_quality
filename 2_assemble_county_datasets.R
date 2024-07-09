#################################
# Add check for duplicate rows!
###################################

library(dplyr)
library(purrr)
library(qaqcmar)
library(sensorstrings)

county <- "new_brunswick"

path <- file.path("R:/data_branches/water_quality/processed_data/qc_data")

dat <- qc_assemble_county_data(folder = county) %>%
  mutate(lease = if_else(lease == "NA" | lease == "na", NA_character_, lease))

unique(dat$waterbody)
unique(dat$station)
unique(dat$lease)

# open data portal (summary flags) -------------------------------------------
# remove the qc_test_variable columns (leaving only the max flag col)
rm_cols <- thresholds %>%
  distinct(qc_test, variable) %>%
  mutate(rm_cols = paste(qc_test, "flag", variable, sep = "_"))
rm_cols <- sort(c(
  rm_cols$rm_cols,
  "human_in_loop_flag_dissolved_oxygen_percent_saturation",
  "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
  "human_in_loop_flag_salinity_psu",
  "human_in_loop_flag_sensor_depth_measured_m",
  "human_in_loop_flag_temperature_degree_c")
)

dat %>%
  select(-county, -any_of(rm_cols)) %>%
  qc_assign_flag_labels() %>%
  ss_export_county_files(county = county, export_rds = FALSE)

# cmar county data (all flags) --------------------------------------------

# remove columns that are all NA
dat %>%
  select_if(~ !all(is.na(.))) %>%
  ss_export_county_files(county = county, export_csv = FALSE)

