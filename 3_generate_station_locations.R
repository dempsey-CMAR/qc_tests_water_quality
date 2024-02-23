# Decmber 5, 2022

# This script looks at all of the data submitted to Open Data Portal and:
## makes a table with the number of observations for each county (useful as a check to make sure Open Data
### uploaded all data);
## extracts unique stations and coordinates

# As the data submitted increases this will take a long time to run


library(dplyr)
library(sensorstrings)
library(readr)

library(purrr)

# Paths to data for each county ------------------------------------------------------------

path_submission <- "R:/data_branches/water_quality/open_data"

# counties submitted to Open Data
counties <- "all"

# Import county data ------------------------------------------------------

dat <- ss_import_data()

# # check if there are any longitudes missing the "-"
dat %>%
  filter(longitude > 0) %>%
  distinct(station)

# count the number of rows for each county (send to Open Data so they can check)
n_obs <- dat %>%
  group_by(county) %>%
  summarize(n_Observations= n())

# distinct stations with lat/long to send to Open Data
stations <- dat %>%
  distinct(station, .keep_all = TRUE) %>%
  select(county, waterbody, station, latitude, longitude) %>%
  arrange(county, waterbody)


# EXPORT ------------------------------------------------------------------

date_today <- Sys.Date()
write_csv(
  stations, paste0(path_submission, "/", date_today, "_station_locations.csv")
)

write_csv(
  n_obs,
  file = paste0(path_submission, "/", date_today, "_number_rows.csv")
)
