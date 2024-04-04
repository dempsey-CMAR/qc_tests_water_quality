# March 8, 2024
# assign human in the loop flags

apply_human_in_loop_flags <- function(dat) {

  station <- unique(dat$station)
  depl_range <- unique(dat$deployment_range)


# Antigonish --------------------------------------------------------------

  if(station == "Antigonish 1" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # sensor was exposed
          # upgrade rolling sd flags from 3 to 4
          (variable == "temperature_degree_c" &
             sensor_serial_number == 20291452 &
             timestamp_utc <= as_datetime("2022-07-01") &
             rolling_sd_flag_value == 3),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Antigonish 2" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # sensors were exposed
          # upgrade rolling sd flags from 3 to 4
          variable == "temperature_degree_c" &
            (timestamp_utc <= as_datetime("2022-07-10") |
               timestamp_utc >= as_datetime("2022-08-01")) &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Antigonish 3" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # sensors were exposed
          # upgrade rolling sd flags from 3 to 4
          variable == "temperature_degree_c" &
            (timestamp_utc <= as_datetime("2022-07-10") |
               timestamp_utc >= as_datetime("2022-08-01")) &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Ile du Havre" & depl_range == "2018-Jul-24 to 2018-Oct-25") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade climatology flags from 3 to 4
          variable == "temperature_degree_c" &
            timestamp_utc <= as_datetime("2018-08-12 20:45:00") &
               timestamp_utc >= as_datetime("2018-08-12 19:30:00") &
            climatology_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Tracadie 1" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade rolling sd flags from 3 to 4
          # sensor was likely exposed
          variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2022-06-16 10:45:00") &
            timestamp_utc <= as_datetime("2022-06-17 01:00:00") &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Tracadie 2" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade rolling sd flags from 3 to 4
          # sensor was likely exposed
          variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2022-06-16 09:30:00") &
            timestamp_utc <= as_datetime("2022-06-17 09:30:00") &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }


  if(station == "Tracadie 3" & depl_range == "2022-Jun-08 to 2022-Sep-21") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      # only observations not flagged
      mutate(
        human_in_loop_flag_value = if_else(
          variable == "dissolved_oxygen_percent_saturation" &
            rolling_sd_flag_value == 1,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }



# Colchester --------------------------------------------------------------

  if(station == "McNabs Point 1" & depl_range == "2022-Jun-08 to 2022-Sep-20") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade rolling sd flags from 3 to 4
          # sensor was likely exposed
          variable == "temperature_degree_c" &
            timestamp_utc <= as_datetime("2022-07-01") &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "McNabs Point 2" & depl_range == "2022-Jun-08 to 2022-Sep-20") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade rolling sd flags from 3 to 4
          # sensor was likely exposed
          variable == "temperature_degree_c" &
            rolling_sd_flag_value == 3 &
            (timestamp_utc <= as_datetime("2022-07-15") |
            (timestamp_utc >= as_datetime("2022-08-01") &
               timestamp_utc <= as_datetime("2022-08-15"))),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "McNabs Point 3" & depl_range == "2022-Jun-08 to 2022-Sep-20") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # upgrade rolling sd flags from 3 to 4
          # sensor was likely exposed
          variable == "temperature_degree_c" &
            rolling_sd_flag_value == 3 &
            timestamp_utc <= as_datetime("2022-08-15"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }



# Digby -------------------------------------------------------------------

  if(station == "Sissiboo" & depl_range == "2021-Aug-05 to 2021-Nov-08") {

    # looks like sensor was exposed
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          (variable == "temperature_degree_c" & rolling_sd_flag_value == 3 &
            (between(timestamp_utc,
                     as_datetime("2021-08-19 06:17:33"),
                     as_datetime("2021-08-25 16:57:34")) |

               between(timestamp_utc,
                       as_datetime("2021-09-17 06:37:38"),
                       as_datetime("2021-09-23 22:17:39")) |

               between(timestamp_utc,
                       as_datetime("2021-10-03 19:37:40"),
                       as_datetime("2021-10-10 23:37:42"))
            )) |

            (variable == "dissolved_oxygen_percent_saturation" &
               (between(timestamp_utc,
                       as_datetime("2021-08-25 23:07:34"),
                       as_datetime("2021-08-26 01:17:43")) |

               between(timestamp_utc,
                       as_datetime("2021-09-07 20:07:36"),
                       as_datetime("2021-09-08 02:47:36")) |

               between(timestamp_utc,
                       as_datetime("2021-09-08 21:27:36"),
                       as_datetime("2021-09-09 03:37:36")) |

               between(timestamp_utc,
                       as_datetime("2021-09-13 00:07:37"),
                       as_datetime("2021-09-13 04:57:37")) |

               timestamp_utc == as_datetime("2021-09-20 09:47:38") |

               between(timestamp_utc,
                       as_datetime("2021-10-18 19:27:43"),
                       as_datetime("2021-10-18 21:47:43")) |

               between(timestamp_utc,
                       as_datetime("2021-11-01 17:47:45"),
                       as_datetime("2021-11-01 20:27:45")) |

               between(timestamp_utc,
                       as_datetime("2021-11-02 05:57:45"),
                       as_datetime("2021-11-02 08:17:45"))
            )),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

# Halifax -----------------------------------------------------------------

  if(station == "Beaver Point" & depl_range == "2019-Apr-12 to 2019-Oct-19") {
    # all DO data was ERR;
    # most temperature data technically passes the other tests,
    # but clearly does not match signals of the other depths
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          sensor_serial_number == 670381 & variable == "temperature_degree_c",
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()

  }


  if(station == "Sober Island" & depl_range == "2022-Sep-13 to 2023-Nov-14") {
    # most dissolved oxygen and temperature obs are flagged
    # temperature sensor failed the post-deployment validation test
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(sensor_serial_number == 670376, 4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

# Guysborough -------------------------------------------------------------

  if(station == "Fishermans Island" & depl_range == "2022-Sep-12 to 2023-Nov-14") {
    # sub-surface buoy was floating on retrieval
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
            #timestamp_utc >= as_datetime("2023-09-17 22:01:34"),
            timestamp_utc >= as_datetime("2023-08-28"),
            4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "White Island" & depl_range == "2016-Jun-07 to 2018-Jul-11") {
    # looks like string was exposed
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          sensor_serial_number == 10817410 &
            variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2018-03-27 07:00:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "White Island" & depl_range == "2018-Oct-26 to 2021-Sep-08") {
    # looks like string was exposed
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          sensor_serial_number == 10755218 &
            variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2019-06-23 02:00:00") &
            timestamp_utc <= as_datetime("2019-08-23 18:00:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }


  if(station == "Wine Harbour" & depl_range == "2019-Apr-22 to 2020-Sep-10") {
    # looks like the string was picked up and replaced
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2020-05-11 15:00:00") &
            timestamp_utc <= as_datetime("2020-05-11 16:15:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Wine Harbour" & depl_range == "2022-Nov-04 to 2023-May-31") {
    # looks like the string was picked up and replaced
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          variable == "temperature_degree_c" &
            timestamp_utc >= as_datetime("2023-04-07 16:45:00") &
            timestamp_utc <= as_datetime("2023-04-07 19:00:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "The Bull" & depl_range == "2022-Jul-15 to 2023-Oct-18") {
    # primary buoy was cut and string was retrieved from bottom buoy
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
            timestamp_utc >= as_datetime("2023-09-16 04:05:05"), 4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

# Lunenburg ---------------------------------------------------------------
  if(station == "Flat Island" & depl_range == "2022-Nov-19 to 2023-Jan-10") {
    # string was entangled in lobster gear and then picked up by fisherman
    # all data should be marked Fail
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = 4,
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }


# Pictou -----------------------------------------------------------------

  if(station == "Olding Island" & depl_range == "2022-Jun-03 to 2022-Sep-30") {
    # sensor malfunctioned for entire depl
    # most obs already flagged as Fail
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          variable == "sensor_depth_measured_m" &
            sensor_serial_number == 675008,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Roy Island 1" & depl_range == "2022-Jun-07 to 2022-Sep-21") {
    # one ons in spike flagged as 3 but should be 4
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          variable == "temperature_degree_c" &
            sensor_serial_number == 21082882 &
            timestamp_utc == as_datetime("2022-06-12 19:00:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Roy Island 2" & depl_range == "2022-Jun-07 to 2022-Sep-20") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          # only observations not flagged by rolling_sd test
          (variable == "dissolved_oxygen_percent_saturation" &
            timestamp_utc >= as_datetime("2022-06-14 22:16:00") &
            timestamp_utc <= as_datetime("2022-06-15 05:16:00")) |

            # upgrade rolling sd flags from 3 to 4
            (variable == "temperature_degree_c" &
               sensor_serial_number == 670379 &
               timestamp_utc <= as_datetime("2022-06-17 11:25:00") &
               rolling_sd_flag_value == 3) |

            #
            (variable == "temperature_degree_c" &
               sensor_serial_number == 21083048 &
               timestamp_utc >= as_datetime("2022-06-12 18:30:00") &
               timestamp_utc <= as_datetime("2022-06-12 20:15:00")) |

            (variable == "temperature_degree_c" &
               sensor_serial_number == 21083048 &
               timestamp_utc >= as_datetime("2022-06-15 21:45:00") &
               timestamp_utc <= as_datetime("2022-06-15 22:45:00")) |

            (variable == "temperature_degree_c" &
               sensor_serial_number == 21083048 &
               timestamp_utc >= as_datetime("2022-06-16 21:30:00") &
               timestamp_utc <= as_datetime("2022-06-16 23:30:00")),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Roy Island 3" & depl_range == "2022-Jun-07 to 2022-Sep-20") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(

          # upgrade rolling sd flags from 3 to 4
          variable == "temperature_degree_c" &
            sensor_serial_number == 680158 &
            timestamp_utc >= as_datetime("2022-06-12 06:18:00") &
            timestamp_utc <= as_datetime("2022-06-17 11:25:00") &
            rolling_sd_flag_value == 3,
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }



  # Richmond ----------------------------------------------------------------
  if(station == "0667" & depl_range == "2015-Nov-26 to 2016-Sep-01") {
    # spike at all depths; partially flagged
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          between(timestamp_utc,
                  as_datetime("2016-08-19 13:00:00"),
                  as_datetime("2016-08-19 18:00:00")),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }

  if(station == "Cape Auguet Bay" & depl_range == "2021-Jun-14 to 2022-Jun-02") {

    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        # looks like sensor was removed; some values already flagged
        human_in_loop_flag_value = if_else(
          variable == "temperature_degree_c" &
            (between(timestamp_utc,
                     as_datetime("2021-11-03 16:17:00"),
                     as_datetime("2021-11-08 14:55:00")) |

               between(timestamp_utc,
                       as_datetime("2022-05-11 12:29:00"),
                       as_datetime("2022-05-11 13:19:00"))),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }


# Shelburne ---------------------------------------------------------------
  if(station == "Taylors Rock" & depl_range == "2018-May-14 to 2019-Jun-28") {
    # looks like sensor was removed
    dat <- dat %>%
      qc_pivot_longer(qc_tests = qc_tests) %>%
      mutate(
        human_in_loop_flag_value = if_else(
          (variable == "temperature_degree_c" &
                         sensor_serial_number == 20291486 &
                         timestamp_utc >= as_datetime("2018-11-30 16:15:00") &
                         timestamp_utc <= as_datetime("2018-11-30 17:15:00")) |

            (variable == "temperature_degree_c" &
                           sensor_serial_number == 20291486 &
                           timestamp_utc == "2019-01-13 21:15:00"),
          4, 1),
        human_in_loop_flag_value = ordered(human_in_loop_flag_value, levels = 1:4)
      ) %>%
      qc_pivot_wider()
  }



# Export ------------------------------------------------------------------

  dat

}
