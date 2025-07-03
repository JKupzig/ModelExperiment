rm(list=ls())

source("./src/helper/comparison.r")

ROOT <- "data"
SIM_DISCHARGE_FILE <- "data - Kopie/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt"
OBS_DISCHARGE_FILE <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten\%s_Q_Day.Cmd.txt)"

END_CAL <- as.Date("1988-12-31")


result_array <- array(NA, dim=c(1, 10, 330))

get_periods <- function(x) {
  x <- base::rle(as.vector(x))
  lens <- x$lengths
  ends <- cumsum(lens)[x$values]
  starts <- ends - lens[x$values] + 1L
  data.frame(
    period = seq_along(starts),
    period_start = starts,
    period_end = ends
  )
}

Q.__calc_mgn_h_1_adopted__ <- function(discharge, add_args = NULL) {
  return(stats::quantile(discharge, 0.99))
}

Q.__calc_no_dur_l_1__ <- function(discharge, additonal_args) {
  if (is.list(additonal_args)){
    complete_discharge <- additonal_args[["discharge"]]
    minimal_length <- additonal_args[["minimal_length"]]
  } else {
    complete_discharge <- additonal_args
    minimal_length <- 0
  }

  threshold <- mean(complete_discharge) * 0.2
  idx <- (discharge < threshold)
  df_periods <- get_periods(idx)
  df_periods$delta <- df_periods$period_end - df_periods$period_start + 1

  df_periods <- df_periods[df_periods$delta > minimal_length,]

  return(length(df_periods$delta))
}

Q.__calc_no_dur_h_1__ <- function(discharge, complete_discharge) {
  threshold <- stats::median(complete_discharge) * 9
  idx <- (discharge > threshold)
  df_periods <- get_periods(idx)
  df_periods$delta <- df_periods$period_end - df_periods$period_start
  return(length(df_periods$delta))
}


info_basins <- watergap3data::txt.read_station_list("data - Kopie/STATION_LIST.OUT")


ref_simulated_data <- read.table(SIM_DISCHARGE_FILE,
                             sep="\t", header=T,
                             colClasses=c("date"="Date"))

run <- 1
for (station in 1:330){

  column_name <- names(ref_simulated_data)[station]
  station_id <- substr(column_name, 2, 10)
  basin_area <- info_basins$basin_size[info_basins$stations ==station_id]

  ref_station_data = data.frame(Date=ref_simulated_data$date,
                                Sim=ref_simulated_data[[column_name]])


  obs_data = read.table(sprintf(OBS_DISCHARGE_FILE, station_id),
                        skip=36, header=T, sep=";",strip.white=T)
  obs_data$Date <- as.Date(obs_data$YYYY.MM.DD, "%Y-%m-%d")
  obs_data$Sim <- obs_data$Value
  obs_data_val <- obs_data[obs_data$Date > END_CAL & obs_data$Date <= max(ref_station_data$Date), ]

  maximal_flood <- WaterGAPLite::Q.calcSI(
    df=obs_data_val,
    func_name="max",
    add_args = NULL)

  result_array[run, 10, station] <- maximal_flood
  ###########

  magnitude_lowflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_mgn_l_1__") # #5th percentile Addor 2018 m3/s

  result_array[run, 1, station] <- magnitude_lowflow / basin_area * 3.6 * 24 #mm/day
  ###########

  magnitude_average <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_mgn_a_2__",
    add_args = basin_area)

  result_array[run, 2, station] <- magnitude_average
  ###########

  magnitude_highflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_mgn_h_1_adopted__")  #99th (former 95th) percentile Addor 2018

  result_array[run, 3, station] <- magnitude_highflow / basin_area * 3.6 * 24 #mm/d
  ###########

  frequency_lowflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_frq_l_1__",
    add_args = ref_station_data$Sim) # Frequency of low-flow days (<0.2 times the mean daily flow) Addor 2018

  result_array[run, 4, station] <- frequency_lowflow
  ###########

  # Frequency of high-flow days (>9 times the median daily flow) Addor 2018
  frequency_highflow <- WaterGAPLite::Q.calcSI(
    df=obs_data_val,
    func_name="Q.__calc_frq_h_1__",
    add_args = ref_station_data$Sim)

  result_array[run, 5, station] <- frequency_highflow
  ###########

  # number of consecutive days <0.2 times the mean daily flow Addor 2018
  duration_lowflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_dur_l_1__",
    add_args = list("discharge"= ref_station_data$Sim,
                    "minimal_length"=7))

  result_array[run, 6, station] <- duration_lowflow
  ###########

  # description number of consecutive days >9 times the median daily flow), Addor 2018
  duration_highflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_dur_h_1__",
    add_args = ref_station_data$Sim)

  result_array[run, 7, station] <- duration_highflow

  # number of periods consecutive days <0.2 times the mean daily flow Addor 2018
  no_lowflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_no_dur_l_1__",
    add_args = list("discharge"= ref_station_data$Sim,
                    "minimal_length"=7))

  result_array[run, 8, station] <- no_lowflow

  # number of periods consecutive days <0.2 times the mean daily flow Addor 2018
  no_highflow <- WaterGAPLite::Q.calcSIComplete(
    df=obs_data_val,
    func_name="Q.__calc_no_dur_h_1__",
    add_args = ref_station_data$Sim)

  result_array[run, 9, station] <- no_highflow
}



dimnames(result_array) <- list("obs" ,
                               c("mgn_l", "mgn_a", "mgn_h", "frq_l", "frq_h", "dur_l", "dur_h",
                                 "max", "lowflow_events", "highflow_events"),
                               names(ref_simulated_data)[1:330])

ids2save <- get_sensitive_basins("reservoir")
result_array[,,dimnames(result_array)[[3]] %in% ids2save]

saveRDS(result_array, file.path(ROOT, "SI_obs_ref_as_m8_100d_complete_validation_period.rds"))
