
rm(list = ls())

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

models <- seq(8, 19, 1)
all_basins <- read_kge_and_define_good_basins(0, NULL)

behavioural_basins <- read_kge_and_define_good_basins(0.4, NULL)
additional_basins <- read_kge_and_define_good_basins(0, 0.4)

ROOT_PATTERN <- "./data - Kopie/evaluation/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
TARGET_PATTERN <- "./data/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
for (model in models) {
  data_in <- read.csv(sprintf(ROOT_PATTERN, model), sep="\t")
  data_in_reduced <- data_in[data_in$station %in% all_basins$behavioural, ]
  # BUG in d1 calculation as 1- was forgotten
  data_in_reduced$d1_cal <- 1 - data_in_reduced$d1_cal
  data_in_reduced$d1_val <- 1 - data_in_reduced$d1_val
  rownames(data_in_reduced) <- 1:nrow(data_in_reduced)
  # Gamma value as integer not string
  data_in_reduced$best_gamma <- readr::parse_number(data_in_reduced$best_gamma) / 10.
  # delete (in paper) unused columns
  not_used_columns <- c("b_cv_cal", "b_cv_val", "bias_cal", "bias_val", "mean_obs_cal", "mean_obs_val", "d1_val", "d1_cal")
  data_in_reduced <- data_in_reduced[,!colnames(data_in_reduced) %in% not_used_columns]
  write.table(data_in_reduced, sprintf(TARGET_PATTERN, model), sep="\t")
}

ROOT_PATTERN <- "./data - Kopie/evaluation/cal_result_discharges_model_m%i_wetlStorage100.txt"
TARGET_PATTERN <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"
for (model in models) {
  data_in <- read.csv(sprintf(ROOT_PATTERN, model), sep="\t")
  data_in_reduced <- data_in[, colnames(data_in) %in% c(all_basins$behavioural, "date")]
  write.table(data_in_reduced, sprintf(TARGET_PATTERN, model), sep="\t")
}

ROOT_PATTERN <- "./data - Kopie/evaluation/cal_result_model_m%i_wetlStorage100.rds"
TARGET_PATTERN <- "./data/cal_result_model_m%i_wetlStorage100.rds"
for (model in models) {
  data_in <- readRDS(sprintf(ROOT_PATTERN, model))
  data_in_reduced <- data_in[,,, dimnames(data_in)[[4]] %in% all_basins$behavioural]
  # BUG in d1 calculation as 1- was forgotten
  data_in_reduced[1,dimnames(data_in_reduced)[[2]] == "d1",,] <- 1 - data_in_reduced[1,dimnames(data_in_reduced)[[2]] == "d1",,]
  data_in_reduced[2,dimnames(data_in_reduced)[[2]] == "d1",,] <- 1 - data_in_reduced[2,dimnames(data_in_reduced)[[2]] == "d1",,]
  # delete unused metrics
  not_used_columns <- c("b_cv", "bias", "mean_obs")
  data_in_reduced <- data_in_reduced[,!dimnames(data_in_reduced)[[2]] %in% not_used_columns,,]
  # rename gama_entries
  dimnames(data_in_reduced)[[3]] <- readr::parse_number(dimnames(data_in_reduced)[[3]]) / 10.
  saveRDS(data_in_reduced, sprintf(TARGET_PATTERN, model))
}

ROOT_PATTERN <- "./data - Kopie/evaluation/cal_result_model_m%i_uncertainty.rds"
TARGET_PATTERN <- "./data/cal_result_model_m%i_uncertainty.rds"
for (model in models) {
  data_in <- readRDS(sprintf(ROOT_PATTERN, model))
  data_in_reduced <- data_in[,,, dimnames(data_in)[[4]] %in% all_basins$behavioural]
  #data_in_reduced <- data_in_reduced[,,endsWith(dimnames(data_in_reduced)[[3]] , "wetlStorage100"),]
  # BUG in d1 calculation as 1- was forgotten
  data_in_reduced[1,dimnames(data_in_reduced)[[2]] == "d1",,] <- 1 - data_in_reduced[1,dimnames(data_in_reduced)[[2]] == "d1",,]
  data_in_reduced[2,dimnames(data_in_reduced)[[2]] == "d1",,] <- 1 - data_in_reduced[2,dimnames(data_in_reduced)[[2]] == "d1",,]
  # delete unused metrics
  not_used_columns <- c("b_cv", "bias", "mean_obs")
  data_in_reduced <- data_in_reduced[,!dimnames(data_in_reduced)[[2]] %in% not_used_columns,,]
  # rename uncertainty_entries
  dimnames(data_in_reduced)[[3]] <- c(
    "original_run", "x_p", "+x_hyd", "-x_hyd", "x_p & +x_hyd", "x_p & -x_hyd")
  saveRDS(data_in_reduced, sprintf(TARGET_PATTERN, model))
}

ROOT <- "./data - Kopie/SI_ref_as_m8_100d_complete_validation_period.rds"
TARGET <- "./data/SI_original.rds"
data_in <- readRDS(ROOT)
data_in_reduced <- data_in[,, dimnames(data_in)[[3]] %in% all_basins$behavioural]
data_in_reduced <- data_in_reduced[endsWith(dimnames(data_in_reduced)[[1]], "wetlStorage100"),,]
# delete unused metrics
not_used_columns <- c("monthly_mean", "monthly_pearson", "monthly_sd", "monthly_kge",
                       "mgn_a")
data_in_reduced <- data_in_reduced[,!dimnames(data_in_reduced)[[2]] %in% not_used_columns,]
saveRDS(data_in_reduced, TARGET)


ROOT_PATTERN <- "./data - Kopie/basin_attributes.txt"
TARGET_PATTERN <- "./data/basin_attributes.txt"
data_in <- read.csv(ROOT_PATTERN, sep="\t")
data_in$grdc_ids <- paste0("X", data_in$grdc_ids)
data_in_reduced <- data_in[data_in$grdc_ids %in% all_basins$behavioural, ]
# delete unused information
not_used_columns <- c("water_stress", "globalWetlands",
                      "localLakes", "open_water", "wetland", "lakes", "global_waterbodies",
                      "local_waterbodies", "batjes")
data_in_reduced <- data_in_reduced[, !colnames(data_in_reduced) %in% not_used_columns]
data_in_reduced$mean_precipitation_as_snow <- data_in_reduced$mean_precipitation_as_snow * 100
write.table(data_in_reduced, TARGET_PATTERN, sep="\t")

# prepare spatial data for plot - Figure 1
ROOT_SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2"
ROOT_STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"
TARGET <- "./data/overview_map.shp"
spatial_data <- watergap3data::unf.readunf(ROOT_SPATIAL_INFO, "na")
station_data <- watergap3data::txt.read_station_list(ROOT_STATION_INFO)
station_data <- station_data[paste0("X", station_data$stations) %in% all_basins$behavioural, ]
station_data$internal_ids <- as.integer(as.character(station_data$internal_ids))

spatial_data_to_plot <- matrix(data=NA, nrow=nrow(spatial_data), ncol=ncol(spatial_data))
pb <- txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in seq_along(station_data$stations)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])
  idx <- which(abs(spatial_data) == internal_id)

  if (grdc_id %in% behavioural_basins$behavioural){
    spatial_data_to_plot[idx] <- 1
  }

  if (grdc_id %in% additional_basins$behavioural){
    spatial_data_to_plot[idx] <- 2
  }

  setTxtProgressBar(pb, row)
}

spatial_layer_behavioural <- watergap3data::unf.vec2raster(as.vector(spatial_data_to_plot), 1, "na")
polygons_behavioural <- raster::rasterToPolygons(spatial_layer_behavioural,
                                                 na.rm=TRUE, dissolve=TRUE)
raster::shapefile(polygons_behavioural, filename=TARGET, overwrite=TRUE)


# prepare spatial data for Figure A1
ROOT_SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2"
ROOT_STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"
TARGET_1 <- "./data/basins.rds"
TARGET_2 <- "./data/calibration_result.shp"

kge_info <- read_benchmarks_all("KGE_cal")
spatial_data_basins <- watergap3data::unf.readunf(ROOT_SPATIAL_INFO, "na")
station_data <- watergap3data::txt.read_station_list(ROOT_STATION_INFO)

MAX_NUMBER_IN_GRID <- 1000
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])

  if (grdc_id %in% kge_info$station){
    grdc_id_as_integer <- as.integer(strsplit(grdc_id, "X")[[1]][2])
    spatial_data_basins[abs(spatial_data_basins) == as.integer(internal_id)] <- MAX_NUMBER_IN_GRID + grdc_id_as_integer
  }
  setTxtProgressBar(pb, row)
}

spatial_data_basins <- (spatial_data_basins - MAX_NUMBER_IN_GRID)
spatial_data_basins[spatial_data_basins < -100] <- NA

spatial_layer_basins <- watergap3data::unf.vec2raster(as.vector(spatial_data_basins), 1, "na")
polygons_behavioural <- raster::rasterToPolygons(spatial_layer_basins,
                                                 na.rm=TRUE, dissolve=TRUE)

kge_info$layer <- sapply(kge_info$station, function(x){ as.integer(strsplit(x, "X")[[1]][2]) })
cal_result <- sp::merge(polygons_behavioural, kge_info, by="layer", how="right")

sf::st_write(sf::st_as_sf(cal_result), TARGET_2, delete_layer=TRUE)

# create map for plotting spatial map in Figure 2
ROOT_SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2"
ROOT_STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"
TARGET_1 <- "./data/basins.rds"
TARGET_2 <- "./data/basin_sets.shp"

attributes <- read.table("./data/basin_attributes.txt", sep = "\t", header = TRUE)
spatial_data_basins <- watergap3data::unf.readunf(ROOT_SPATIAL_INFO, "na")
station_data <- watergap3data::txt.read_station_list(ROOT_STATION_INFO)

MAX_NUMBER_IN_GRID <- 1000
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])

  if (grdc_id %in% attributes$grdc_ids){
    grdc_id_as_integer <- as.integer(strsplit(grdc_id, "X")[[1]][2])
    spatial_data_basins[abs(spatial_data_basins) == as.integer(internal_id)] <- MAX_NUMBER_IN_GRID + grdc_id_as_integer
  }
  setTxtProgressBar(pb, row)
}

spatial_data_basins <- (spatial_data_basins - MAX_NUMBER_IN_GRID)
spatial_data_basins[spatial_data_basins < -100] <- NA
saveRDS(spatial_data_basins, TARGET_1)

spatial_layer_basins <- watergap3data::unf.vec2raster(as.vector(spatial_data_basins), 1, "na")


polygons_behavioural <- raster::rasterToPolygons(spatial_layer_basins,
                                                 na.rm=TRUE, dissolve=TRUE)

snowish_basins <- get_sensitive_basins("snow")
reservoir_basins <- get_sensitive_basins("reservoir")
river_basins <- get_sensitive_basins("river")

basin_sets <- data.frame("basins" = river_basins)
basin_sets$type <- 0
basin_sets$type <- ifelse(basin_sets$basins %in% snowish_basins, 2, basin_sets$type)
basin_sets$type <- ifelse(basin_sets$basins %in% reservoir_basins, 1, basin_sets$type)
basin_sets$type <- ifelse(((basin_sets$basins %in% reservoir_basins) &
                             ( basin_sets$basins %in% snowish_basins)), 3, basin_sets$type)

basin_sets$layer <- sapply(basin_sets$basins, function(x){ as.integer(strsplit(x, "X")[[1]][2]) })
cal_result <- sp::merge(polygons_behavioural, basin_sets, by="layer", how="right")

sf::st_write(sf::st_as_sf(cal_result), TARGET_2, delete_layer=TRUE)

#reservoir informatio
res_type <- watergap3data::unf.readunf("./data - Kopie/G_RES_TYPE.UNF1", "na")
saveRDS(res_type, "./data/res_types.rds")

local_lakes <- watergap3data::unf.readunf("./data - Kopie/G_LOCLAK.UNF1", "na")
local_wetlands <- watergap3data::unf.readunf("./data - Kopie/G_LOCWET.UNF1", "na")
global_wetlands <- watergap3data::unf.readunf("./data - Kopie/G_GLOWET.UNF1", "na")
res_storage <- watergap3data::unf.readunf("./data - Kopie/G_STORAGE_CAPACITY.UNF0", "na") #km3
res_area <- watergap3data::unf.readunf("./data - Kopie/G_RESAREA.UNF4", "na") #km2
res_mean_inflow <- watergap3data::unf.readunf("./data - Kopie/G_MEAN_INFLOW.UNF0", "na")
spatial_data <- watergap3data::unf.readunf("./data - Kopie/G_CALIB_BASIN.UNF2", "na")
station_data <- watergap3data::txt.read_station_list("./data - Kopie/STATION_LIST.OUT")

## getting reservoirs in simulated basins
station_data <- station_data[paste0("X", station_data$stations) %in% all_basins$behavioural, ]
station_data$internal_ids <- as.integer(as.character(station_data$internal_ids))

result <- data.frame(matrix(ncol = 7, nrow = 0))
column_names <-  c("grdc-id", "basin-set", "reservoir", "capacity_km3", "surface area_km2",
                      "mean depth_m", "mean inflow_km3d")
colnames(result) <- column_names

pb <- txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in seq_along(station_data$stations)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])
  idx <- which(
    (abs(spatial_data) == as.integer(internal_id)) &
    (res_type > 0)
    )

  for (entry in seq_along(idx)){

    basin_id <- grdc_id
    basin_group <- ifelse(grdc_id %in% behavioural_basins$behavioural, "behavioural", "additional")

    res_pos <- idx[entry]
    mean_inflow <- res_mean_inflow[res_pos]
    capacity <- res_storage[res_pos]
    surface <- res_area[res_pos]
    # bei 1 wird Hanasaki ausgeführt --> irrigation (routing.cpp ll. 3023/4)
    irrigation_info <- ifelse(res_type[res_pos] == 1, "irrigation", "non-irrigation")

    reservoir_info <- data.frame(list(
      "grdc-id" = basin_id,
      "basin-set" = basin_group,
      "reservoir" = irrigation_info,
      "capacity_km3" = capacity,
      "surface area_km2" = surface,
      "mean depth_m" = capacity / surface * 1000.,
      "mean inflow_km3d" = mean_inflow
      )
    )
    result <- rbind(result, reservoir_info)
  }
  setTxtProgressBar(pb, row)
}

# es gibt einige reservoire, die eigentlich zu klein für die Einheit sind, sodass
# die reservoir area < 1 km2 ist, und als 0 angezeigt wird
# diese reservoire besitzen deswegen im Modell keine vertikale Wasserbilanz
result$mean.depth_m[result$surface.area_km2 == 0] <- 0
write.table(result, "./data/reservoir_data.csv", sep = "\t",
            row.names = TRUE, col.names = TRUE)

mean(result$mean.depth_m)

# maybe add to text (maybe after review)
nrow(result)
table(result$reservoir[result$basin.set == "additional"]) #128
table(result$reservoir[result$basin.set == "behavioural"]) #245

# prepare quality plot
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten)"
precipitation <- readRDS("data - Kopie/create_basin_timeseries_precipitation.rds") # mm/d ?
basin_area_info <- read.csv("data - Kopie/basin_attributes.txt", sep = "\t")
start <- min(precipitation$date)
end <- max(precipitation$date)

simulated_basins <- precipitation[, colnames(precipitation) %in% all_basins$behavioural]

quality_results <- data.frame(matrix(NA, nrow=length(all_basins$behavioural), ncol=4))

pb = txtProgressBar(min = 0, max = length(all_basins$behavioural), initial = 0)
count <- 1
for (basin_name in all_basins$behavioural) {
  basin_data <- simulated_basins[[basin_name]]
  basin_size <- basin_area_info$basin_size[basin_area_info$grdc_ids == as.integer(substr(basin_name, 2, 10))]
  group_info <- ifelse(basin_name %in% behavioural_basins$behavioural, "behavioural", "additional")

  observed <- WaterGAPLite::Q.read_grdc(
    substr(basin_name, 2, 10),
    NULL,
    "na",
    start=start,
    end=end,
    use_folder=ROOT_GRDC)

  data <- data.frame(
    date = precipitation$date,
    precipitation = basin_data,
    discharge = observed$Value)

  data <- data[182:(nrow(data)-183),] # nehmen unvollständige wintersaison raus

  data$precipitation_in_m3s <- data$precipitation / (60 * 60 * 24) / 1000. * basin_size * 1000 * 1000
  data$rolling_mean_precipitation <- stats::filter(data$precipitation_in_m3s, rep(1 / 5, 5), sides = 2)
  data <- data [3:(nrow(data)-2),]
  correlation <- ccf(
    data$discharge,
    data$rolling_mean_precipitation,
    na.action=na.pass, lag.max=10, plot = FALSE)[1:10]

  psi <- mean(data$discharge) / mean(data$precipitation_in_m3s) # * 100
  max_cor <- max(correlation$acf)

  quality_results[count, ] <- c(basin_name, psi, max_cor, group_info)
  count <- count + 1

  setTxtProgressBar(pb, count)
}
close(pb)

# TODO: nach Update der Date nochmal ausführen!
quality_results$X2 <- as.numeric(quality_results$X2)
quality_results$X3 <- as.numeric(quality_results$X3)
colnames(quality_results) <- c("grdc_id", "psi", "max_cor", "basin_set")

basin_area_info$grdc_id <- paste0("X", basin_area_info$grdc_ids)
evaluate <- merge(quality_results, basin_area_info, on="grdc_id")

cr <- colorRamp(c("cornflowerblue", "navy", "firebrick"))

plot(evaluate$mean_precipitation_as_snow, evaluate$max_cor, pch=20,
     col = rgb(cr(evaluate$psi / max(evaluate$psi)), max=255))
boxplot(quality_results$psi)
boxplot(quality_results$max_cor)

# unrealistic psi
unrealistic_psi <- evaluate[((evaluate$psi > 1)), c(1,2,3,4,6,10,16,17,21)]
psi_evaluate <- merge(unrealistic_psi, basin_area_info, on="grdc_id")
psi_evaluate[, c(1,2,3,4,5,6,7,8,9)]
# -> very low temperature where multi-annual snow cycle is likely, i.e. permanent
# snow cover is likely -> snow melt might be intensified due to climate change
# or extraordinary hot years (5 basins effected, 1 out of them behavioural)

# unrealistic maxCor
unrealistic_max_cor <- evaluate[((evaluate$max_cor < 0)), c(1,2,3,4,6,10,16,17,21)]
evaluate_max_cor <- merge(unrealistic_max_cor, basin_area_info, on="grdc_id")
evaluate_max_cor[, c(1,2,3,4,5,6,7,8,9)]
# -> low temperature and sometime lake or wetlands effected. Sometimes also
# quite low psi but low temperatures so that anthropogenic influnece is likely
# versatile reasons, in total 9 basins are effcted (2 out of them behavioural)


# create SIs for observed data
ROOT <- "data"
SIM_DISCHARGE_FILE <- "data - Kopie/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt"
OBS_DISCHARGE_FILE <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten\%s_Q_Day.Cmd.txt)"

END_CAL <- as.Date("1988-12-31")


result_array <- array(NA, dim=c(1, 12, 330))

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

  result_array[run, 11, station] <- mean(obs_data_val$Value)
  result_array[run, 12, station] <- sd(obs_data_val$Value)
}



dimnames(result_array) <- list("obs" ,
                               c("mgn_l", "mgn_a", "mgn_h", "frq_l", "frq_h", "dur_l", "dur_h",
                                 "max", "lowflow_events", "highflow_events", "mean", "sd"),
                               names(ref_simulated_data)[1:330])

ids2save <- get_sensitive_basins("reservoir")
result_array[,,dimnames(result_array)[[3]] %in% ids2save]

saveRDS(result_array, file.path("./data/SI_obs_ref_as_m8_100d_complete_validation_period.rds"))

data <- readRDS("./data/SI_obs_ref_as_m8_100d_complete_validation_period.rds")
dimnames(data)[[2]]
