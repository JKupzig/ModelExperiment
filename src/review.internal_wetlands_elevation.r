
source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

CONT <- "na"
loc_wet <- watergap3data::unf.readunf("./data - Kopie/G_LOCWET.UNF1", cont=CONT)
elevation <- watergap3data::unf.readunf("./data - Kopie/G_ELEV_RANGE.26.UNF2", cont=CONT)

SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2"
STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"

spatial_data_basins <- watergap3data::unf.readunf(SPATIAL_INFO, CONT)
station_data <- watergap3data::txt.read_station_list(STATION_INFO)

basins <- get_sensitive_basins("river")

MAX_NUMBER_IN_GRID <- 1000
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])

  if (grdc_id %in% basins){
    grdc_id_as_integer <- as.integer(strsplit(grdc_id, "X")[[1]][2])
    spatial_data_basins[abs(spatial_data_basins) == as.integer(internal_id)] <- MAX_NUMBER_IN_GRID + grdc_id_as_integer
  }
  setTxtProgressBar(pb, row)
}

spatial_data_basins <- (spatial_data_basins - MAX_NUMBER_IN_GRID)
spatial_data_basins[spatial_data_basins < -100] <- NA

elevation_in_calibrated_wetlands <- elevation[,!is.null(spatial_data_basins) & loc_wet > 0]

max_elevation_in_calibrated_wetlands <- apply(elevation_in_calibrated_wetlands,2,max)
min_elevation_in_calibrated_wetlands <- apply(elevation_in_calibrated_wetlands,2,min)

hist(max_elevation_in_calibrated_wetlands - min_elevation_in_calibrated_wetlands, freq=FALSE)
boxplot(max_elevation_in_calibrated_wetlands - min_elevation_in_calibrated_wetlands)


sum((max_elevation_in_calibrated_wetlands - min_elevation_in_calibrated_wetlands) > 500) / length(min_elevation_in_calibrated_wetlands) * 100

sum(max_elevation_in_calibrated_wetlands > 1000) / length(min_elevation_in_calibrated_wetlands) * 100

loc_wet_raster <- watergap3data::unf.unf2raster("./data - Kopie/G_GLOWET.UNF1", cont=CONT)
raster::writeRaster(loc_wet_raster, "./data - Kopie/glowet.tif")

length(get_sensitive_basins("snow"))
length(get_sensitive_basins("reservoir"))
length(get_sensitive_basins("river"))
