
rm(list=ls())
library(reshape2)
library(ggplot2)

TARGET <- r"(C:\Users\jenny\MyProject_sciebo\ModelStructure\ModelExperiment-1\data\experiment_STATIONS.DAT)"
STATION_SELECTION <- r"(C:\Users\jenny\MyProject_sciebo\ModelStructure\ModelExperiment-1\data\STATIONS.DAT)"
GRDC_FOLDER <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten\%i_Q_Day.Cmd.txt)"


possible_dates <- data.frame(date = seq(as.Date("1978-01-01"), as.Date("2016-12-31"), by=1),
                             count = 0)


station_to_analyse <- read.table(STATION_SELECTION, sep="\t", header=F)

pb = txtProgressBar(min = 0, max = nrow(station_to_analyse), initial = 0)

step_i <- 0
for (station in station_to_analyse$V1){

  step_i <- step_i + 1
  station_data <- read.table(sprintf(GRDC_FOLDER, station),
                             skip=36, header=T, sep=";",strip.white=T)
  station_data <- station_data[station_data$Value != -999., ]
  station_data$Date <- as.Date(station_data$YYYY.MM.DD, "%Y-%m-%d")

  possible_dates$count = possible_dates$count +
    as.integer(possible_dates$date %in% station_data$Date)

  setTxtProgressBar(pb,step_i)
}
plot(possible_dates$date, possible_dates$count/nrow(station_to_analyse)*100)
abline(v=as.Date("1978-01-01"), col="firebrick")
abline(v=as.Date("1994-12-31"), col="firebrick")

pb = txtProgressBar(min = 0, max = nrow(station_to_analyse), initial = 0)
period_to_use <- seq(as.Date("1978-01-01"), as.Date("1994-12-31"), by=1)
stations_to_use <- c()
step_i <- 0
for (station in station_to_analyse$V1){

  step_i <- step_i + 1
  station_data <- read.table(sprintf(GRDC_FOLDER, station),
                             skip=36, header=T, sep=";",strip.white=T)
  station_data$Date <- as.Date(station_data$YYYY.MM.DD, "%Y-%m-%d")
  cut_station_data <- station_data[station_data$Date %in% period_to_use, ]

  is_complete <- nrow(cut_station_data) == length(period_to_use)
  number_of_errors <- sum(cut_station_data$Value == -999.)

  if ((number_of_errors == 0) & (is_complete == T))
  {
    stations_to_use <- c(stations_to_use, station)
  }
  setTxtProgressBar(pb,step_i)
}

#used stations
length(stations_to_use) / nrow(station_to_analyse)*100
selected_stations <- station_to_analyse[station_to_analyse$V1 %in% stations_to_use,]
write.table(selected_stations, TARGET, col.names = F, row.names = F, sep="\t")

# catchment attributes
ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
attributes <- read.table(ATTRIBUTES, header=T, sep="\t")
stations_to_use <- attributes$grdc_ids
x <- melt(attributes)

plt <- ggplot(data = x, aes(y = value))
plt + geom_boxplot() +
  theme_minimal() +
  facet_wrap(.~variable, scales="free") +
  labs(x = "Title", y = "x")

#plot used stations
CALIB_FOLDER <- r"(C:/Users/jenny/MyProject_sciebo/_Nina/Regionalization/_Data/2nd_Calibration)"
cont <- "na"

RasterTempl <- watergap3data::GCRC_list[[cont]]
crs2use <- raster::crs(RasterTempl)
name_basins <- sprintf("G_BASCALIB_GRDC_%s.UNF0", cont)

basins <- as.vector(watergap3data::unf.readunf(file.path(CALIB_FOLDER, name_basins), cont))
basins[(!basins %in% stations_to_use) & (!is.na(basins))] <- NA
for (basin_id in stations_to_use){
  value_to_set <- attributes$aridity[attributes$grdc_ids == basin_id]
  basins[(basins == basin_id) & (!is.na(basins))] <- value_to_set
}

if (!sum(is.na(basins)) == length(basins)){
  basins_raster <- watergap3data::unf.vec2raster(as.vector(basins), 1, cont)
  basins_polygons <- raster::rasterToPolygons(basins_raster, na.rm=TRUE, dissolve=TRUE)
}

raster::plot(basins_raster)
