rm(list = ls())

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

precipitation <- readRDS("./data - Kopie/create_basin_timeseries_precipitation.rds")

stations <- get_sensitive_basins("river")

attributes <- read.csv("./data/basin_attributes.txt", sep="\t")
root_data <-  r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten)"

ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}

pb <- txtProgressBar(min = 1, max =  length(stations), style = 3)
psi <- c()
missing_data <- c()
for (i in seq_along(stations)){
  setTxtProgressBar(pb, i)
  file2read <- file.path(root_data, paste0(substr(stations[i],2,9), "_Q_day.Cmd.txt"))
  data <- read.csv2(
    file2read, skip = 37, sep = ";", header=FALSE,
    dec = ".", colClasses = c("Date", NA, "numeric"))
  colnames(data) <- c("Date", "Time", "Value")

  precipitation_station <- precipitation[,c("date", stations[i])]

  together <- merge(precipitation_station, data, by.x="date", by.y="Date", how="left")
  no_missing_data <- together[together$Value >= 0,]

  if (nrow(together) != nrow(no_missing_data)){
    missing_data <- c(missing_data, i)
  }
  basin_size <- attributes$basin_size[attributes$grdc_ids == stations[i]]

  vol_p <- sum(no_missing_data[stations[i]]) * 1000 / 3600 / 24 * basin_size
  vol_q <- sum(no_missing_data$Value)
  psi <- c(psi, vol_q / vol_p)

}

boxplot(psi)
attributes[attributes$grdc_ids %in% stations[which(psi > 1)],]
attributes[attributes$grdc_ids %in% stations[missing_data],]
