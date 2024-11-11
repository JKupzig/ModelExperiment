
rm(list=ls())
library(WaterGAPLite)
library(raster)
library(dplyr)
library(ggplot2)
source("./src/helper/load_wgl.r")
source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")

target = "./plots/weird_basin_reservoir_%s.png"
basin_info <- read.table("./data/weird_reservoir_basins.txt", sep=";", header=TRUE)
basin_to_simulate <- basin_info[6,]
example_basin <- list("grdc_no" = basin_to_simulate$grdc_no,
                      "corLong" = basin_to_simulate$long,
                      "corlat" = basin_to_simulate$lat,
                      "cont" = basin_to_simulate$cont,
                      "name" = basin_to_simulate$name)

loaded_basin <- load_wgl(example_basin)


# get info about local wetlands
raster::plot(WaterGAPLite::basin.create_raster(loaded_basin$basin_object@G_LOCWET,
                                               loaded_basin$basin_object))
raster::plot(WaterGAPLite::basin.create_raster(loaded_basin$basin_object@G_ALTITUDE,
                                               loaded_basin$basin_object))

# run model
settings_with_snow_in_wetlands <- init.settings(snow_in_wetlands = "on")
settings_no_snow_in_wetlands <- init.settings(snow_in_wetlands = "off")
warmup_years <- 10

standard_run <- runModel(
  SimPeriod = loaded_basin$run_object$SimPeriod,
  ListConst = loaded_basin$run_object,
  Settings = settings_no_snow_in_wetlands,
  nYears = warmup_years
)

snow_run <- runModel(
  SimPeriod = loaded_basin$run_object$SimPeriod,
  ListConst = loaded_basin$run_object,
  Settings = settings_with_snow_in_wetlands,
  nYears = warmup_years
)

plot(WaterGAPLite::basin.create_average(
  snow_run$routing$locWetland$Snow,
  loaded_basin$basin_object))

plot(snow_run$routing$River$Discharge, standard_run$routing$River$Discharge,
     xlim=c(0, 1.5), ylim=c(0,1.5))
abline(a=0, b=1, col="firebrick")

plot(snow_run$routing$River$Discharge, type="l")
lines(standard_run$routing$River$Discharge, col="firebrick")



# now comparing streams:
ref_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt",
                                   sep="\t", header=T)
snow_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m9_wetlStorage100.txt",
                                     sep="\t", header=T)


ref_discharge <- ref_discharge_all[[paste0("X", loaded_basin$basin_object@id)]]
snow_discharge <- snow_discharge_all[[paste0("X", loaded_basin$basin_object@id)]]
date <- as.Date(snow_discharge_all[["date"]], format="%Y-%m-%d")

mean(snow_run$routing$River$Discharge) / mean(standard_run$routing$River$Discharge)
mean(snow_discharge) / mean(ref_discharge)

observed_discharge <- Q.read_grdc(
  loaded_basin$basin_object@id,
  NULL,
  cont = "na",
  start = min(date),
  end = max(date),
  use_folder = DISCHARGE_DATA_PATH
)

no_leap_year <- which(format(observed_discharge$Date, "%d-%m") != "29-02")

colors_to_use <- datylon_map[c(1,7)]
names_of_simtype <- c("No snow", "Snow","Observed discharge")
values=c(colors_to_use, "black")
names(values) <- c(names_of_simtype)
data_all <- data.frame(date=date,
                       no_snow=ref_discharge,
                       snow=snow_discharge,
                       obs=observed_discharge$Value[no_leap_year])
names(data_all) <- c("Date", names_of_simtype)
data_all %>%
  mutate(month = lubridate::month(date)) %>%
  tidyr::pivot_longer(., cols= names_of_simtype) %>%
  group_by(name, month) %>% summarise(monthly_mean=mean(value)) %>%
  ggplot(.) +
  geom_line(aes(x=month, y=monthly_mean, col=name), lwd=1) +
  scale_color_manual(values=values) +
  ylab("Monthly mean of discharge (m3/s)") +
  xlab("Month of year") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  theme_bw()


LWD = 1.5
from <- 6*365
to <- 8*365
plot(data_all$Date[from:to],
     data_all$`Observed discharge`[from:to], type="l", lwd=LWD,
     ylab="Discharge [m3/s]",
     xlab="")

lines(data_all$Date[from:to],
      data_all$`No snow`[from:to], col="firebrick", lwd=LWD)
lines(data_all$Date[from:to],
      data_all$Snow[from:to], col="cornflowerblue", lwd=LWD)

# comparing!
attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)
snow <- read.table("./data/evaluation/cal_result_benchmarks_model_m9.txt", sep="\t", header=T)
no_snow <- read.table("./data/evaluation/cal_result_benchmarks_model_m8.txt", sep="\t", header=T)
interesting_basins <- abs(no_snow$KGE_val - snow$KGE_val) > 0.01
interesting_basins_id <- no_snow$station[interesting_basins]

behavioural_basins <- read_kge_and_define_good_basins(0.0, NULL)
behaviorual_interesting_basins_id <- interesting_basins_id[interesting_basins_id %in% behavioural_basins$behavioural]

subset_attributes <- attributes[paste0("X", attributes$grdc_ids) %in% behaviorual_interesting_basins_id,]

entry <- subset_attributes$mean_temperature
boxplot(entry)
abline(h=entry[subset_attributes$grdc_ids == 4133250],
       col="firebrick")

min(attributes$basin_size[paste0("X", attributes$grdc_ids) %in% behavioural_basins$behavioural]) #2928.832
attributes$grdc_ids[attributes$basin_size < 3000]
