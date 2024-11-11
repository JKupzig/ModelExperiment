#get amount of non-irrigation and irrigation resevroirs with reservoir area
rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

ROOT_100 <- "./data/evaluation/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_100_DISCHARGE <- "./data/evaluation/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"

MIN_QUAL <- 0.4
MAX_QUAL <- NULL
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)

res_storage <- watergap3data::unf.readunf("./data/G_STORAGE_CAPACITY.UNF0", "na") #km3
res_area <- watergap3data::unf.readunf("./data/G_RESAREA.UNF4", "na") #km2
res_type <- watergap3data::unf.readunf("./data/G_RES_TYPE.UNF1", "na")
res_mean_inflow <- watergap3data::unf.readunf("./data/G_MEAN_INFLOW.UNF0", "na")

res_area_nonirrigation <- res_area
res_area_nonirrigation[res_type == 1] <- NA
depth <- (res_storage[1,]/res_area_nonirrigation[1,])*1000 #m
boxplot(depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0], ylim=c(0,50))
mean(depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0])

res_area_irrigation <- res_area
res_area_irrigation[res_type != 1] <- NA
depth <- (res_storage[1,]/res_area_irrigation[1,])*1000 #m
boxplot(depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0], ylim=c(0,50))
mean(depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0])

depth <- (res_storage[1,]/res_area[1,])*1000 #m
mean(depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0])

local_lakes <- watergap3data::unf.readunf("./data/G_LOCLAK.UNF1", "na")
local_wetlands <- watergap3data::unf.readunf("./data/G_LOCWET.UNF1", "na")
global_wetlands <- watergap3data::unf.readunf("./data/G_GLOWET.UNF1", "na")

station_map <- watergap3data::unf.readunf("./data/G_CALIB_BASIN.UNF2", "na")
station_info <- watergap3data::txt.read_station_list("./data/STATION_LIST.OUT")

irrigation <- c()
non_irrigation <- c()
waterbodies <- c()

for (basin in behavioural_basins$behavioural){

  internal_id <- as.integer(station_info$internal_ids[paste0("X", station_info$stations) == basin])
  irrigation_in_basin <- sum(res_area[abs(station_map) == internal_id & res_type == 1])
  non_irrigation_in_basin <- sum(res_area[abs(station_map) == internal_id & res_type != 1])

  waterbodies_in_basin <- (sum(local_wetlands[abs(station_map) == internal_id]) +
                             sum(local_wetlands[abs(station_map) == internal_id]) +
                             sum(global_wetlands[abs(station_map) == internal_id]))

  irrigation <- c(irrigation, irrigation_in_basin)
  non_irrigation <- c(non_irrigation, non_irrigation_in_basin)
  waterbodies <- c(waterbodies, waterbodies_in_basin)
}

irrigation_basins <- behavioural_basins$behavioural[irrigation > 0 & non_irrigation == 0]
non_irrigation_basins <- behavioural_basins$behavioural[irrigation == 0 & non_irrigation > 0]
other_basins <- behavioural_basins$behavioural[irrigation > 0 & non_irrigation > 0]

irrigation[irrigation > 0 & non_irrigation == 0]
waterbodies[irrigation > 0 & non_irrigation == 0]

cal_results_ref <- read.table(sprintf(ROOT_100, 8), sep="\t", header=T)
cal_results_hanasaki <- read.table(sprintf(ROOT_100, 12), sep="\t", header=T)

all_delta <- (cal_results_ref$r_val[cal_results_ref$station %in% behavioural_basins$behavioural] -
              cal_results_hanasaki$r_val[cal_results_hanasaki$station %in% behavioural_basins$behavioural])
behavioural <- cal_results_ref[cal_results_ref$station %in% behavioural_basins$behavioural,]
sensitive_stations <- behavioural[abs(all_delta) > 0.05,1]
sensitive_stations_mask <- behavioural_basins$behavioural %in% sensitive_stations
non_irrigation[sensitive_stations_mask]
irrigation[sensitive_stations_mask]

both_delta <- (cal_results_ref$r_val[cal_results_ref$station %in% other_basins] -
                 cal_results_hanasaki$r_val[cal_results_hanasaki$station %in% other_basins])
irrigation_delta <- (cal_results_ref$r_val[cal_results_ref$station %in% irrigation_basins] -
                 cal_results_hanasaki$r_val[cal_results_hanasaki$station %in% irrigation_basins])
non_irrigation_delta <- (cal_results_ref$r_val[cal_results_ref$station %in% non_irrigation_basins] -
                 cal_results_hanasaki$r_val[cal_results_hanasaki$station %in% non_irrigation_basins])


boxplot(list("irrigation"=irrigation_delta,
             "non-irrigation"=non_irrigation_delta,
             "both"=both_delta))
abline(h=0)

ref_quality <- cal_results_ref[cal_results_ref$station %in% irrigation_basins, c(1, 14, 15, 16, 17)]
ref_hanasaki <- cal_results_hanasaki[cal_results_hanasaki$station %in% irrigation_basins, c(1,14, 15, 16, 17)]

ref_quality$b_val_norm <- abs(1-ref_quality$b_val)
ref_quality$a_val_norm <- abs(1-ref_quality$a_val)
ref_hanasaki$b_val_norm <- abs(1-ref_hanasaki$b_val)
ref_hanasaki$a_val_norm <- abs(1-ref_hanasaki$a_val)

round(ref_quality[,2:ncol(ref_quality)], 2)
round(ref_hanasaki[2:ncol(ref_quality)], 2)

#select simulated discharge
discharge_ref <- read.table(sprintf(ROOT_100_DISCHARGE, 8), sep="\t", header=T)
discharge_hanasaki <- read.table(sprintf(ROOT_100_DISCHARGE, 12), sep="\t", header=T)

period <- (365*11+1):(365*12)

for (basin in irrigation_basins){
  observed <- WaterGAPLite::Q.read_grdc(
    substr(basin, 2, 10),
    NULL,
    "na",
    start=as.Date(min(discharge_hanasaki$date)),
    end=as.Date(max(discharge_hanasaki$date)),
    use_folder=ROOT_GRDC)

  ylim <- c(min(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period])),
            max(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period]))
  )
  x <- as.POSIXct(discharge_hanasaki$date)[period]
  png(sprintf("plots/reservoirs/irrigation_%s.png", basin), units="cm",width=24, height=16, res=300)
  plot(x,
       discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
       type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2, ylim=ylim)

  lines(x, discharge_hanasaki[[basin]][period],
        col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)

  lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)

  legend("topleft", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
         legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))

  axis.POSIXct(side = 1,
               x = x,
               at = seq(from = x[1],
                        to = x[length(x)],
                        by = "1 month"),
               las = 2)
  dev.off()

}

#period <- (365*11+1):(365*13)
for (basin in non_irrigation_basins){
  observed <- WaterGAPLite::Q.read_grdc(
    substr(basin, 2, 10),
    NULL,
    "na",
    start=as.Date(min(discharge_hanasaki$date)),
    end=as.Date(max(discharge_hanasaki$date)),
    use_folder=ROOT_GRDC)

  ylim <- c(min(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period])),
            max(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period]))
  )
  x <- as.POSIXct(discharge_hanasaki$date)[period]
  png(sprintf("plots/reservoirs/non_irrigation_%s.png", basin), units="cm", width=24, height=16, res=300)
  plot(x,
       discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
       type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2, ylim=ylim)

  lines(x, discharge_hanasaki[[basin]][period],
        col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)

  lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)
  legend("topright", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
         legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))

  axis.POSIXct(side = 1,
               x = x,
               at = seq(from = x[1],
                        to = x[length(x)],
                        by = "1 month"),
               las = 2)

  dev.off()

}


cal_results_ref$KGE_val[cal_results_ref$station == "X4136400"]
cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4136400"]

cal_results_ref$b_val[cal_results_ref$station == "X4115400"]
cal_results_hanasaki$b_val[cal_results_hanasaki$station == "X4115400"] #10% mehr variability!

#looking at ohter basins: 4136400, 4125050, 4125915
names_of_simtype <- c("Reservoirs as lakes", "Reservoir algorithm (V1)",
                      "Observed discharge")
values <- datylon_map[c(1,4,8)]
names(values) <- c(names_of_simtype)


for (basin in c("X4136400", "X4125050", "X4125915")){
  cal_results_ref$KGE_val[cal_results_ref$station == basin]
  cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == basin]

  internal_id <- as.integer(station_info$internal_ids[paste0("X", station_info$stations) == basin])
  depth <- res_storage[abs(station_map) == internal_id] / res_area[abs(station_map) == internal_id]
  depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0]*1000

  mean_inflow <- res_mean_inflow[abs(station_map) == internal_id] * 12. * 1000000000. / 31536000.
  mean_inflow_basin <- sum(mean_inflow[res_area[abs(station_map) == internal_id] > 0])

  c_ratio <- (res_storage[abs(station_map) == internal_id] /
                (res_mean_inflow[abs(station_map) == internal_id] * 12))

  print(c_ratio[!is.infinite(c_ratio) & !is.nan(c_ratio) & !is.na(c_ratio) & c_ratio > 0])

  observed <- WaterGAPLite::Q.read_grdc(
    substr(basin, 2, 10),
    NULL,
    "na",
    start=as.Date(min(discharge_hanasaki$date)),
    end=as.Date(max(discharge_hanasaki$date)),
    use_folder=ROOT_GRDC)

  no_leap_year <- which(format(observed$Date, "%d-%m") != "29-02")

  data_all <- data.frame(date=as.POSIXct(discharge_ref$date),
                         no_res_discharge=discharge_ref[[basin]],
                         hanasaki_discharge=discharge_hanasaki[[basin]],
                         obs=observed$Value[no_leap_year])
  names(data_all) <- c("Date", names_of_simtype)
  print(data_all %>%
    mutate(month = lubridate::month(Date)) %>%
    tidyr::pivot_longer(., cols= names_of_simtype) %>%
    group_by(name, month) %>% summarise(monthly_mean=mean(value)) %>%
    ggplot(.) +
    geom_line(aes(x=month, y=monthly_mean, col=name), lwd=1) +
    scale_color_manual(values=values) +
    ylab("Monthly mean of discharge (m3/s)") +
    xlab("Month of year") +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    theme_bw() +
    ggtitle(basin))

  period <- (365*15):(365*16)


  period <- (365*10+1):(365*14)
  threshold <- mean(discharge_ref[[basin]])*0.2
  sum(discharge_ref[[basin]][period] < threshold)
  sum(discharge_hanasaki[[basin]][period] < threshold)

  period <- (365*11+1):(365*14)
  x <- as.POSIXct(discharge_hanasaki$date)[period]
  plot(x,
       discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
       type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2)

  lines(x, discharge_hanasaki[[basin]][period],
        col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)

  lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)
  legend("topright", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
         legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))
  abline(h=threshold, lwd=2)
  abline(h=mean_inflow_basin, lwd=2, col="cornflowerblue")
  axis.POSIXct(side = 1,
               x = x,
               at = seq(from = x[1],
                        to = x[length(x)],
                        by = "1 month"),
               las = 2)

  WaterGAPLite::Q.calcSI(
    df=data.frame(
      "Date"=as.Date(discharge_ref[["date"]][period]),
      "Sim"=discharge_ref[[basin]][period]),
    func_name="Q.__calc_frq_l_1__",
    add_args = discharge_ref[[basin]])

  WaterGAPLite::Q.calcSI(
    df=data.frame(
      "Date"=as.Date(discharge_ref[["date"]][period]),
      "Sim"=discharge_hanasaki[[basin]][period]),
    func_name="Q.__calc_frq_l_1__",
    add_args = discharge_ref[[basin]])


}


cal_results_ref$KGE_val[cal_results_ref$station == "X4125050"]
cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125050"]

cal_results_ref$KGE_val[cal_results_ref$station == "X4125915"]
cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125915"]


# suggesting the improvement of Q.__calc_dur_l_1__
WaterGAPLite::Q.calcSI(
  df=data.frame(
    "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
    "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
  func_name="Q.__calc_dur_l_1__",
  add_args = discharge_ref[["X4125915"]])

WaterGAPLite::Q.calcSI(
  df=data.frame(
    "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
    "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
  func_name="Q.__calc_dur_l_1__",
  add_args = list("discharge"=discharge_ref[["X4125915"]],
                  "minimal_length"=7))

WaterGAPLite::Q.calcSI(
  df=data.frame(
    "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
    "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
  func_name="Q.__calc_dur_l_1__",
  add_args = discharge_ref[["X4125915"]])

WaterGAPLite::Q.calcSI(
  df=data.frame(
    "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
    "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
  func_name="Q.__calc_dur_l_1__",
  add_args = list("discharge"=discharge_ref[["X4125915"]],
                  "minimal_length"=7))
