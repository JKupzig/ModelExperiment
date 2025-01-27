#get amount of non-irrigation and irrigation resevroirs with reservoir area
rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

hline <- function(y = 0, color = "black") {

  list(

    type = "line",

    x0 = 0,

    x1 = 1,

    xref = "paper",

    y0 = y,

    y1 = y,

    line = list(color = color)

  )

}

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
bankfull_flow <- watergap3data::unf.readunf("./data/G_BANKFULL.UNF0", "na")
routeorder <- watergap3data::unf.readunf("./data/G_ROUT_ORDER.UNF4", "na")

local_lakes <- watergap3data::unf.readunf("./data/G_LOCLAK.UNF1", "na")
local_wetlands <- watergap3data::unf.readunf("./data/G_LOCWET.UNF1", "na")
global_wetlands <- watergap3data::unf.readunf("./data/G_GLOWET.UNF1", "na")
global_lakes <- watergap3data::unf.readunf("./data/G_GLOLAK.UNF1", "na")
global_wb <- global_lakes + global_wetlands

station_map <- watergap3data::unf.readunf("./data/G_CALIB_BASIN.UNF2", "na")
station_info <- watergap3data::txt.read_station_list("./data/STATION_LIST.OUT")

weird_basin <- c("X4146210", "X4231620", "X4125025", "X4125050",
                 "X4356300", "X4149405", "X4115400", "X4126701")

weird_basin_hanasaki <- c(
  "X4131500", "X4136400", "X4356300",
  "X4115400", "X4125050", "X4150310", "X4122271")
for (basin in weird_basin_hanasaki){
  print(basin)

  internal_id <- as.integer(station_info$internal_ids[paste0("X", station_info$stations) == basin])
  restype_in_basin <- (res_type[abs(station_map) == internal_id & res_type != 0])
  reservoir_in_basin <- (res_area[abs(station_map) == internal_id & res_type != 0])
  capacity_in_basin <- (res_storage[abs(station_map) == internal_id & res_type != 0])
  meanflow <- (res_mean_inflow[abs(station_map) == internal_id & res_type != 0])
  bankfull_res <-  (bankfull_flow[abs(station_map) == internal_id & res_type != 0])


  routeorder_res <- (routeorder[abs(station_map) == internal_id & res_type != 0])
  downstream_wb <- c()
  for (entry in routeorder_res){
    downstream_wb <- mean(global_wb[routeorder > entry])
  }


  print(sum(abs(station_map) == internal_id))
  print(sprintf("type: %i - area: %i, c_ratio: %f., depth: %f bankfull flow: %f, downstream_global: %f",
                restype_in_basin,
                reservoir_in_basin,
                capacity_in_basin / (meanflow * 12),
                capacity_in_basin / reservoir_in_basin * 1000,
                bankfull_res,
                downstream_wb))
}



cal_results_ref <- read.table(sprintf(ROOT_100, 8), sep="\t", header=T)
cal_results_schneider <- read.table(sprintf(ROOT_100, 16), sep="\t", header=T)

all_delta_schneider <- (cal_results_ref$KGE_val - cal_results_schneider$KGE_val)
a = cal_results_schneider[cal_results_ref$station %in% weird_basin_hanasaki, c(14,15,17,16)]
b = cal_results_ref[cal_results_ref$station %in% weird_basin_hanasaki, c(1, 14,15,17,16)]
merken <- data.frame(station=b[,1], hanasaki=b[1+1], schneider=a[1])
merken[,2] - merken[,3]

discharge_ref <- read.table(sprintf(ROOT_100_DISCHARGE, 8), sep="\t", header=T)
discharge_schneider <- read.table(sprintf(ROOT_100_DISCHARGE, 16), sep="\t", header=T)
discharge_hanasaki <- read.table(sprintf(ROOT_100_DISCHARGE, 12), sep="\t", header=T)

period <- (365*10+1):(365*16)
for (basin in weird_basin_hanasaki){
  observed <- WaterGAPLite::Q.read_grdc(
    substr(basin, 2, 10),
    NULL,
    "na",
    start=as.Date(min(discharge_hanasaki$date)),
    end=as.Date(max(discharge_hanasaki$date)),
    use_folder=ROOT_GRDC)

  ylim <- c(min(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period])),
            max(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period]))*1.2
  )
  x <- as.POSIXct(discharge_hanasaki$date)[period]

  threshold_high_ref <- as.numeric(quantile(discharge_ref[[basin]][period], 0.95))
  threshold_high_schneider <- as.numeric(quantile(discharge_schneider[[basin]][period], 0.95))


  threshold_high <- stats::median(discharge_ref[[basin]]) * 9
  threshold_low <- mean(discharge_ref[[basin]]) * 0.2
  data = data.frame(x=x, aslakes= discharge_ref[[basin]][period],
                    schneider=discharge_schneider[[basin]][period],
                    hanasaki=discharge_hanasaki[[basin]][period],
                    obs=observed$Value[period])
  fig <- plotly::plot_ly(data, x = ~x, y=~aslakes,  mode = 'lines', type = 'scatter', name = 'as_lakes',
                         line = list(color = grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7)))
  fig <- fig %>% plotly::add_trace(y = ~schneider, name = 'schneider',mode = 'lines',
                                   line = list(color = grDevices::adjustcolor(datylon_map[5], alpha.f = 0.7)))
  fig <- fig %>% plotly::add_trace(y = ~hanasaki, name = 'hanasaki',mode = 'lines',
                                   line = list(color = grDevices::adjustcolor(datylon_map[4], alpha.f = 0.7)))
  fig <- fig %>% plotly::add_trace(y = ~obs, name = 'obs',mode = 'lines',
                                   line = list(color = grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7)))

  fig <- fig %>%  plotly::layout(shapes = list(hline(threshold_high), hline(threshold_low),
                                               hline(threshold_high_ref, color=datylon_map[1]),
                                               hline(threshold_high_schneider, color=datylon_map[5])),
                                title = basin,
                                 xaxis = list(title = ""),
                                 yaxis = list (title = "Discharge"))

  shapes = list(hline(threshold_high), hline(threshold_low),
                hline(threshold_high_ref, color=datylon_map[1]),
                hline(threshold_high_schneider, color=datylon_map[5]))


  print(fig)
}

# #period <- (365*11+1):(365*13)
# for (basin in non_irrigation_basins){
#   observed <- WaterGAPLite::Q.read_grdc(
#     substr(basin, 2, 10),
#     NULL,
#     "na",
#     start=as.Date(min(discharge_hanasaki$date)),
#     end=as.Date(max(discharge_hanasaki$date)),
#     use_folder=ROOT_GRDC)
#
#   ylim <- c(min(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period])),
#             max(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period]))
#   )
#   x <- as.POSIXct(discharge_hanasaki$date)[period]
#   png(sprintf("plots/reservoirs/non_irrigation_%s.png", basin), units="cm", width=24, height=16, res=300)
#   plot(x,
#        discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
#        type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2, ylim=ylim)
#
#   lines(x, discharge_hanasaki[[basin]][period],
#         col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)
#
#   lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)
#   legend("topright", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
#          legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))
#
#   axis.POSIXct(side = 1,
#                x = x,
#                at = seq(from = x[1],
#                         to = x[length(x)],
#                         by = "1 month"),
#                las = 2)
#
#   dev.off()
#
# }

#
# cal_results_ref$KGE_val[cal_results_ref$station == "X4136400"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4136400"]
#
# cal_results_ref$b_val[cal_results_ref$station == "X4115400"]
# cal_results_hanasaki$b_val[cal_results_hanasaki$station == "X4115400"] #10% mehr variability!
#
# #looking at ohter basins: 4136400, 4125050, 4125915
# names_of_simtype <- c("Reservoirs as lakes", "Reservoir algorithm (V1)",
#                       "Observed discharge")
# values <- datylon_map[c(1,4,8)]
# names(values) <- c(names_of_simtype)
#
#
# for (basin in c("X4136400", "X4125050", "X4125915")){
#   cal_results_ref$KGE_val[cal_results_ref$station == basin]
#   cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == basin]
#
#   internal_id <- as.integer(station_info$internal_ids[paste0("X", station_info$stations) == basin])
#   depth <- res_storage[abs(station_map) == internal_id] / res_area[abs(station_map) == internal_id]
#   depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0]*1000
#
#   mean_inflow <- res_mean_inflow[abs(station_map) == internal_id] * 12. * 1000000000. / 31536000.
#   mean_inflow_basin <- sum(mean_inflow[res_area[abs(station_map) == internal_id] > 0])
#
#   c_ratio <- (res_storage[abs(station_map) == internal_id] /
#                 (res_mean_inflow[abs(station_map) == internal_id] * 12))
#
#   print(c_ratio[!is.infinite(c_ratio) & !is.nan(c_ratio) & !is.na(c_ratio) & c_ratio > 0])
#
#   observed <- WaterGAPLite::Q.read_grdc(
#     substr(basin, 2, 10),
#     NULL,
#     "na",
#     start=as.Date(min(discharge_hanasaki$date)),
#     end=as.Date(max(discharge_hanasaki$date)),
#     use_folder=ROOT_GRDC)
#
#   no_leap_year <- which(format(observed$Date, "%d-%m") != "29-02")
#
#   data_all <- data.frame(date=as.POSIXct(discharge_ref$date),
#                          no_res_discharge=discharge_ref[[basin]],
#                          hanasaki_discharge=discharge_hanasaki[[basin]],
#                          obs=observed$Value[no_leap_year])
#   names(data_all) <- c("Date", names_of_simtype)
#   print(data_all %>%
#     mutate(month = lubridate::month(Date)) %>%
#     tidyr::pivot_longer(., cols= names_of_simtype) %>%
#     group_by(name, month) %>% summarise(monthly_mean=mean(value)) %>%
#     ggplot(.) +
#     geom_line(aes(x=month, y=monthly_mean, col=name), lwd=1) +
#     scale_color_manual(values=values) +
#     ylab("Monthly mean of discharge (m3/s)") +
#     xlab("Month of year") +
#     scale_x_continuous(breaks = scales::breaks_pretty()) +
#     theme_bw() +
#     ggtitle(basin))
#
#   period <- (365*15):(365*16)
#
#
#   period <- (365*10+1):(365*14)
#   threshold <- mean(discharge_ref[[basin]])*0.2
#   sum(discharge_ref[[basin]][period] < threshold)
#   sum(discharge_hanasaki[[basin]][period] < threshold)
#
#   period <- (365*11+1):(365*14)
#   x <- as.POSIXct(discharge_hanasaki$date)[period]
#   plot(x,
#        discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
#        type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2)
#
#   lines(x, discharge_hanasaki[[basin]][period],
#         col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)
#
#   lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)
#   legend("topright", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
#          legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))
#   abline(h=threshold, lwd=2)
#   abline(h=mean_inflow_basin, lwd=2, col="cornflowerblue")
#   axis.POSIXct(side = 1,
#                x = x,
#                at = seq(from = x[1],
#                         to = x[length(x)],
#                         by = "1 month"),
#                las = 2)
#
#   WaterGAPLite::Q.calcSI(
#     df=data.frame(
#       "Date"=as.Date(discharge_ref[["date"]][period]),
#       "Sim"=discharge_ref[[basin]][period]),
#     func_name="Q.__calc_frq_l_1__",
#     add_args = discharge_ref[[basin]])
#
#   WaterGAPLite::Q.calcSI(
#     df=data.frame(
#       "Date"=as.Date(discharge_ref[["date"]][period]),
#       "Sim"=discharge_hanasaki[[basin]][period]),
#     func_name="Q.__calc_frq_l_1__",
#     add_args = discharge_ref[[basin]])
#
#
# }
#
#
# cal_results_ref$KGE_val[cal_results_ref$station == "X4125050"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125050"]
#
# cal_results_ref$KGE_val[cal_results_ref$station == "X4125915"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125915"]
#
#
# # suggesting the improvement of Q.__calc_dur_l_1__
# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = discharge_ref[["X4125915"]])
#
# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = list("discharge"=discharge_ref[["X4125915"]],
#                   "minimal_length"=7))
#
# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = discharge_ref[["X4125915"]])
#
# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = list("discharge"=discharge_ref[["X4125915"]],
#                   "minimal_length"=7))
