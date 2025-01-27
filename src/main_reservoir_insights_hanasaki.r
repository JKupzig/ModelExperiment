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
for (basin in weird_basin){
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

all_delta_schneider <- (cal_results_ref$r_val - cal_results_schneider$r_val)
a = cal_results_schneider[cal_results_ref$station %in% weird_basin, c(14,15,17,16)]
b = cal_results_ref[cal_results_ref$station %in% weird_basin, c(1, 14,15,17,16)]
merken <- data.frame(station=b[,1], hanasaki=b[1+1], schneider=a[1])
merken[,2] - merken[,3]

discharge_ref <- read.table(sprintf(ROOT_100_DISCHARGE, 8), sep="\t", header=T)
discharge_schneider <- read.table(sprintf(ROOT_100_DISCHARGE, 16), sep="\t", header=T)
discharge_hanasaki <- read.table(sprintf(ROOT_100_DISCHARGE, 12), sep="\t", header=T)

period <- (365*10+1):(365*16)
for (basin in weird_basin){
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

  fig <- fig %>%  plotly::layout(shapes = list(hline(threshold_low)),
                                 title = basin,
                                 xaxis = list(title = ""),
                                 yaxis = list (title = "Discharge"))

  shapes = list(hline(threshold_high), hline(threshold_low),
                hline(threshold_high_ref, color=datylon_map[1]),
                hline(threshold_high_schneider, color=datylon_map[5]))


  print(fig)
}
