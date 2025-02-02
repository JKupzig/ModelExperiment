rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")


ROOT_SIMULATED <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"

period_1 <- list(as.Date("1990-01-01"), as.Date("1990-12-31"))
period_2 <- list(as.Date("1991-10-15"), as.Date("1992-05-15"))
period_3 <- list(as.Date("1991-02-15"), as.Date("1991-04-05"))

schneider_examine <- c("X4149405", "X4146210")
hanasaki_examine <- c("X4213101", "X4115102")

all_examine <- c(hanasaki_examine, schneider_examine)
periods <- list(period_1, period_1, period_2, period_3)
frequency <- c("1 month", "1 month", "2 week", "1 week")
add_obs <- c(TRUE, TRUE, FALSE, FALSE)
add_extras <- c("None", "None", "high_flow", "high_flow")

#select simulated discharge
discharge_reference <- read.table(sprintf(ROOT_SIMULATED, 8), sep = "\t", header=T)
discharge_hanasaki <- read.table(sprintf(ROOT_SIMULATED, 12), sep="\t", header=T)
discharge_schneider <- read.table(sprintf(ROOT_SIMULATED, 16), sep="\t", header=T)
complete_period <- as.POSIXct(discharge_hanasaki$date)

count <- 1
for (basin in unique(all_examine)) {

  plot_name <- sprintf("plots/Appendix_b_%s.png", basin)

  discharge_observed <- WaterGAPLite::Q.read_grdc(
    substr(basin, 2, 10),
    NULL,
    "na",
    start = as.Date(min(discharge_hanasaki$date)),
    end = as.Date(max(discharge_hanasaki$date)),
    use_folder = ROOT_GRDC)

  ref_high_flow <- median(discharge_reference[[basin]])*9

  period <- which(
    (complete_period >= as.POSIXct(periods[[count]][[1]])) &
    (complete_period <= as.POSIXct(periods[[count]][[2]]))
    )

  basin_hanasaki <- discharge_hanasaki[[basin]][period]
  basin_schneider <- discharge_schneider[[basin]][period]
  basin_observed <- discharge_observed$Value[period]
  basin_reference <- discharge_reference[[basin]][period]
  x_values <- as.POSIXct(discharge_hanasaki$date[period])

  ylim <- c(min(c(basin_hanasaki, basin_schneider, basin_observed, basin_reference)),
            max(c(basin_hanasaki, basin_schneider, basin_observed, basin_reference))
            )

  png(plot_name, units = "cm", width = 19, height = 12, res = 300)

  legend_entries <- c("Simulated discharge (V0)")
  legend_colors <- c(datylon_map[4])
  alpha <- 0.8
  par(mar = c(6, 4 + 1, 4, 2))
  plot(x_values, basin_reference,
       xaxt = "n",  xlab = "",
       type = "l", col = datylon_map[4],
       ylab = expression("Discharge (m" ^ 3 * "/s)"), lwd = 2,
       ylim = ylim)

  if (add_obs[count] == TRUE) {
    lines(x_values, basin_observed,
          col = grDevices::adjustcolor(datylon_map[8], alpha.f = alpha), lwd = 2)
    legend_entries <- c(legend_entries, "Observed discharge")
    legend_colors <- c(legend_colors, datylon_map[8])
  }

  if (basin %in% hanasaki_examine) {
    lines(x_values, basin_hanasaki,
          col = grDevices::adjustcolor(datylon_map[1], alpha.f = alpha), lwd = 2)
    legend_entries <- c(legend_entries, "Simulated discharge (V1)")
    legend_colors <- c(legend_colors, datylon_map[1])
  }

  if (basin %in% schneider_examine) {
    lines(x_values, basin_schneider,
          col = grDevices::adjustcolor(datylon_map[2], alpha.f = alpha), lwd = 2)
    legend_entries <- c(legend_entries, "Simulated discharge (V2)")
    legend_colors <- c(legend_colors, datylon_map[2])
  }

  if (add_extras[count] == "high_flow") {
    abline(h = ref_high_flow, col = "black", lwd = 2)
  }

  legend("topright", col = legend_colors,
         lty = 1, lwd = 2, legend = legend_entries)

  axis.POSIXct(side = 1,
               x = x_values,
               at = seq(from = x_values[1],
                        to = x_values[length(x_values)],
                        by = frequency[count]),
               las = 2,
               format = "%e %b")
  mtext(format(x_values[length(x_values)], "%Y"),
        side = 1, line = 4, at = x_values[length(x_values) - 2],
        cex = 1.2)
  dev.off()

  count <- count  + 1
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

#   ylim <- c(min(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period])),
#             max(c(discharge_hanasaki[[basin]][period], observed$Value[period], discharge_ref[[basin]][period]))
#   )
#   x <- as.POSIXct(discharge_hanasaki$date)[period]
#   png(sprintf("plots/reservoirs/non_irrigation_%s.png", basin), units="cm", width=24, height=16, res=300)
#   plot(x,
#        discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
#        type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2, ylim=ylim)

#   lines(x, discharge_hanasaki[[basin]][period],
#         col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)

#   lines(x, observed$Value[period],col=grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7), lwd=2)
#   legend("topright", col=c(datylon_map[8], datylon_map[1], datylon_map[4]), lty=1,
#          legend=c("Observed discharge", "Simulated discharge (V1)", "Simulated discharge (as lakes)"))

#   axis.POSIXct(side = 1,
#                x = x,
#                at = seq(from = x[1],
#                         to = x[length(x)],
#                         by = "1 month"),
#                las = 2)

#   dev.off()

# }


# cal_results_ref$KGE_val[cal_results_ref$station == "X4136400"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4136400"]

# cal_results_ref$b_val[cal_results_ref$station == "X4115400"]
# cal_results_hanasaki$b_val[cal_results_hanasaki$station == "X4115400"] #10% mehr variability!

# #looking at ohter basins: 4136400, 4125050, 4125915
# names_of_simtype <- c("Reservoirs as lakes", "Reservoir algorithm (V1)",
#                       "Observed discharge")
# values <- datylon_map[c(1,4,8)]
# names(values) <- c(names_of_simtype)


# for (basin in c("X4136400", "X4125050", "X4125915")){
#   cal_results_ref$KGE_val[cal_results_ref$station == basin]
#   cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == basin]

#   internal_id <- as.integer(station_info$internal_ids[paste0("X", station_info$stations) == basin])
#   depth <- res_storage[abs(station_map) == internal_id] / res_area[abs(station_map) == internal_id]
#   depth[!is.infinite(depth) & !is.nan(depth) & !is.na(depth) & depth > 0]*1000

#   mean_inflow <- res_mean_inflow[abs(station_map) == internal_id] * 12. * 1000000000. / 31536000.
#   mean_inflow_basin <- sum(mean_inflow[res_area[abs(station_map) == internal_id] > 0])

#   c_ratio <- (res_storage[abs(station_map) == internal_id] /
#                 (res_mean_inflow[abs(station_map) == internal_id] * 12))

#   print(c_ratio[!is.infinite(c_ratio) & !is.nan(c_ratio) & !is.na(c_ratio) & c_ratio > 0])

#   observed <- WaterGAPLite::Q.read_grdc(
#     substr(basin, 2, 10),
#     NULL,
#     "na",
#     start=as.Date(min(discharge_hanasaki$date)),
#     end=as.Date(max(discharge_hanasaki$date)),
#     use_folder=ROOT_GRDC)

#   no_leap_year <- which(format(observed$Date, "%d-%m") != "29-02")

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

#   period <- (365*15):(365*16)


#   period <- (365*10+1):(365*14)
#   threshold <- mean(discharge_ref[[basin]])*0.2
#   sum(discharge_ref[[basin]][period] < threshold)
#   sum(discharge_hanasaki[[basin]][period] < threshold)

#   period <- (365*11+1):(365*14)
#   x <- as.POSIXct(discharge_hanasaki$date)[period]
#   plot(x,
#        discharge_ref[[basin]][period], xaxt = "n",  xlab = "",
#        type="l", col=datylon_map[4], main=basin, ylab="Discharge", lwd=2)

#   lines(x, discharge_hanasaki[[basin]][period],
#         col=grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7), lwd=2)

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

#   WaterGAPLite::Q.calcSI(
#     df=data.frame(
#       "Date"=as.Date(discharge_ref[["date"]][period]),
#       "Sim"=discharge_ref[[basin]][period]),
#     func_name="Q.__calc_frq_l_1__",
#     add_args = discharge_ref[[basin]])

#   WaterGAPLite::Q.calcSI(
#     df=data.frame(
#       "Date"=as.Date(discharge_ref[["date"]][period]),
#       "Sim"=discharge_hanasaki[[basin]][period]),
#     func_name="Q.__calc_frq_l_1__",
#     add_args = discharge_ref[[basin]])


# }


# cal_results_ref$KGE_val[cal_results_ref$station == "X4125050"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125050"]

# cal_results_ref$KGE_val[cal_results_ref$station == "X4125915"]
# cal_results_hanasaki$KGE_val[cal_results_hanasaki$station == "X4125915"]


# # suggesting the improvement of Q.__calc_dur_l_1__
# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = discharge_ref[["X4125915"]])

# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_hanasaki[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = list("discharge"=discharge_ref[["X4125915"]],
#                   "minimal_length"=7))

# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = discharge_ref[["X4125915"]])

# WaterGAPLite::Q.calcSI(
#   df=data.frame(
#     "Date"=as.Date(discharge_hanasaki[["date"]][(365*10+1):(365*16)]),
#     "Sim"=discharge_ref[["X4125915"]][(365*10+1):(365*16)]),
#   func_name="Q.__calc_dur_l_1__",
#   add_args = list("discharge"=discharge_ref[["X4125915"]],
#                   "minimal_length"=7))
