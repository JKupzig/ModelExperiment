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

  plot_name <- sprintf("plots/appendix/Figure_B_%s.png", basin)

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
         lty = 1, lwd = 2, legend = legend_entries,
         bg = NA, bty = "n")

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
