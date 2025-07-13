rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")


ROOT_SIMULATED <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten)"

period_1 <- list(as.Date("1990-01-01"), as.Date("1990-12-31"))

hanasaki_examine <- c("X4213101", "X4115102")

all_examine <- c("X4213101", "X4115102")
periods <- list(period_1, period_1)
frequency <- c("1 month", "1 month")
add_obs <- c(TRUE, TRUE)
add_extras <- c("None", "None")

# select simulated discharge
discharge_reference <- read.table(sprintf(ROOT_SIMULATED, 8), sep = "\t", header = T)
discharge_hanasaki <- read.table(sprintf(ROOT_SIMULATED, 12), sep = "\t", header = T)
discharge_schneider <- read.table(sprintf(ROOT_SIMULATED, 16), sep = "\t", header = T)
complete_period <- as.POSIXct(discharge_hanasaki$date)

count <- 1
for (basin in all_examine){
  plot_name <- sprintf("plots/appendixC34_%s.png", basin)

  discharge_observed <- tryCatch(
      WaterGAPLite::Q.read_grdc(
      substr(basin, 2, 10),
      NULL,
      "na",
      start = as.Date(min(discharge_hanasaki$date)),
      end = as.Date(max(discharge_hanasaki$date)),
      use_folder = ROOT_GRDC
    ),
    error = function(err) {NULL})


  ref_high_flow <- median(discharge_reference[[basin]]) * 9

  period <- which(
    (complete_period >= as.POSIXct(periods[[count]][[1]])) &
      (complete_period <= as.POSIXct(periods[[count]][[2]]))
  )

  basin_hanasaki <- discharge_hanasaki[[basin]][period]
  basin_schneider <- discharge_schneider[[basin]][period]
  if (!is.null(discharge_observed)){
    basin_observed <- discharge_observed$Value[period]
  }else {
    basin_observed <- NULL
  }
  basin_reference <- discharge_reference[[basin]][period]
  x_values <- as.POSIXct(discharge_hanasaki$date[period])

  ylim <- c(
    min(c(basin_hanasaki, basin_schneider, basin_observed, basin_reference)),
    max(c(basin_hanasaki, basin_schneider, basin_observed, basin_reference))
  )

  legend_entries <- c()
  legend_colors <- c()
  png(plot_name, units = "cm", width = 19, height = 12, res = 300)

  if (basin %in% hanasaki_examine) {
    line1 <- basin_reference
    line2 <- basin_hanasaki
    legend_entries <- c(legend_entries, "Simulated discharge (V0)", "Simulated discharge (V1)")
    legend_colors <- c(legend_colors, datylon_map[1], datylon_map[4])
  } else if (basin %in% schneider_examine) {
    line1 <- basin_hanasaki
    line2 <- basin_schneider
    legend_entries <- c(legend_entries, "Simulated discharge (V1)", "Simulated discharge (V2)")
    legend_colors <- c(legend_colors, datylon_map[1], datylon_map[4])
  }

  alpha <- 0.8
  par(mar = c(6, 4 + 1, 4, 2), mai = c(1, 1, 1, 0))
  plot(x_values, line1,
    xaxt = "n", xlab = "",
    type = "l", col = datylon_map[4],
    ylab = expression("Discharge [m"^3 * "s"^-1 * "]"), lwd = 2,
    ylim = ylim
  )

  lines(x_values, line2,
    col = grDevices::adjustcolor(datylon_map[1], alpha.f = alpha), lwd = 2
  )

  if ((add_obs[count] == TRUE) & !(is.null(basin_observed))) {
    lines(x_values, basin_observed,
      col = grDevices::adjustcolor(datylon_map[8], alpha.f = alpha), lwd = 2
    )
    legend_entries <- c(legend_entries, "Observed discharge")
    legend_colors <- c(legend_colors, datylon_map[8])
  }


  if (add_extras[count] == "high_flow") {
    abline(h = ref_high_flow, col = "black", lwd = 2)
    legend_entries <- c(legend_entries, "high flow")
    legend_colors <- c(legend_colors, "black")
  } else if (add_extras[count] == "low_flow") {
    abline(h = ref_high_flow, col = "black", lwd = 2)
    legend_entries <- c(legend_entries, "low flow")
    legend_colors <- c(legend_colors, "black")
  }

  legend("topright",
    col = legend_colors,
    lty = 1, lwd = 2, legend = legend_entries,
    bg = NA, bty = "n"
  )

  axis.POSIXct(
    side = 1,
    x = x_values,
    at = seq(
      from = x_values[1],
      to = x_values[length(x_values)],
      by = frequency[count]
    ),
    las = 2,
    format = "%e %b"
  )
  mtext(format(x_values[length(x_values)], "%Y"),
    side = 1, line = 4, at = x_values[length(x_values) - 2],
    cex = 1.2
  )
  dev.off()

  count <- count + 1
}
