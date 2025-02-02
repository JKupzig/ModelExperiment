# Figure 6 a) + b) + c)

rm(list = ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(grid)
library(dplyr)
library(caret)

source("./src/helper/comparison.r")
source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

PLOT_NAME_1 <- "./plots/Figure_6a.png"
PLOT_PATTERN_2 <- "./plots/Figure_6bc_%s.png"

METRIC <- "r_val"
BASINS <- c("X4119101", "X4232700")

ROOT_ATTRIBUTES <- "./data/basin_attributes.txt"
ROOT_BENCHMARK <- "./data/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_SIMULATED <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"
# retrieved from https://grdc.bafg.de/data/data_portal/
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"

attributes <- read.table(ROOT_ATTRIBUTES, sep = "\t", header = TRUE)

reference_discharge_all <- read.table(sprintf(ROOT_SIMULATED, 8), sep = "\t", header = TRUE)
comparison_discharge_all <- read.table(sprintf(ROOT_SIMULATED, 9), sep = "\t", header = TRUE)

cal_results_reference <- read.table(sprintf(ROOT_BENCHMARK, 8), sep = "\t", header = TRUE)
cal_results_comparison <- read.table(sprintf(ROOT_BENCHMARK, 9), sep = "\t", header = TRUE)
cal_results <- data.frame(
  basin_id=cal_results_reference$station,
  reference=cal_results_reference[[METRIC]],
  comparison=cal_results_comparison[[METRIC]],
  delta = cal_results_comparison[[METRIC]] - cal_results_reference[[METRIC]]
)

data <- merge(cal_results, attributes, by.x="basin_id", by.y="grdc_ids", how="left")

sensitive_basins <- data[
  (data$mean_precipitation_as_snow > 0.2) &
   (data$localWetlands > 10),]

# Figure 6a
sensitive_basins %>%
  ggplot(., aes(x=localWetlands, y=delta)) +
  xlab("Fraction of smaller wetlands (%)") +
  ylab(("\u0394 Timing (-)")) +
  geom_point() +
  geom_hline(yintercept=0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=-0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=0.1, col="darkgrey", lwd=.6) +
  xlim(10,75) +
  theme_classic()
ggsave(PLOT_NAME_1, units="cm", width=16, height=12, dpi=300)

# create table infor for Figure 6a
THRESHOLDS <- c(0.05, 0.01, -0.01, -0.05)
LABELS <- c("significantly better", "moderately better",
            "no change", "moderately worse", "significantly worse")
labeled_data <- sensitive_basins %>%
  mutate(group=
           ifelse(delta >= THRESHOLDS[1], LABELS[1],
                  ifelse(delta < THRESHOLDS[1] & delta > THRESHOLDS[2], LABELS[2],
                         ifelse(delta <= THRESHOLDS[2] & delta >= THRESHOLDS[3], LABELS[3],
                                ifelse(delta < THRESHOLDS[3] & delta > THRESHOLDS[4], LABELS[4],
                                       LABELS[5]
                                ))))
  ) %>%
  mutate(group = factor(group, levels = rev(LABELS)))
table(labeled_data$group)

### Plots for b)

labels <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
interesting_months <- c(11:12, 1:4)
legend_position <- c("topleft", "bottomleft")

count <- 1
for (basin in BASINS){
  legend_position_basins <- legend_position[count]
  count <- count + 1

  sim_reference <- reference_discharge_all[[basin]]
  sim_comparison <- comparison_discharge_all[[basin]]
  date <- as.Date(comparison_discharge_all$date, "%Y-%m-%d")
  sim_data <- data.frame(
    reference = sim_reference,
    comparison = sim_comparison,
    date = date
  )

  start <- min(date)
  end <- max(date)

  doy_obs <- rep(NA, length(labels))
  if (file.exists(ROOT_GRDC)) {
    observed <- WaterGAPLite::Q.read_grdc(
      substr(basin, 2, 10),
      NULL,
      "na",
      start=start,
      end=end,
      use_folder=ROOT_GRDC)


  mean_discharge_obs <- observed %>%
    mutate(daily_code = format(Date, "%m")) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(Value, na.rm=T)) %>%
    filter(daily_code != "02-29")

    doy_obs <- mean_discharge_obs[interesting_months, ]$mean_basin
  }

  mean_discharge_no_snow <- sim_data %>%
    mutate(daily_code = substr(date, 6, 7)) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(.data[["reference"]]))

  mean_discharge_snow <- sim_data %>%
    mutate(daily_code = substr(date, 6, 7)) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(.data[["comparison"]]))

  doy_no_snow <- mean_discharge_no_snow[interesting_months, ]$mean_basin
  doy_snow <- mean_discharge_snow[interesting_months, ]$mean_basin

  min_y <- min(doy_snow, doy_obs, doy_no_snow, na.rm = TRUE) * 0.95
  max_y <- max(doy_snow, doy_obs, doy_no_snow, na.rm = TRUE) * 1.05

  ylabel <- expression(paste("Long-term monthly discharge (m"^"3", "/s)", sep=""))

  png(sprintf(PLOT_PATTERN_2, basin),
      res = 300, units = "cm", width = 18, height = 12)
  plot(seq_along(labels), doy_obs,
       xlab = "", ylab = ylabel, xaxt = "n",
       ylim = c(min_y, max_y), lwd=1.4, pch = 19, type = "b")
  lines(seq_along(labels), doy_no_snow,
        col = datylon_map[2], lwd=1.4, pch = 19, type = "b")
  lines(seq_along(labels), doy_snow,
        col = datylon_map[5], lwd=1.4, pch = 19, type = "b")
  legend(legend_position_basins,
    legend = c("Observed discharge", "Inactive snow on wetlands",
            "Active snow on wetlands"),
    cex=1.0, bg = NA, bty = "n",
    lty = 1, lwd = 2, col = c("black", datylon_map[2], datylon_map[5]))
  axis(1, at = seq_along(labels),
       labels = labels)
  dev.off()
}
