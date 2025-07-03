rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")


ATTRIBUTES <- "./data/basin_attributes.txt"
CHARACTERISTIC_LIST <- c("sum_precipitation", "mean_temperature", "mean_precipitation_as_snow","aridity")
LABELS_TO_USE <- c("Annual~precipitation~'[mm]'",
                   "Mean~temperature~'[°C]'",
                   "Precipitation~as~snow~'[%]'",
                   "Aridity~Index~'[-]'")


data_to_plot <- list()
lower_thresholds <- list(NULL, 0.4, NULL)
higher_thresholds <- list(0.4,  NULL, NULL)
names <- c("non-behavioural", "behavioural", "all")

attributes <- read.table(ATTRIBUTES, sep = "\t", header = TRUE)
for (basin_group in c(1, 2, 3)){
  behavioural <- read_kge_and_define_good_basins(
    lower_thresholds[[basin_group]],
    higher_thresholds[[basin_group]]
  )
  name <- names[basin_group]

  behavioural_basins <- as.data.frame(list("station" = behavioural$behavioural))
  data <- merge(behavioural_basins, attributes, by.x = "station", by.y = "grdc_ids", how="left")

  data$mean_precipitation_as_snow <- data$mean_precipitation_as_snow * 100.

  data_long <- tidyr::pivot_longer(data, cols = all_of(CHARACTERISTIC_LIST))
  data_long$name <-  factor(data_long$name,
                            levels = CHARACTERISTIC_LIST,
                            labels = LABELS_TO_USE)
  data_to_plot[[basin_group]] <- data_long

  # wetland_groups <- cut(data$localWetlands,
  #                       breaks = c(-0.1, 0.1, 5, 10, 20, 50, 100),
  #                       labels = c("no local wetlands", "1-5%", "5-10%", "10-20%",
  #                                  "20-50%", "50-100%"))
  # wetland_groups_df <- as.data.frame(table(wetland_groups))
  #
  #
  # lbls <- paste0(wetland_groups_df$wetland_groups, " (", wetland_groups_df$Freq, ")")
  # par(mar = c(4.1, 8.1, 4.1, 8.1)) #bg = NA,
  #
  # pie(wetland_groups_df$Freq, labels = lbls,
  #     border = "white", col = RColorBrewer::brewer.pal(6, "Blues"),
  #     radius = 0.6, cex = 2.5)
  # dev.copy(png,
  #          sprintf("./plots/Figure1_wetlands_%s.png", name), res=300,
  #          width = 30, height = 30, unit = "cm")
  # dev.off()
}

CEX=12
ggplot() +
  geom_histogram(data_to_plot[[2]], mapping = aes(value,
                                                  fill = names[2]),
                 alpha = 1) +
  geom_histogram(data_to_plot[[1]], mapping = aes(value,
                                                  fill = names[1]),
                 alpha = 0.7) +
  facet_wrap(~name, scales = "free",
             labeller = label_parsed) +
  scale_fill_manual(name = "",
                    values = c(
                      "behavioural" = datylon_map[5],
                      "non-behavioural" = datylon_map[7])) +
  theme_bw() +
  ylab("Count [-]") +
  xlab("") +
  theme(axis.text = element_text(size = 10)) +
  theme(panel.spacing = unit(1.5, "lines"),
        legend.position = "bottom") +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black"),
        legend.text = element_text(size=CEX, color="black"),
        legend.title = element_text(size=CEX, color="black"))



ggsave("./plots/review.Figure1_histogram.png",
       dpi = 600, units = "cm", width = 16, height = 12)


# Numbers for text to describe versatility in basins
data <- data_to_plot[[3]]
min(data$value[data$name == "Precipitation~as~snow~'[%]'"])
max(data$value[data$name == "Precipitation~as~snow~'[%]'"])
min(data$value[data$name == "Annual~precipitation~'[mm]'"])
max(data$value[data$name == "Annual~precipitation~'[mm]'"])
min(data$value[data$name == "Mean~temperature~'[°C]'"])
max(data$value[data$name == "Mean~temperature~'[°C]'"])
min(data$value[data$name == "Aridity~Index~'[-]'"])
max(data$value[data$name == "Aridity~Index~'[-]'"])

# plotting snow on wetlands as CDF
break_points <- seq(0,100,5)
behavioural_data = cut(data_to_plot[[2]]$localWetlands, break_points, right=FALSE)
behavioural_freq_table = table(behavioural_data)
behavioural_cumulative_frequency <- c(0, cumsum(behavioural_freq_table)/4)
behavioural_cumulative_frequency_percent <- behavioural_cumulative_frequency / nrow(data_to_plot[[2]]) * 100

nonbehavioural_data = cut(data_to_plot[[1]]$localWetlands, break_points, right=FALSE)
nonbehavioural_freq_table = table(nonbehavioural_data)
nonbehavioural_cumulative_frequency <- c(0, cumsum(nonbehavioural_freq_table)/4)
nonbehavioural_cumulative_frequency_percent <- nonbehavioural_cumulative_frequency / nrow(data_to_plot[[1]]) * 100

CEX <- 1.2
png("./plots/review.Figure1_cdf_snow.png", units="cm", bg="white", res=600, width=32, height=6)
par(mgp=c(1.8,0.7,0),mar=c(2.8,2.8,0.5,0)+0.1)
plot(break_points, behavioural_cumulative_frequency , type="l",
     xlab="Local wetlands [%] ",
     ylab="Cum. freq. [-]", cex.axis=CEX, cex.lab=CEX,
     col=datylon_map[5], lwd=3)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = 0.5)
lines(break_points, nonbehavioural_cumulative_frequency, col=datylon_map[7], lwd=3)
lines(break_points, behavioural_cumulative_frequency, col=datylon_map[5], lwd=3)
dev.off()

