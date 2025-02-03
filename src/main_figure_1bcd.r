rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")


ATTRIBUTES <- "./data/basin_attributes.txt"
CHARACTERISTIC_LIST <- c("sum_precipitation", "mean_temperature", "mean_precipitation_as_snow","aridity")
LABELS_TO_USE <- c("Annual~Precipitation~(mm)",
                   "Mean~Temperature~('°C')",
                   "Precipitation~as~snow~('%')",
                   "Aridity~Index~('-')")


data_to_plot <- list()
lower_thresholds <- list(NULL, 0.4, NULL)
higher_thresholds <- list(0.4,  NULL, NULL)
names <- c("additional", "behavioural", "all")

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

  wetland_groups <- cut(data$localWetlands,
                        breaks = c(-0.1, 0.1, 5, 10, 20, 50, 100),
                        labels = c("no local wetlands", "1-5%", "5-10%", "10-20%",
                                   "20-50%", "50-100%"))
  wetland_groups_df <- as.data.frame(table(wetland_groups))


  lbls <- paste0(wetland_groups_df$wetland_groups, " (", wetland_groups_df$Freq, ")")
  par(mar = c(4.1, 8.1, 4.1, 8.1)) #bg = NA,

  pie(wetland_groups_df$Freq, labels = lbls,
      border = "white", col = RColorBrewer::brewer.pal(6, "Blues"),
      radius = 0.6, cex = 2.5)
  dev.copy(png,
           sprintf("./plots/Figure1_wetlands_%s.png", name), res=300,
           width = 30, height = 30, unit = "cm")
  dev.off()
}


ggplot() +
  geom_histogram(data_to_plot[[1]], mapping = aes(value,
                                                  fill = names[1]),
                 alpha = 1) +
  geom_histogram(data_to_plot[[2]], mapping = aes(value,
                                                  fill = names[2]),
                 alpha = 0.7) +
  facet_wrap(~name, scales = "free",
             labeller = label_parsed) +
  scale_fill_manual(name = "",
                    values = c(
                      "behavioural" = datylon_map[2],
                      "additional" = datylon_map[5])) +
  theme_bw() +
  ylab("Count (-)") +
  xlab("Characteristic value") +
  theme(axis.text = element_text(size = 10)) +
  theme(panel.spacing = unit(1.5, "lines"))

ggsave("./plots/Figure1_histogram.png",
       dpi = 300, units = "cm", width = 16, height = 12)


# Numbers for text to describe versatility in basins
data <- data_to_plot[[3]]
min(data$value[data$name == "Precipitation~as~snow~('%')"])
max(data$value[data$name == "Precipitation~as~snow~('%')"])
min(data$value[data$name == "Annual~Precipitation~(mm)"])
max(data$value[data$name == "Annual~Precipitation~(mm)"])
min(data$value[data$name == "Mean~Temperature~('°C')"])
max(data$value[data$name == "Mean~Temperature~('°C')"])
min(data$value[data$name == "Aridity~Index~('-')"])
max(data$value[data$name == "Aridity~Index~('-')"])
