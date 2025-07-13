rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

plot_name <- "./plots/review/appendixA1b_behavioural_characteristics.png"

ATTRIBUTES <- "./data/basin_attributes.txt"
CHARACTERISTIC_LIST <- c("sum_precipitation", "mean_temperature", "mean_precipitation_as_snow", "aridity", "localWetlands", "globalLakes")
LABELS_TO_USE <- c(
  "Annual precipitation [mm]",
  "Mean temperature [°C]",
  "Precipitation as snow [%]",
  "Aridity Index [-]",
  "Local wetlands [%]",
  "Global lakes [%]"
)


data_to_plot <- list()

names <- c("behavioural", "non-behavioural")

attributes <- read.table(ATTRIBUTES, sep = "\t", header = TRUE)
for (name in names) {
  affected_basins <- read_kge_and_define_good_basins()

  if (name == "non-behavioural") {
    data <- attributes[!attributes$grdc_ids %in% affected_basins$behavioural, ]
  } else {
    data <- attributes[attributes$grdc_ids %in% affected_basins$behavioural, ]
  }

  data_long <- tidyr::pivot_longer(data, cols = all_of(CHARACTERISTIC_LIST))
  data_long$name <- factor(data_long$name,
    levels = CHARACTERISTIC_LIST,
    labels = LABELS_TO_USE
  )
  data_long$set <- name
  data_to_plot[[name]] <- data_long
}

data_all <- do.call(rbind, data_to_plot)

CEX <- 12
ggplot(data_all) +
  stat_ecdf(geom = "step", aes(x = value, color = set), linewidth = 1) +
  facet_wrap(. ~ name, scales = "free", ncol = 2) +
  theme_bw() +
  xlab("") +
  ylab("Cum. freq [-]") +
  scale_color_manual(
    name = "", values =
      c(
        "behavioural" = datylon_map[4],
        "non-behavioural" = datylon_map[6]
      )
  ) +
  theme(axis.text = element_text(size = CEX)) +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(-10, -10, -10, -10),
    legend.box.spacing = unit(0, "pt")
  ) +
  theme(
    axis.text = element_text(size = CEX, color = "black"),
    axis.title = element_text(size = CEX, color = "black"),
    strip.text = element_text(size = CEX, color = "black"),
    legend.text = element_text(size = CEX, color = "black"),
    legend.title = element_text(size = CEX, color = "black")
  )

ggsave(plot_name,
  dpi = 600, units = "cm", width = 14, height = 14
)



# Numbers for text to describe versatility in basins
data <- data_to_plot[["behavioural"]]
min(data$value[data$name == "Precipitation as snow [%]"])
max(data$value[data$name == "Precipitation as snow [%]"])
min(data$value[data$name == "Annual precipitation [mm]"])
max(data$value[data$name == "Annual precipitation [mm]"])
min(data$value[data$name == "Mean temperature [°C]"])
max(data$value[data$name == "Mean temperature [°C]"])
min(data$value[data$name == "Aridity Index [-]"])
max(data$value[data$name == "Aridity Index [-]"])
