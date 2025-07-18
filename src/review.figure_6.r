rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

source("./src/helper/color_ramps.r")

plot_name_1 <- "./plots/figure6_big_zoom.png"
plot_name_2 <- "./plots/figure6_small_zoom.png"

BASIN <- "X4126701"
ROOT_REEFERENCE <- "./data/cal_result_discharges_model_m12_wetlStorage100.txt"
ROOT_COMPARE <- "./data/cal_result_discharges_model_m16_wetlStorage100.txt"

discharge_all_reference <- read.table(ROOT_REEFERENCE, sep = "\t", header = TRUE)
discharge_all_comparison <- read.table(ROOT_COMPARE, sep = "\t", header = TRUE)

discharge_reference <- discharge_all_reference[[BASIN]]
discharge_comparison <- discharge_all_comparison[[BASIN]]
date <- as.Date(discharge_all_comparison$date, "%Y-%m-%d")

threshold_low <- mean(discharge_reference) * 0.2
zoom_1_x <- c(as.Date("1989-02-01"), as.Date("1990-02-01"))
zoom_2_x <- c(as.Date("1989-11-01"), as.Date("1990-01-05"))
zoom_1_y <- c(0, 3300)
zoom_2_y <- c(40, 140)

data <- data.frame(date = date, schneider = discharge_comparison, as_lakes = discharge_reference)
data_to_plot <- data %>%
  tidyr::pivot_longer(!date)

ggplot(data_to_plot) +
  geom_line(aes(x = date, y = value, col = name), lwd = 1.) +
  scale_x_date(date_labels = "%b %Y") +
  xlim(zoom_1_x) +
  ylim(zoom_1_y) +
  labs(y = expression(Discharge ~ "[" ~ m^3 ~ s^-1 ~ "]"), x = "") +
  geom_hline(yintercept = threshold_low, colour = "black") +
  scale_linetype_manual(values = 1) +
  theme_classic() +
  scale_color_manual(
    name = "",
    values = c(
      "low flow" = "black",
      "schneider" = datylon_map[2],
      "as_lakes" = datylon_map[5]
    ),
    labels = c(
      "low flow" = "black",
      "as_lakes" = "reservoir algorithm (V1)",
      "schneider" = "reservoir algorithm (V2)"
    )
  ) +
  theme(
    legend.position = c(0.4, 0.9),
    legend.text = element_text(colour = "black", size = 14),
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14)
  )


ggsave(plot_name_1,
  units = "cm", width = 20, height = 14, dpi = 300
)


plot2 <- ggplot(data_to_plot) +
  geom_line(aes(x = date, y = value, col = name), lwd = 1.) +
  scale_x_date(date_labels = "%b %Y") +
  xlim(zoom_2_x) +
  ylim(zoom_2_y) +
  labs(y = expression(Discharge ~ "[" ~ m^3 ~ s^-1 ~ "]"), x = "") +
  geom_hline(yintercept = threshold_low, col = "black") +
  theme_classic() +
  scale_color_manual(
    name = "",
    values = c(
      "schneider" = datylon_map[2],
      "as_lakes" = datylon_map[5]
    )
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1)
  )

ggsave(plot_name_2,
  units = "cm", width = 10, height = 8, dpi = 300, plot = plot2
)
