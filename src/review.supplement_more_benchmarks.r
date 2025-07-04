rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

PLOT_PATTERN <- "./plots/review/supplement_S2_%s.png"
CEX = 12
reservoir_set <- get_sensitive_basins("reservoirs")
snow_set <- get_sensitive_basins("snow")


for (column in c("r_val", "logNSE_val", "KGE_mod_val",
                 "NSE_val", "KGE_val", "d1_val")){

  target_plot <- sprintf(PLOT_PATTERN, column)

  print(column)

  ylabel <- sprintf("\u0394%s [-]", stringr::str_remove(column, "_val"))

  behavioural_basins <- read_kge_and_define_good_basins()
  kge_info <- read_benchmarks_all(column = column)

  hanasaki_data <- compare_models(kge_info,
                                  "model_m8_100",
                                  "model_m12_100",
                                  "hanasaki")
  hanasaki_data <- hanasaki_data[hanasaki_data$basin_id %in% reservoir_set,]

  schneider_data <- compare_models(kge_info,
                                   "model_m12_100",
                                   "model_m16_100",
                                   "schneider")
  schneider_data <- schneider_data[schneider_data$basin_id %in% reservoir_set,]

  snow_data <- compare_models(kge_info,
                              "model_m8_100",
                              "model_m9_100",
                              "snow on wetlands")
  snow_data <- snow_data[snow_data$basin_id %in% snow_set,]

  velocity_data <- compare_models(kge_info,
                                "model_m8_100",
                                "model_m11_100",
                                "variable flow velocity")


  comparison_data <- rbind(hanasaki_data,
                           schneider_data,
                           snow_data,
                           velocity_data)

  comparison_data$set <- ifelse(
    comparison_data$basin_id %in% behavioural_basins$behavioural, "behavioural", "non-behavioural"
  )

  comparison_data$label <- factor(
    comparison_data$label,
    levels=c("snow on wetlands", "hanasaki", "schneider", "variable flow velocity"),
    labels = c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity")
  )
  ylim <- c(-0.4, 0.7)
  vjust <- 0.5
  if (column == "d1_val") {
    ylim <- c(-0.2, 0.3)
    vjust <- 0.2
  }
  if (column %in% c("r_val", "a_val")) {
    ylim <- c(-0.25, 0.25)
    vjust <- 0.2
  }
  comparison_data %>%
    group_by(label) %>%
    mutate(count = n()) %>%
    mutate(mean = mean(value)) %>%
    filter(abs(value) > 0.01) %>%
    mutate(count_with_change = n()) %>%
    ggplot(., aes(x = label, y = value, fill=set)) +
    geom_boxplot(position = position_dodge(0.9),) +
    geom_violin(alpha = 0.2, position = position_dodge(0.9),) +
    geom_hline(yintercept = 0, col = "firebrick", linetype = "dashed", linewidth = 1.2) +
    scale_fill_manual(name="",values=c("behavioural"=datylon_map[5],
                                "non-behavioural"=datylon_map[7])) +
    theme_bw() +
    theme(
      legend.position = c(0.15, 0.9),
      legend.background = element_blank(),
      legend.ticks = element_blank(),
      legend.frame = element_blank(),
      legend.text = element_text(colour = "black", size=CEX),
      axis.title = element_text(colour = "black", size=CEX),
      axis.text.y = element_text(colour = "black", size=CEX),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, colour = "black", size=CEX)) +
    labs(
      x = "",
      y = ylabel) +
    coord_cartesian(ylim = ylim)

  ggsave(target_plot, units = "cm", width = 16, height = 16, dpi = 300)

}

# info text:
column <- "KGE_val"
kge_info <- read_benchmarks_all(column = column)
velocity_data <- compare_models(kge_info, "model_m8_100", "model_m11_100", "variable flow velocity")
merken <- velocity_data[velocity_data$basin_id %in% behavioural_basins$behavioural, ]
sum(merken < -0.01) #87
sum(merken >= -0.01 & merken <= 0.01) #12

column <- "NSE_val"
kge_info <- read_benchmarks_all(column = column)
velocity_data <- compare_models(kge_info, "model_m8_100", "model_m11_100", "variable flow velocity")
merken <- velocity_data[velocity_data$basin_id %in% behavioural_basins$behavioural, ]
sum(merken < -0.01) #95
sum(merken >= -0.01 & merken <= 0.01) #12


