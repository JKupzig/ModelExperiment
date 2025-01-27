rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

PLOT_PATTERN <- "./plots/supplement/Supplement_S2_%s_%s.png"
additional <- TRUE

for (column in c("r_val", "logNSE_val", "KGE_mod_val",
                 "NSE_val", "KGE_val", "d1_val")){

  print(column)

  ylabel <- sprintf("\u0394%s", stringr::str_remove(column, "_val"))

  if (additional == TRUE) {
    min_qual <- NULL
    max_qual <- 0.4
    name <- "additional"
  } else {
    min_qual <- 0.4
    max_qual <- NULL
    name <- "behavioural"
  }
  
  target_plot <- sprintf(PLOT_PATTERN, column, name)
  behavioural_basins <- read_kge_and_define_good_basins(min_qual, max_qual)
  kge_info <- read_benchmarks_all(column = column)

  hanasaki_data <- compare_models(kge_info,
                                  "model_m8_100",
                                  "model_m12_100",
                                  "hanasaki")

  schneider_data <- compare_models(kge_info,
                                   "model_m8_100",
                                   "model_m16_100",
                                   "schneider")


  snow_data <- compare_models(kge_info,
                              "model_m8_100",
                              "model_m9_100",
                              "snow on wetlands")

  velocity_data <- compare_models(kge_info,
                                  "model_m8_100",
                                  "model_m11_100",
                                  "variable flow velocity")

  comparison_data <- rbind(hanasaki_data,
                           schneider_data,
                           snow_data,
                           velocity_data)

  reduced_data <- comparison_data[comparison_data$basin_id %in% behavioural_basins$behavioural,]
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
  reduced_data %>%
    group_by(label) %>%
    mutate(count = n()) %>%
    mutate(mean = mean(value)) %>%
    filter(abs(value) > 0.01) %>%
    mutate(count_with_change = n()) %>%
    mutate(label_as_label = factor(label, levels=c("variable flow velocity", "hanasaki", "schneider",
                                                   "slow wetlands", "fast wetlands",
                                                   "snow on wetlands"),
                                   labels = c("variable flow velocity",
                                              "reservoir algorithm (V1)",
                                              "reservoir algorithm (V2)",
                                              "slower wetlands", "fast wetlands",
                                              "snow on wetlands"))) %>%
    ggplot(., aes(x = label_as_label, y = value)) +
    geom_boxplot(fill = "grey") +
    geom_violin(alpha = 0.2) +
    geom_hline(yintercept = 0, col = "firebrick", linetype = "dashed", linewidth = 1.2) +
    scale_color_manual(values=hcl.colors(length(unique(comparison_data$label)), "Berlin")) +
    theme_classic() +
    theme(axis.text.y = element_text(colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, colour = "black")) +
    geom_label(aes(label = count_with_change, y = vjust),
               size = 4, position = position_dodge(width = 0.75)) +
    labs(
      x = "",
      y = ylabel) +
    coord_cartesian(ylim = ylim)

  ggsave(target_plot, units = "cm", width = 12, height = 10, dpi = 300)
}
