rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

PLOT_PATTERN <- "./plots/review/supplement_s1_%s_%s.png"
CEX <- 12

hanasaki_pairs <- list("M1" = c("model_m8_100", "model_m12_100"),
                       "M2" = c("model_m9_100", "model_m13_100"),
                       "M3" = c("model_m11_100", "model_m14_100"),
                       "M4" = c("model_m10_100", "model_m15_100")
)
schneider_pairs <- list("M1" = c("model_m12_100", "model_m16_100"),
                        "M2" = c("model_m13_100", "model_m17_100"),
                        "M3" = c("model_m14_100", "model_m18_100"),
                        "M4" = c("model_m15_100", "model_m19_100")
)

snow_pairs <- list("M1" = c("model_m8_100", "model_m9_100"),
                   "M2" = c("model_m11_100", "model_m10_100"),
                   "M3" = c("model_m12_100", "model_m13_100"),
                   "M4" = c("model_m14_100", "model_m15_100"),
                   "M5" = c("model_m16_100", "model_m17_100"),
                   "M6" = c("model_m18_100", "model_m19_100")
)

velocity_pairs <- list("M1"= c("model_m8_100", "model_m11_100"),
                       "M2" = c("model_m9_100", "model_m10_100"),
                       "M3" = c("model_m12_100", "model_m14_100"),
                       "M4"= c("model_m13_100", "model_m15_100"),
                       "M5"= c("model_m16_100", "model_m18_100"),
                       "M6"= c("model_m17_100", "model_m19_100")
)

groups <- list("hanasaki" = hanasaki_pairs,
               "snow" = snow_pairs,
               "velocity" = velocity_pairs,
               "schneider" = schneider_pairs)

for (column in c("logNSE_val", "KGE_val")) { #"logNSE_val", "KGE_mod_val",
  print(column)
  ylabel <- sprintf("\u0394%s", stringr::str_remove(column, "_val"))

  behavioural_basins <- read_kge_and_define_good_basins()
  kge_info <- read_benchmarks_all(column = column)




  for (group_name in names(groups)) {
    print(group_name)

    to_analyse <- groups[[group_name]]
    target_plot <- sprintf(PLOT_PATTERN, column, group_name)

    comparison_data <- NULL
    for (label in names(to_analyse)){

      mset_data <- compare_models(kge_info,
                                      to_analyse[[label]][1],
                                      to_analyse[[label]][2],
                                      label)

      comparison_data <- rbind(comparison_data, mset_data)
    }

    comparison_data$set <- ifelse(comparison_data$basin_id %in% behavioural_basins$behavioural,
                           "behavioural", "non-behavioural")

    if (group_name %in% c("hanasaki", "schneider")){
      affected_group <- get_sensitive_basins("reservoir")
    } else {
      affected_group <- get_sensitive_basins(group_name)
    }
    comparison_data <- comparison_data[comparison_data$basin_id %in% affected_group,]

    ylim <- c(-0.4, 0.7)
    vjust <- 0.5

    comparison_data %>%
      group_by(label) %>%
      mutate(count = n()) %>%
      mutate(mean = mean(value)) %>%
      filter(abs(value) > 0.01) %>%
      mutate(count_with_change = n()) %>%
      mutate(label_as_label = factor(
        label,
        levels = c("M1", "M2", "M3", "M4", "M5", "M6"),
        labels = c("M1", "M2", "M3", "M4", "M5", "M6"))) %>%
      ggplot(., aes(x = label_as_label, y = value, fill=set)) +
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
      labs(x = "",
          y = ylabel) +
      coord_cartesian(ylim = ylim)

    ggsave(target_plot, units = "cm", width = 16, height = 16, dpi = 300)
  }
}
