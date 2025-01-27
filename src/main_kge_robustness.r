rm(list=ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

MIN_QUAL <- 0.4 #0.4 - 0.2
MAX_QUAL <- NULL #NULL - 0.4
DUMMY <- NULL

hanasaki_pairs <- list("M1"= c("model_m8_100", "model_m12_100"),
                       "M2" = c("model_m9_100", "model_m13_100"),
                       "M3" = c("model_m11_100", "model_m14_100"),
                       "M4"= c("model_m10_100", "model_m15_100")
)
schneider_pairs <- list("M1"= c("model_m8_100", "model_m16_100"),
                        "M2" = c("model_m9_100", "model_m17_100"),
                        "M3" = c("model_m11_100", "model_m18_100"),
                        "M4"= c("model_m10_100", "model_m19_100")
)

snow_pairs <- list("M1"= c("model_m8_100", "model_m9_100"),
                   "M2" = c("model_m11_100", "model_m10_100"),
                   "M3" = c("model_m12_100", "model_m13_100"),
                   "M4"= c("model_m14_100", "model_m15_100"),
                   "M5"= c("model_m16_100", "model_m17_100"),
                   "M6"= c("model_m18_100", "model_m19_100")
)

velocity_pairs <- list("M1"= c("model_m8_100", "model_m11_100"),
                       "M2" = c("model_m9_100", "model_m10_100"),
                       "M3" = c("model_m12_100", "model_m14_100"),
                       "M4"= c("model_m13_100", "model_m15_100"),
                       "M5"= c("model_m16_100", "model_m18_100"),
                       "M6"= c("model_m17_100", "model_m19_100")
)
for (column in c("logNSE_val", "KGE_mod_val", "NSE_val", "KGE_val")) {

  ylabel = sprintf("\u0394%s", stringr::str_remove(column, "_val"))

  behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)
  kge_info <- read_benchmarks_all(column = column)

  comparison_data <- NULL
  target_plot <- sprintf("./plots/supplement/S1_%s_min_kge_%f_hanasaki.png", column, MIN_QUAL)
  to_analyse <- hanasaki_pairs
  for (label in names(to_analyse)){

    hanasaki_data <- compare_models(kge_info,
                                    to_analyse[[label]][1],
                                    to_analyse[[label]][2],
                                    label,
                                    min_quality = DUMMY)

    comparison_data <- rbind(comparison_data, hanasaki_data)
  }




  reduced_data <- comparison_data[comparison_data$basin_id %in% behavioural_basins$behavioural,]
  ylim <- c(-0.4,0.7)
  vjust = 0.5

  reduced_data %>%
    group_by(label) %>%
    mutate(count = n()) %>%
    mutate(mean = mean(value)) %>%
    filter(abs(value) > 0.01) %>%
    mutate(count_with_change = n()) %>%
    mutate(label_as_label = factor(label, levels=c("M1", "M2", "M3", "M4", "M5", "M6"),
                                   labels = c("M1", "M2", "M3", "M4", "M5", "M6"))) %>%
    ggplot(., aes(x=label_as_label, y=value)) +
    geom_boxplot(fill="grey") +
    geom_violin(alpha=0.2) +
    geom_hline(yintercept=0, col="firebrick", linetype="dashed", linewidth=1.2) +
    scale_color_manual(values=hcl.colors(length(unique(comparison_data$label)), "Berlin")) +
    theme_classic() +
    theme(axis.text.y = element_text(colour="black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, colour="black")) +
    geom_label(aes(label= count_with_change , y = vjust), # mean + 0.1 <- change this to move label up and down
               size = 4, position = position_dodge(width = 0.75)) +
    labs(x="",
         y=ylabel) +
    coord_cartesian(ylim=ylim)

  ggsave(target_plot, units="cm", width=12, height=10, dpi=300)
}






