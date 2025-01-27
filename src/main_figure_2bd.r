rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")
source("./src/helper/comparison.r")


PLOT_PATTERN <- "./plots/Figure2_barplot_%s_%s.png"
additional <- FALSE
if (additional == TRUE) {
  min_qual <- NULL
  max_qual <- 0.4
  name <- "additional"
} else {
  min_qual <- 0.4
  max_qual <- NULL
  name <- "behavioural"
}


# comparison pairs: snow
snow <- c("model_m9_100", "model_m10_100", "model_m13_100","model_m15_100", "model_m17_100", "model_m19_100")
no_snow <- c("model_m8_100", "model_m11_100", "model_m12_100","model_m14_100", "model_m16_100", "model_m18_100")

# comparison pairs: flow velocity
variable <- c("model_m11_100", "model_m10_100", "model_m14_100","model_m15_100", "model_m18_100", "model_m19_100")
static <- c("model_m8_100", "model_m9_100", "model_m12_100","model_m13_100", "model_m16_100", "model_m17_100")

#comparison pairs: reservoirs hanaski
no_res <- c("model_m8_100", "model_m9_100", "model_m10_100", "model_m11_100")
hanasaki <- c("model_m12_100", "model_m13_100", "model_m15_100", "model_m14_100")
schneider <- c("model_m16_100", "model_m17_100", "model_m19_100", "model_m18_100")

columns <- c("NSE_val", "KGE_val")
behavioural_basins <- read_kge_and_define_good_basins(min_qual, max_qual)
all_to_check <- c(variable, hanasaki, schneider, snow)
all_ref <- c(static, no_res, no_res, no_snow)
names <- c(rep("variable flow velocity", length(variable)),
           rep("reservoir algorithm (V1)", length(hanasaki)),
           rep("reservoir algorithm (V2)", length(schneider)),
           rep("snow on wetlands", length(snow)))
for (column in columns){
  print(column)

  data_frame <- setNames(
    data.frame(matrix(NA, ncol = 3,
                      nrow = length(all_to_check) * 2)),
    c("type", "condition", "value"))

  info <- read_benchmarks_all(column = column)
  reduced_info <- info[info$station %in% behavioural_basins$behavioural,]


  for (entry in 1:length(all_ref)){
    diff_snow <- reduced_info[[all_to_check[entry]]] - reduced_info[[all_ref[entry]]]
    worse <- sum(diff_snow < -0.01)
    better <- sum(diff_snow > 0.01)

    type <- names[entry]
    data_frame[entry,] <- list(type, "worse", as.numeric(worse))
    data_frame[length(all_ref)+entry,] <- list(type, "better", as.numeric(better))
  }

  data_frame$type <- factor(data_frame$type, levels=unique(names))
  plot_name <- sprintf(PLOT_PATTERN, column, name)
  print(ggplot(
    data_frame, aes(fill = condition, y = value, x = type)) +
    geom_boxplot() +
    #ggtitle(column) +
    ylab("#basins") +
    scale_fill_manual(values = c(datylon_map[2], datylon_map[5])) +
    xlab("") +
    theme_bw() +
    ylim(c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)))

  ggsave(plot_name, dpi = 300, units = "cm", width = 12, height = 10)
}
