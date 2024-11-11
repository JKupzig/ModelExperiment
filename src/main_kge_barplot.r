rm(list=ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")

MIN_QUAL <- 0.0 #0.4 - 0.2
MAX_QUAL <- 0.4 #NULL - 0.4
DUMMY <- NULL

# comparison pairs: snow
snow <- c("model_m9_100", "model_m10_100", "model_m13_100","model_m15_100", "model_m17_100", "model_m19_100")
no_snow <- c("model_m8_100", "model_m11_100", "model_m12_100","model_m14_100", "model_m16_100", "model_m18_100")

# comparison pairs: flow velocity
variable <- c("model_m11_100", "model_m10_100", "model_m14_100","model_m15_100", "model_m18_100", "model_m19_100")
static <- c("model_m8_100", "model_m9_100", "model_m12_100","model_m13_100", "model_m16_100", "model_m17_100")

#comparison pairs: reservoirs hanaski
no_res <- c("model_m8_100", "model_m9_100", "model_m10_100","model_m11_100")
hanasaki <-c("model_m12_100", "model_m13_100", "model_m15_100","model_m14_100")
schneider <-c("model_m16_100", "model_m17_100", "model_m19_100","model_m18_100")

columns <- c("logNSE_val", "KGE_mod_val", "NSE_val", "KGE_val", "d1_val")
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)
all_to_check <- c(variable, hanasaki, schneider, snow)
all_ref <- c(static, no_res, no_res, no_snow)
names <- c(rep("variable flow velocity", length(variable)),
           rep("reservoir algorithm (V1)", length(hanasaki)),
           rep("reservoir algorithm (V2)", length(schneider)),
           rep("snow on wetlands", length(snow)))
for (column in columns){
  print(column)

  data_frame <- setNames(
    data.frame(matrix(NA,ncol=3,
                      nrow=length(all_to_check)*2)),
    c("type", "condition", "value"))

  info <- read_benchmarks_all(column =column)
  if (column == "d1_val"){
    info[,names(info) != "station"] <- 1 - info[,names(info) != "station"]
  }
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
  plot_name <- sprintf("./plots/KGE_barplot_%s_%f.png", column, MIN_QUAL)
  print(ggplot(data_frame,
               aes(fill=condition, y=value, x=type)) +
    geom_boxplot() +
    #ggtitle(column) +
    ylab("#basins") +
    scale_fill_manual(values=c("firebrick", "cornflowerblue")) +
    #scale_y_continuous(trans = "log10") +
    xlab("") +
    theme_bw() +
    ylim(c(0, 100)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)))

  ggsave(plot_name, dpi=300, units="cm", width=12, height=10)
}

