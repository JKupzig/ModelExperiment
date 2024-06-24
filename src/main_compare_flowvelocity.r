
rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

################################################################################
# calibrated version
################################################################################
ROOT <- "./data/KGE_results/cal_result_appendix_variableflow.rds"
sim_results <- readRDS(ROOT)

min_quality <- 0.2
station_sufficient_quality <- (val_static > min_quality | val_variable > min_quality)

val_static <- sim_results[2, 1, 1,]
val_variable <- sim_results[2, 1, 2,]

boxplot(list("static"=val_static[station_sufficient_quality],
             "variable"=val_variable[station_sufficient_quality]))
################################################################################
# MC version
################################################################################
ROOT <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
models <- c(0,1,2,3,4,6,8,11,12,14)

model <- 1
cal_results <- read.table(sprintf(ROOT, model), sep="\t", header=T)
cal_results <- data.frame(station=cal_results[,names(cal_results) %in% c("station")])

for (model in models){
  model_result <- read.table(sprintf(ROOT, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

compare_models <- function(cal_results, reference_column, to_compare_column,
                           label_to_use, min_quality = 0.2){

  station_sufficient_quality <- cal_results$station[cal_results[reference_column] > min_quality |
                                                      cal_results[to_compare_column] > min_quality]

  selected_stations <- cal_results$station %in% c(station_sufficient_quality)
  cal_results_intersting <- cal_results[selected_stations,]
  cal_results_others <- cal_results[!selected_stations,]

  delta <- (cal_results_intersting[[to_compare_column]]  -
            cal_results_intersting[[reference_column]])

  data <- data.frame("value"= delta,
                     "label"= label_to_use)
  # data <- rbind(data,
  #               data.frame("value"=cal_results_intersting[[to_compare_column]],
  #                          "label"= paste(to_compare_column, "affected")))
  # # data <- rbind(data,
  #               data.frame("value"=cal_results_others[[reference_column]],
  #                          "label"= paste(reference_column, "not affected")))
  # data <- rbind(data,
  #               data.frame("value"=cal_results_others[[to_compare_column]],
  #                          "label"= paste(to_compare_column, "not affected")))

  return(data)
}


apply(cal_results[,-c(1)], 2, median)
data_1 <- compare_models(cal_results, "model_m0", "model_m3", "simplest")
data_2 <- compare_models(cal_results, "model_m1", "model_m2", "water use")
data_3 <- compare_models(cal_results, "model_m8", "model_m11", "lakes + water use")

all_together <- rbind(data_1, data_2, data_3)
ggplot(all_together, aes(x=label, y=value)) +
  geom_boxplot(fill="grey") +
  geom_violin(alpha=0.2) +
  geom_hline(yintercept=0, col="firebrick", linetype="dashed", linewidth=1.1) +
  scale_color_manual(values=hcl.colors(length(unique(all_together$label)), "Berlin")) +
  theme_classic()
#coord_cartesian(xlim = c(0, 1))
