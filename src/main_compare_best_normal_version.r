rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

ROOT_100d <- "./data/KGE_results/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_10d <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
models <- seq(4,19,1)

ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
attributes <- read.table(ATTRIBUTES, header=T, sep="\t")
attributes$grdc_ids <- paste0("X", attributes$grdc_ids)

cal_results_10d <- read.table(sprintf(ROOT_10d, models[1]), sep="\t", header=T)
cal_results_10d <- data.frame(station=cal_results_10d[,names(cal_results_10d) %in% c("station")])
cal_results_100d <- cal_results_10d

for (model in models){
  model_result <- read.table(sprintf(ROOT_10d, model), sep="\t", header=T)
  cal_results_10d[[sprintf("model_m%i", model)]] <- model_result$kge_val

  model_result <- read.table(sprintf(ROOT_100d, model), sep="\t", header=T)
  cal_results_100d[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

cal_results_100d <- merge(cal_results_100d, attributes,
                          by.x="station", by.y="grdc_ids",
                          all.x=T)
cal_results_100d <- cal_results_100d[order(cal_results_100d$station),]
cal_results_10d <- cal_results_10d[order(cal_results_10d$station),]


model_name <- "model_m13"
ref_name <- "model_m18"
sufficient <- (cal_results_100d[[ref_name]] > 0.2 |
                 cal_results_10d[[model_name]] > 0.2) &
  (abs(cal_results_100d[[ref_name]] - cal_results_10d[[model_name]]) > 0.01 &
     abs(cal_results_100d[[ref_name]] - cal_results_10d[[model_name]]) <= 0.1)
red_100d <- cal_results_100d[[ref_name]][sufficient]
red_10d <- cal_results_10d[[model_name]][sufficient]


boxplot(list("new"=red_10d,
             "standard"=red_100d),
        ylim=c(0, 1))

boxplot(list("new-standard"=red_10d-red_100d))
abline(h=0)



