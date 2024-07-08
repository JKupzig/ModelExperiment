
rm(list=ls())
library(ggplot2)
library(dplyr)
library(gridExtra)

ROOT <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
models <- seq(0,19,1)


model <- 1
cal_results <- read.table(sprintf(ROOT, model), sep="\t", header=T)
cal_results <- data.frame(station=cal_results[,names(cal_results) %in% c("station")])

for (model in models){
  model_result <- read.table(sprintf(ROOT, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

compare_models_wateruse <- function(cal_results, reference_column, to_compare_column, min_quality = 0.2){
  ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
  attributes <- read.table(ATTRIBUTES, header=T, sep="\t")

  median_water_stress <- median(attributes$water_stress)
  stations_to_examine <- attributes$grdc_ids[attributes$water_stress > 5]

  station_sufficient_quality <- cal_results$station[cal_results[reference_column] > min_quality |
                                                    cal_results[to_compare_column] > min_quality]

  selected_stations <- cal_results$station %in% c(station_sufficient_quality, paste0("X", stations_to_examine))
  cal_results_intersting <- cal_results[selected_stations,]
  cal_results_others <- cal_results[!selected_stations,]

  delta <- cal_results_intersting[[to_compare_column]]  - cal_results_intersting[[reference_column]]
  data <- data.frame("value"= delta,
                     "label"= paste(reference_column, "affected"))
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


# water use
data_1 <- compare_models_wateruse(cal_results, "model_m1", "model_m0")
data_2 <- compare_models_wateruse(cal_results, "model_m2", "model_m3")
data_3 <- compare_models_wateruse(cal_results, "model_m8", "model_m4")
data_4 <- compare_models_wateruse(cal_results, "model_m9", "model_m5")
data_5 <- compare_models_wateruse(cal_results, "model_m10", "model_m7")

all_together <- rbind(data_1, data_2, data_3, data_4, data_5)
all_together <- all_together[(abs(all_together$value) > 0.01),]
ggplot(all_together, aes(x=label, y=value, col=label)) +
  geom_boxplot() +
  scale_color_manual(values=hcl.colors(length(unique(all_together$label)), "spectral")) +
  theme_bw() +
  coord_cartesian(ylim = c(-1, 1))




ggplot(data, aes(x=delta, group=label, col=label) ) +
  geom_boxplot() +
  theme_bw()

boxplot(list("all"=delta_1, "affected"=delta_1_intersting))


