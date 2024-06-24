
library(ggplot2)
library(dplyr)
library(gridExtra)

ROOT <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
models <- c(0, 1,2,3,4,6,8)

model <- 1
cal_results <- read.table(sprintf(ROOT, model), sep="\t", header=T)
cal_results <- data.frame(station=cal_results[,names(cal_results) %in% c("station")])

for (model in models){
  model_result <- read.table(sprintf(ROOT, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

mytable <- tidyr::pivot_longer(cal_results, cols=-station) %>%
  group_by(name) %>%
  summarize("KGE < zero" = sum(value < 0))


theme_table <- modifyList(ttheme_minimal(),
                                list(core=list(fg_params=list(cex=0.7))))
tidyr::pivot_longer(cal_results, cols=-station) %>%
  ggplot(cal_results, mapping=aes(value, col=name)) +
    stat_ecdf(geom = "line") +
    scale_color_manual(values=hcl.colors(length(models), "spectral")) +
    coord_cartesian(xlim = c(0, 1)) +
    annotation_custom(tableGrob(mytable, theme=ttheme_minimal()), xmin=0.5, xmax=1, ymin=0, ymax=0.5) +
    theme_bw()




