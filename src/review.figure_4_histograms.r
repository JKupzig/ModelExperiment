rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")
source("./src/helper/comparison.r")


PLOT_PATTERN <- "./plots/review.histogram_%s.png"
PLOT_PATTERN2 <- "./plots/review.histogram_notaffected_%s.png"
CEX = 7
behavioural_set <- read_kge_and_define_good_basins(min_kge = 0.4, max_kge = NULL)
ROOT_ATTRIBUTES <- "./data/basin_attributes.txt"
attributes <- read.table(ROOT_ATTRIBUTES, sep = "\t", header = TRUE)

snow <- list(c(12,13), c(14, 15), c(16, 17), c(18, 19), c(8,9), c(11, 10))
reservoir1 <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))
reservoir <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))
river <- list(c(12, 14), c(13, 15), c(16, 18), c(17, 19), c(8, 11), c(9, 10))

# reservoir2 <- list(c(8,16), c(9, 17), c(11, 18), c(10, 19))


columns <- c("KGE_val", "logNSE_val") # KGE & logNSE

names <- c(rep("variable flow velocity", length(river)),
           rep("reservoir algorithm (V1)", length(reservoir1)),
           rep("reservoir algorithm (V2)", length(reservoir)),
           rep("snow on wetlands", length(snow)))
all_to_check <- c(river, reservoir1, reservoir, snow)
intervals <- c(-100, -0.2, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.2, 100)
interval_labels <- c("<-0.2", "-0.2 to -0.1", "-0.1 to -0.05", "-0.05 to -0.01",
                    "-0.01 to 0.01", "0.01 to 0.05", "0.05 to 0.1", "0.1 to 0.2", ">0.2")

for (column in columns){
  print(column)

  plot_name <- sprintf(PLOT_PATTERN, column)
  plot_name2 <- sprintf(PLOT_PATTERN2, column)

  result <- list()
  for (set_name in c("behavioural", "complete")){

    data_frame <- setNames(
      data.frame(matrix(NA, ncol = 3,
                        nrow = length(names)*(length(intervals)-1))),
      c("model_part", "interval", "count"))

    info <- read_benchmarks_all(column = column)
    info <- merge(info, attributes, by.x="station", by.y="grdc_ids", how="left")

    if (set_name == "behavioural"){
      info <- info[info$station %in% behavioural_set$behavioural,]
    } else {
        info <- info[!info$station %in% behavioural_set$behavioural,]
    }

    start_row <- 1
    end_row <- 1 + length(intervals)-2

    for (entry in 1:length(names)){
      name <- names[entry]

      if (name == "snow on wetlands"){
          sensitive_data <- info[
            (info$mean_precipitation_as_snow > 20) &
              (info$localWetlands > 10),]
      } else if ((name == "reservoir algorithm (V1)") | (name == "reservoir algorithm (V2)")) {
        sensitive_data <- info[info$reservoir_area > 5,]
      } else {
        sensitive_data <- info
      }


      ref_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][1])
      comp_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][2])
      diff <- sensitive_data[[comp_model]]- sensitive_data[[ref_model]]
      cutted_diff <- cut(diff, intervals,include.lowest = TRUE)
      hist_values <- table(cutted_diff)

      name <- names[entry]

      data_frame[start_row:end_row, 1] <- name
      data_frame[start_row:end_row, 2] <- names(hist_values)
      data_frame[start_row:end_row, 3] <- as.numeric(hist_values)

      start_row <- start_row + length(intervals)-1
      end_row <- end_row + length(intervals)-1
    }

    summarized_values <- data_frame %>%
      group_by(model_part, interval) %>%
      dplyr::summarize(., lower=min(count), mean=median(count), upper=max(count)) %>%
      data.frame()
    summarized_values$set <- set_name

    result[[set_name]] <- summarized_values
  }

  summarized_values <- do.call(rbind, result)
  summarized_values$interval <- factor(
    summarized_values$interval,
    levels=names(hist_values),
    labels = interval_labels)
  summarized_values$model_part <- factor(
    summarized_values$model_part,
    levels=c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity"),
    labels = c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity"))
  summarized_values$set <- factor(
    summarized_values$set,
    levels = c("complete", "behavioural"),
    labels=c("non-behav.", "behav."))

  summarized_values$cor_mean <- summarized_values$mean
  summarized_values$cor_mean[37:72] <- summarized_values$mean[1:36] + summarized_values$mean[37:72]
  summarized_values$cor_lower <- summarized_values$lower
  summarized_values$cor_lower[37:72] <- summarized_values$mean[1:36] + summarized_values$lower[37:72]
  summarized_values$cor_upper <- summarized_values$upper
  summarized_values$cor_upper[37:72] <- summarized_values$mean[1:36] + summarized_values$upper[37:72]

  summarized_values$cor_upper <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_upper)
  summarized_values$cor_lower <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_lower)
  summarized_values$cor_mean <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_mean)

  summarized_values %>%
    group_by(interval) %>%
    ggplot(aes(x = interval, y = mean, fill=set)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA)) +
    facet_wrap(. ~ model_part, scales="free_y", nrow=1) +
    geom_errorbar(aes(y = cor_mean, ymin = cor_lower, ymax = cor_upper), width = 0.4,
                  position = position_dodge(width = 0.5), size=0.3) +
    geom_point(aes(y = cor_mean), position = position_dodge(width = 0.5), size=0.4) +
    scale_fill_manual(values = c(datylon_map[7],datylon_map[5])) +
    theme_bw() +
    theme(
      legend.position = c(0.055, 0.9), # c(0,0) bottom left, c(1,1) top-right.
      legend.background = element_rect(fill = NA, colour = NA),
      legend.title=element_blank(),
      legend.text = element_text(size=CEX-1, color="black"),
      legend.key.size = unit(c(0.5), c("line")),
    ) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    theme(axis.text=element_text(size=CEX, color="black"),
          axis.title=element_text(size=CEX, color="black"),
          strip.text = element_text(size = CEX, color="black")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

    xlab(bquote(Delta ~ .(strsplit(column, "_")[[1]][1]) ~ "[-]")) +
    ylab("Count [-]")

  ggsave(plot_name, dpi = 600, units = "cm", width = 16, height = 7)


# check not-affected basins for snow and reservoirs
data_frame <- setNames(
  data.frame(matrix(NA, ncol = 3,
                    nrow = (length(names)-6)*(length(intervals)-1))),
  c("model_part", "interval", "count"))

info <- read_benchmarks_all(column = column)
info <- merge(info, attributes, by.x="station", by.y="grdc_ids", how="left")

start_row <- 1
end_row <- 1 + length(intervals)-2

for (entry in 7:length(names)){
  name <- names[entry]

  if (name == "snow on wetlands"){
    sensitive_data <- info[
      !((info$mean_precipitation_as_snow > 20) &
        (info$localWetlands > 10)),]
  } else if ((name == "reservoir algorithm (V1)") | (name == "reservoir algorithm (V2)")) {
    sensitive_data <- info[!(info$reservoir_area > 5),]
  } else {
    sensitive_data <- info
  }

  ref_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][1])
  comp_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][2])
  diff <- sensitive_data[[comp_model]]- sensitive_data[[ref_model]]
  cutted_diff <- cut(diff, intervals,include.lowest = TRUE)
  hist_values <- table(cutted_diff)

  data_frame[start_row:end_row, 1] <- name
  data_frame[start_row:end_row, 2] <- names(hist_values)
  data_frame[start_row:end_row, 3] <- as.numeric(hist_values)

  start_row <- start_row + length(intervals)-1
  end_row <- end_row + length(intervals)-1
}

summarized_values <- data_frame %>%
group_by(model_part, interval) %>%
dplyr::summarize(., lower=min(count), mean=median(count), upper=max(count)) %>%
data.frame()
summarized_values$set <- set_name
summarized_values$interval <- factor(
  summarized_values$interval,
  levels=names(hist_values),
  labels = interval_labels)
summarized_values$model_part <- factor(
  summarized_values$model_part,
  levels=c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity"),
  labels = c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity"))

summarized_values %>%
  group_by(interval) %>%
  ggplot(aes(x = interval, y = mean, fill=set)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  facet_wrap(. ~ model_part, scales="free_y", nrow=1) +
  geom_errorbar(aes(y = mean, ymin = lower, ymax = upper), width = 0.4,
                position = position_dodge(width = 0.5), size=0.3) +
  geom_point(aes(y = mean), position = position_dodge(width = 0.5), size=0.4) +
  scale_fill_manual(values = c("darkgrey")) +
  theme_bw() +
  theme(
    legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = NA, colour = NA),
    legend.title=element_blank(),
    legend.text = element_text(size=CEX, color="black"),
    legend.key.size = unit(c(0.5),c("line")),
  ) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

  xlab(bquote(Delta ~ .(strsplit(column, "_")[[1]][1]) ~ "[-]")) +
  ylab("Count [-]")

  ggsave(plot_name2, dpi = 600, units = "cm", width = 16, height = 7)
}
