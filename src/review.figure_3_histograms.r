# Figure 3 and Appendix
rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")
source("./src/helper/comparison.r")


PLOT_PATTERN <- "./plots/review/figure3_histogram_%s.png"
PLOT_PATTERN2 <- "./plots/review/additional_info_histogram_notaffected_%s.png"
CEX = 7
behavioural_set <- read_kge_and_define_good_basins(min_kge = 0.4, max_kge = NULL)
ROOT_ATTRIBUTES <- "./data/basin_attributes.txt"
attributes <- read.table(ROOT_ATTRIBUTES, sep = "\t", header = TRUE)


snow <- list(c(12,13), c(14, 15), c(16, 17), c(18, 19), c(8,9), c(11, 10))
reservoir1 <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))
reservoir <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))
river <- list(c(12, 14), c(13, 15), c(16, 18), c(17, 19), c(8, 11), c(9, 10))


columns <- c("KGE_val", "NSE_val", "KGE_mod_val", "logNSE_val")

names <- c(rep("variable flow velocity", length(river)),
           rep("reservoir algorithm (V1)", length(reservoir1)),
           rep("reservoir algorithm (V2)", length(reservoir)),
           rep("snow on wetlands", length(snow)))
all_to_check <- c(river, reservoir1, reservoir, snow)
intervals <- c(-100, -0.2, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.2, 100)
interval_labels <- c("<-0.2", "-0.2 to -0.1", "-0.1 to -0.05", "-0.05 to -0.01",
                    "-0.01 to 0.01", "0.01 to 0.05", "0.05 to 0.1", "0.1 to 0.2", ">0.2")


diff_for_analyse <- list()
for (column in columns){
  print(column)

  plot_name <- sprintf(PLOT_PATTERN, column)
  plot_name2 <- sprintf(PLOT_PATTERN2, column)

  result <- list()
  diff_for_analyse[[column]] <- list()
  for (set_name in c("behavioural", "non-behavioural")){

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

    diff_for_analyse[[column]][[set_name]] <- list()
    for (entry in 1:length(names)){
      name <- names[entry]

      print(name)

      if (name == "snow on wetlands"){
        sensitive_basins <- get_sensitive_basins("snow")
      } else if (name == "reservoir algorithm (V1)"){
        sensitive_basins <- get_sensitive_basins("reservoir")
      } else if (name == "reservoir algorithm (V2)") {
        sensitive_basins <- get_sensitive_basins("non-irrig")
      } else {
        sensitive_basins <- get_sensitive_basins("river")
      }
      sensitive_data <- info[info$station %in% sensitive_basins,]

      ref_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][1])
      comp_model <- sprintf("model_m%i_100", all_to_check[entry][[1]][2])
      diff <- sensitive_data[[comp_model]]- sensitive_data[[ref_model]]
      cutted_diff <- cut(diff, intervals,include.lowest = TRUE)
      hist_values <- table(cutted_diff)

      diff_for_analyse[[column]][[set_name]][[name]] <- c(diff_for_analyse[[column]][[set_name]][[name]], diff)

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
    levels = c("non-behavioural", "behavioural"),
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
  print(name)

  if (name == "snow on wetlands"){
    sensitive_basins <- get_sensitive_basins("snow")
  } else if (name == "reservoir algorithm (V1)"){
    sensitive_basins <- get_sensitive_basins("reservoir")
  } else if (name == "reservoir algorithm (V2)") {
    sensitive_basins <- get_sensitive_basins("non-irrig")
  } else {
    sensitive_basins <- get_sensitive_basins("river")
  }
  sensitive_data <- info[!info$station %in% sensitive_basins,]


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

################################################################################
# getting some numbers for the rivers
################################################################################
col <- "KGE_val" # NSE_val
n_behav <- 104
behav <- 122
river <- data.frame(
  M1 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*0+1):(behav*1)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*0+1):(n_behav*1)]),
  M2 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*1+1):(behav*2)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*1+1):(n_behav*2)]),
  M3 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*2+1):(behav*3)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*2+1):(n_behav*3)]),
  M4 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*3+1):(behav*4)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*3+1):(n_behav*4)]),
  M5 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*4+1):(behav*5)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*4+1):(n_behav*5)]),
  M6 = c(diff_for_analyse[[col]][["behavioural"]][["variable flow velocity"]][(behav*5+1):(behav*6)],
         diff_for_analyse[[col]][["non-behavioural"]][["variable flow velocity"]][(n_behav*5+1):(n_behav*6)])
)

river_aggregated <- apply(river, 1, median)
river_aggregated_behav <- apply(river[1:behav,], 1, median)
river_aggregated_nbehav <- apply(river[(behav+1):(n_behav+behav),], 1, median)

round(sum(river_aggregated_behav < -0.1) / length(river_aggregated_behav) * 100,0)
round(sum(river_aggregated_behav > 0.1) / length(river_aggregated_behav) * 100,0)

round(sum(river_aggregated_nbehav < -0.1) / length(river_aggregated_nbehav) * 100,0)
round(sum(river_aggregated_nbehav > 0.1) / length(river_aggregated_nbehav) * 100,0)

round(sum(river_aggregated < -0.01) / length(river_aggregated) * 100,0)
round(sum(river_aggregated > 0.01) / length(river_aggregated) * 100,0)


monthly_nse <- readRDS("./data/SI_original.rds")
monthly_nse <- monthly_nse[c(1,4), 11, dimnames(monthly_nse)[[3]] %in% get_sensitive_basins("river")]
round(sum(monthly_nse[2,] - monthly_nse[1,] > 0.0) / ncol(monthly_nse) * 100,0)

################################################################################
# getting some numbers for the reservoirs (V1)
################################################################################

n_total <- length(get_sensitive_basins("reservoir"))
behav <- sum(read_kge_and_define_good_basins()$behavioural %in% get_sensitive_basins("reservoir"))
n_behav <- n_total - behav

col <- "NSE_val"
res1 <- data.frame(
  M1 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V1)"]][(behav*0+1):(behav*1)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V1)"]][(n_behav*0+1):(n_behav*1)]),
  M2 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V1)"]][(behav*1+1):(behav*2)],
          diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V1)"]][(n_behav*1+1):(n_behav*2)]),
  M3 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V1)"]][(behav*2+1):(behav*3)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V1)"]][(n_behav*2+1):(n_behav*3)]),
  M4 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V1)"]][(behav*3+1):(behav*4)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V1)"]][(n_behav*3+1):(n_behav*4)])
)

res1_aggregated <- apply(res1, 1, median)
res1_aggregated_behav <- apply(res1[1:behav,], 1, median)
res1_aggregated_nbehav <- apply(res1[(behav+1):(n_behav+behav),], 1, median)

round(sum(res1_aggregated_behav < -0.1) / length(res1_aggregated_behav) * 100,0)
round(sum(res1_aggregated_behav > 0.1) / length(res1_aggregated_behav) * 100,0)

round(sum(res1_aggregated_nbehav < -0.1) / length(res1_aggregated_nbehav) * 100,0)
round(sum(res1_aggregated_nbehav > 0.1) / length(res1_aggregated_nbehav) * 100,0)

round(sum(res1_aggregated < -0.01) / length(res1_aggregated) * 100,0)
round(sum(res1_aggregated > 0.01) / length(res1_aggregated) * 100,0)

attributes <- read.csv("./data/basin_attributes.txt", sep="\t")
info <- read_benchmarks_all(column = column)
info$delta_r1 <- delta <- info$model_m8_100 - info$model_m12_100
info$delta_r2 <- delta <- info$model_m12_100 - info$model_m16_100
info <- info[info$station %in% get_sensitive_basins("reservoir"),]

info <- merge(attributes, info, by.x = "grdc_ids", by.y = "station", how="right")
not_sensitive <- info[abs(info$delta_r1) < 0.01, names(info) %in% c("basin_size", "reservoir_area", "mean_precipitation_as_snow")]
sensitive <- info[abs(info$delta_r1) >= 0.01, names(info) %in% c("basin_size", "reservoir_area", "mean_precipitation_as_snow")]
boxplot(list(sensitive=sensitive$reservoir_area, not_sensitive=not_sensitive$reservoir_area))
boxplot(list(sensitive=sensitive$reservoir_area/sensitive$basin_size, not_sensitive=not_sensitive$reservoir_area/not_sensitive$basin_size))
mean(not_sensitive$reservoir_area)
mean(sensitive$reservoir_area)
mean(not_sensitive$reservoir_area/not_sensitive$basin_size)
mean(sensitive$reservoir_area/sensitive$basin_size)

info <- info[info$grdc_ids %in% get_sensitive_basins("non-irrig"),]
not_sensitive <- info[abs(info$delta_r2) < 0.01, names(info) %in% c("basin_size", "reservoir_area", "mean_precipitation_as_snow")]
sensitive <- info[abs(info$delta_r2) >= 0.01, names(info) %in% c("basin_size", "reservoir_area", "mean_precipitation_as_snow")]
mean(not_sensitive$reservoir_area)
mean(sensitive$reservoir_area)
mean(not_sensitive$reservoir_area/not_sensitive$basin_size)
mean(sensitive$reservoir_area/sensitive$basin_size)


################################################################################
# getting some numbers for the reservoirs (V2)
################################################################################

n_total <- length(get_sensitive_basins("non-irrig"))
behav <- sum(read_kge_and_define_good_basins()$behavioural %in% get_sensitive_basins("non-irrig"))
n_behav <- n_total - behav

col <- "KGE_val"
res2 <- data.frame(
  M1 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V2)"]][(behav*0+1):(behav*1)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V2)"]][(n_behav*0+1):(n_behav*1)]),
  M2 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V2)"]][(behav*1+1):(behav*2)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V2)"]][(n_behav*1+1):(n_behav*2)]),
  M3 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V2)"]][(behav*2+1):(behav*3)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V2)"]][(n_behav*2+1):(n_behav*3)]),
  M4 = c(diff_for_analyse[[col]][["behavioural"]][["reservoir algorithm (V2)"]][(behav*3+1):(behav*4)],
         diff_for_analyse[[col]][["non-behavioural"]][["reservoir algorithm (V2)"]][(n_behav*3+1):(n_behav*4)])
)

res2_aggregated <- apply(res2, 1, median)
res2_aggregated_behav <- apply(res2[1:behav,], 1, median)
res2_aggregated_nbehav <- apply(res2[(behav+1):(n_behav+behav),], 1, median)

round(sum(res2_aggregated_behav < -0.1) / length(res2_aggregated_behav) * 100,0)
round(sum(res2_aggregated_behav > 0.1) / length(res2_aggregated_behav) * 100,0)

round(sum(res2_aggregated_nbehav < -0.1) / length(res2_aggregated_nbehav) * 100,0)
round(sum(res2_aggregated_nbehav > 0.1) / length(res2_aggregated_nbehav) * 100,0)

round(sum(res2_aggregated < -0.01) / length(res2_aggregated) * 100,0)
round(sum(res2_aggregated > 0.01) / length(res2_aggregated) * 100,0)


################################################################################
# getting some numbers for the snow on wetlands process
################################################################################
col <- "KGE_val" # NSE_val

n_total <- length(get_sensitive_basins("snow"))
behav <- sum(read_kge_and_define_good_basins()$behavioural %in% get_sensitive_basins("snow"))
n_behav <- n_total - behav

snow_set <- data.frame(
  M1 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*0+1):(behav*1)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*0+1):(n_behav*1)]),
  M2 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*1+1):(behav*2)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*1+1):(n_behav*2)]),
  M3 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*2+1):(behav*3)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*2+1):(n_behav*3)]),
  M4 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*3+1):(behav*4)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*3+1):(n_behav*4)]),
  M5 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*4+1):(behav*5)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*4+1):(n_behav*5)]),
  M6 = c(diff_for_analyse[[col]][["behavioural"]][["snow on wetlands"]][(behav*5+1):(behav*6)],
         diff_for_analyse[[col]][["non-behavioural"]][["snow on wetlands"]][(n_behav*5+1):(n_behav*6)])
)

snow_set_aggregated <- apply(snow_set, 1, median)
snow_set_aggregated_behav <- apply(snow_set[1:behav,], 1, median)
snow_set_aggregated_nbehav <- apply(snow_set[(behav+1):(n_behav+behav),], 1, median)

round(sum(snow_set_aggregated_behav < -0.1) / length(snow_set_aggregated_behav) * 100,0)
round(sum(snow_set_aggregated_behav > 0.1) / length(snow_set_aggregated_behav) * 100,0)

round(sum(snow_set_aggregated_nbehav < -0.1) / length(snow_set_aggregated_nbehav) * 100,0)
round(sum(snow_set_aggregated_nbehav > 0.1) / length(snow_set_aggregated_nbehav) * 100,0)

round(sum(snow_set_aggregated < -0.01) / length(snow_set_aggregated) * 100,0)
round(sum(snow_set_aggregated > 0.01) / length(snow_set_aggregated) * 100,0)

# most affected basins
info <- read_benchmarks_all(column = "KGE_val")
m1 <- info[info$station %in% get_sensitive_basins("snow"), c(1,2,3)]
most_affected_basin <- which(abs(m1[,3] - m1[,2]) == max(abs(m1[,3] - m1[,2])))
m1[most_affected_basin,]
attributes[attributes$grdc_ids == m1[most_affected_basin,1],]

# unaffected basins
info <- read_benchmarks_all(column = "KGE_val")
m1 <- info[info$station %in% get_sensitive_basins("snow"), c(1,2,3)]
unaffected_basin <- which(abs(m1[,3] - m1[,2]) < .01)
affected_basin <- which(abs(m1[,3] - m1[,2]) >= .01)

data_unaffected_basins <- attributes[attributes$grdc_ids %in% m1[unaffected_basin,1],]
data_affected_basins <- attributes[attributes$grdc_ids %in% m1[affected_basin,1],]

# less snow impact
boxplot(list("unaffected"=data_unaffected_basins$mean_precipitation_as_snow,
             "affected" = data_affected_basins$mean_precipitation_as_snow))
# less local wetlands
boxplot(list("unaffected"=data_unaffected_basins$localWetlands,
             "affected" = data_affected_basins$localWetlands))
