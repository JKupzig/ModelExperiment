# Figure 3 a) + b)

rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(grid)
library(dplyr)
library(caret)

source("./src/helper/comparison.r")

source("./src/helper/read_data.r")

is_100d <- TRUE
MIN_QUAL <- 0.0 #0.2
MAX_QUAL <- NULL #0.4
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)
metric <- "r_val"
suffix <- "100d"

ROOT_100 <- "./data/evaluation/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_100_DISCHARGE <- "./data/evaluation/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"
models <- seq(8,19,)

if (!is_100d){
  ROOT_100 <- "./data/evaluation/cal_result_benchmarks_model_m%i.txt"
  ROOT_100_DISCHARGE <- "./data/evaluation/cal_result_discharges_model_m%i.txt"
  suffix <- "10d"
}

attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)

TARGET <- "./plots/snow_insights_%s_%s_%f.png"

cal_results <- read.table(sprintf(ROOT_100, models[1]), sep="\t", header=T)
cal_results <- data.frame(station=cal_results[,names(cal_results) %in% c("station")])


for (model in models){
  model_result <- read.table(sprintf(ROOT_100, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i_100", model)]] <- model_result[[metric]]
  if (metric %in% c("b_val", "a_val")){
    cal_results[[sprintf("model_m%i_100", model)]] = 1 - abs(1-cal_results[[sprintf("model_m%i_100", model)]])
  }
}

cal_results <- cal_results[cal_results$station %in% behavioural_basins$behavioural,]



data_1_ <- compare_models(cal_results, "model_m8_100", "model_m9_100", "lakes + static (100d)")
data_2_ <- compare_models(cal_results, "model_m11_100", "model_m10_100", "lakes + variable (100d)")
data_3_ <- compare_models(cal_results, "model_m12_100", "model_m13_100", "hanasaki + static (100d)")
data_4_ <- compare_models(cal_results, "model_m14_100", "model_m15_100", " hanasaki + variable (100d)")
data_5_ <- compare_models(cal_results, "model_m16_100", "model_m17_100", "schneider + static (100d)")
data_6_ <- compare_models(cal_results, "model_m18_100", "model_m19_100", "schneider + variable (100d)")

THRESHOLDS <- c(0.05, 0.01, -0.01, -0.05)
LABELS <- c("significantly better", "moderately better",
            "no change", "moderately worse", "significantly worse")
labeled_data <- data_1_ %>%
  mutate(group=
           ifelse(value >= THRESHOLDS[1], LABELS[1],
                  ifelse(value < THRESHOLDS[1] & value > THRESHOLDS[2], LABELS[2],
                         ifelse(value <= THRESHOLDS[2] & value >= THRESHOLDS[3], LABELS[3],
                                ifelse(value < THRESHOLDS[3] & value > THRESHOLDS[4], LABELS[4],
                                       LABELS[5]
                                ))))
  ) %>%
  mutate(group = factor(group, levels = rev(LABELS)))

attributes$basin_id <- paste0("X", attributes$grdc_ids)
labeled_data <- merge(labeled_data, attributes, by="basin_id", all.x=T)

labeled_data$basin_id[labeled_data$group == "moderately worse"]

find_hull <- function(df) df[chull(df$mean_precipitation_as_snow, df$localWetlands), ]
hulls <- plyr::ddply(labeled_data, "group", find_hull)

colors_to_use <- setNames(hcl.colors(5, "Hawaii"), rev(LABELS))

labeled_data %>%
  ggplot(., aes(x=mean_precipitation_as_snow*100, y=localWetlands, col=group)) +
  xlab("Precipitation as snow (%)") +
  ylab("Fraction of smaller wetlands (%)") +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.4, aes(fill=group)) +
  scale_fill_manual(name="",values=colors_to_use) +
  scale_colour_manual(name="",values=colors_to_use) +
  ylim(0,70) +
  xlim(0,70) +
  theme_classic()

ggsave(sprintf(TARGET, suffix, metric, MIN_QUAL), units="cm", width=16, height=12, dpi=300)

boxplot(list("worse"=labeled_data$globalLakes[labeled_data$group == "moderately worse"],
        "better"=labeled_data$globalLakes[labeled_data$group == "moderately better" |
                                      labeled_data$group == "significantly better"]))

### Plots for b)

interesting_stations <- labeled_data$grdc_ids[labeled_data$group != "no change"]

m8_model_result <- read.table(sprintf(ROOT_100_DISCHARGE, 8), sep="\t", header=T)
m9_model_result <- read.table(sprintf(ROOT_100_DISCHARGE, 9), sep="\t", header=T)
m8_model_result <- m8_model_result[,names(m8_model_result) %in% c("date",paste0("X", interesting_stations)) ]
m9_model_result <- m9_model_result[,names(m9_model_result) %in% c("date", paste0("X", interesting_stations)) ]

mean_no_snow <- list()
mean_snow <- list()
snow_vs_no_snow <- list()
mean_obs = list()
for (basin in interesting_stations){

  #labels <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
  #interesting_months <-c(5:12, 1:4)

  labels <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
  interesting_months <-c(11:12, 1:4)

  start <- min(m8_model_result$date)
  end <- max(m8_model_result$date)
  basin_name <- paste0("X", basin)
  column_names <- c(basin_name, "date")

  no_snow_discharge <- m8_model_result[names(m8_model_result) %in% column_names,]
  snow_discharge <- m9_model_result[names(m8_model_result) %in% column_names,]

  observed <- WaterGAPLite::Q.read_grdc(basin,
                                        NULL,
                                        "na",
                                        start=as.Date(start),
                                        end=as.Date(end),
                                        use_folder=ROOT_GRDC)

  mean_discharge_obs <- observed %>%
    mutate(daily_code = format(Date, "%m")) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(Value, na.rm=T)) %>%
    filter(daily_code != "02-29")
  mean_obs[[basin]] <- mean_discharge_obs

  mean_discharge_no_snow <- no_snow_discharge %>%
    mutate(daily_code = substr(date, 6, 7)) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(.data[[basin_name]]))
  mean_no_snow[[basin]] <- mean_discharge_no_snow

  mean_discharge_snow <- snow_discharge %>%
    mutate(daily_code = substr(date, 6, 7)) %>%
    group_by(daily_code) %>%
    summarize(mean_basin = median(.data[[basin_name]]))
  mean_snow[[basin]] <- mean_discharge_snow


  doy_no_snow <- mean_discharge_no_snow[interesting_months,]$mean_basin
  doy_snow <- mean_discharge_snow[interesting_months,]$mean_basin
  doy_obs <- mean_discharge_obs[interesting_months,]$mean_basin

  min_y <- min(doy_snow, doy_obs, doy_no_snow)*0.95
  max_y <- max(doy_snow, doy_obs, doy_no_snow)*1.05

  ylabel <- expression(paste("Long-term monthly discharge (m"^"3", "/s)", sep=""))

  png(sprintf("./plots/doy_snow/%s_%s.png", basin, suffix), res=300, units="cm", width=18, height=12)
  plot(1:length(labels), doy_obs, type="l",
       xlab="Month", ylab=ylabel, xaxt = "n",
       ylim=c(min_y, max_y))
  lines(1:length(labels), doy_no_snow, col="firebrick")
  lines(1:length(labels), doy_snow, col="cornflowerblue")
  legend("bottomleft", legend=c("observed", "no snow-on-wetlands", "snow-on-wetlands"),
         lty=1, col=c("black", "firebrick", "cornflowerblue"))
  axis(1, at = 1:length(labels),
       labels = labels)
  dev.off()
}

labeled_data$grdc_ids[labeled_data$group == "significantly better"]
