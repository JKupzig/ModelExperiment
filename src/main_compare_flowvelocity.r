# creates 2 plots (1 boxplot and 1 table) comparing (using delta KGe)
# all model variants and the resulting KGE when basins > 0.2

rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
################################################################################
# calibrated version
################################################################################
# ROOT <- "./data/KGE_results/cal_result_appendix_variableflow.rds"
# sim_results <- readRDS(ROOT)
#
# min_quality <- 0.2
# station_sufficient_quality <- (val_static > min_quality | val_variable > min_quality)
#
# val_static <- sim_results[2, 1, 1,]
# val_variable <- sim_results[2, 1, 2,]
#
# boxplot(list("static"=val_static[station_sufficient_quality],
#              "variable"=val_variable[station_sufficient_quality]))
################################################################################
# MC version
################################################################################
ROOT <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
ROOT_100 <- "./data/KGE_results/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
models <- seq(8,19,)


cal_results <- read.table(sprintf(ROOT, models[1]), sep="\t", header=T)
cal_results <- data.frame(station=cal_results[,names(cal_results) %in% c("station")])
for (model in models){
  model_result <- read.table(sprintf(ROOT, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

for (model in models){
  model_result <- read.table(sprintf(ROOT_100, model), sep="\t", header=T)
  cal_results[[sprintf("model_m%i_100", model)]] <- model_result$kge_val
}


compare_models <- function(cal_results,
                           reference_column,
                           to_compare_column,
                           label_to_use,
                           min_quality = 0.2){

  station_sufficient_quality <- cal_results$station[cal_results[reference_column] > min_quality |
                                                      cal_results[to_compare_column] > min_quality]

  selected_stations <- cal_results$station %in% c(station_sufficient_quality)
  cal_results_intersting <- cal_results[selected_stations,]
  cal_results_others <- cal_results[!selected_stations,]

  delta <- (cal_results_intersting[[to_compare_column]]  -
            cal_results_intersting[[reference_column]])

  data <- data.frame("value"= delta,
                     "label"= label_to_use,
                     "basin_id"=cal_results_intersting$station)

  return(data)
}


data_1 <- compare_models(cal_results, "model_m11", "model_m8", "lakes (10d)")
data_2 <- compare_models(cal_results, "model_m10", "model_m9", "lakes + snow (10d)")
data_3 <- compare_models(cal_results, "model_m14", "model_m12", "hanasaki (10d)")
data_4 <- compare_models(cal_results, "model_m15", "model_m13", "hanasaki + snow (10d)")
data_5 <- compare_models(cal_results, "model_m18", "model_m16", "schneider (10d)")
data_6 <- compare_models(cal_results, "model_m19", "model_m17", "schneider + snow (10d)")

data_1_ <- compare_models(cal_results, "model_m11_100", "model_m8_100", "lakes (100d)")
data_2_ <- compare_models(cal_results, "model_m10_100", "model_m9_100", "lakes + snow (100d)")
data_3_ <- compare_models(cal_results, "model_m14_100", "model_m12_100", "hanasaki (100d)")
data_4_ <- compare_models(cal_results, "model_m15_100", "model_m13_100", "hanasaki + snow (100d)")
data_5_ <- compare_models(cal_results, "model_m18_100", "model_m16_100", "schneider (100d)")
data_6_ <- compare_models(cal_results, "model_m19_100", "model_m17_100", "schneider + snow (100d)")

all_together <- rbind(data_1, data_2, data_3, data_4, data_5, data_6,
                      data_1_, data_2_, data_3_, data_4_, data_5_, data_6_)
all_together %>%
  group_by(label) %>%
  mutate(count = n()) %>%
  filter(abs(value) > 0.01) %>%
  mutate(count_with_change = n()) %>%
  ggplot(., aes(x=label, y=value)) +
    geom_boxplot(fill="grey") +
    geom_violin(alpha=0.2) +
    geom_hline(yintercept=0, col="firebrick", linetype="dashed", linewidth=1.2) +
    scale_color_manual(values=hcl.colors(length(unique(all_together$label)), "Berlin")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_label(aes(label= count , y = 0.4), # mean + 0.1 <- change this to move label up and down
               size = 4, position = position_dodge(width = 0.75)) +
  geom_label(aes(label= count_with_change, y = 0.35), # mean + 0.1 <- change this to move label up and down
             size = 4, position = position_dodge(width = 0.75)) +
  labs(x="",
       y="\u0394KGE")

ggsave("./plots/MC_flow_velocity.png", units="cm", width=16, height=12, dpi=300)

######################

table <- all_together %>%
  mutate(group =
    ifelse(value >= 0.1, "better",
           ifelse(value < 0.1 & value > 0.01, "moderately better",
           ifelse(value <= 0.01 & value >= -0.01, "no change",
           ifelse(value < -0.01 & value > -0.1, "moderately worse", "worse"
           ))))
  ) %>%
  mutate(group = factor(group, levels = c("worse", "moderately worse",
                                          "no change",
                                          "moderately better", "better"))) %>%
  tidytable::group_by(group, label) %>%
  tidytable::count(group) %>%
  tidytable::group_by(group) %>%
  summarise(min = min(n),
            Q25 = quantile(n, 0.25),
            median = median(n),
            mean = median(n),
            Q75 = quantile(n, 0.75),
            max = max(n))


dev.off()
png("./plots/MC_flow_velocity_table.png", units="cm", width=16, height=12, res=300)
table_to_save <- tableGrob(table)
grid.draw(table_to_save)
dev.off()

################################################################################
# lookint at example: data_1
data <- readRDS("./data/SI_original.rds")

sensitive_stations <- data_1$basin_id[abs(data_1$value) > 0.01]
si_id <- 1:16
interesting_stations <- dimnames(data)[[3]] %in% unique(sensitive_stations)
cum_values <- data[c(1,4), si_id, interesting_stations]
relative_differences <- NULL
for (slice in 1:dim(cum_values)[3]){
  magnitude <- (cum_values[1,1:3,slice] - cum_values[2,1:3,slice]) /
                cum_values[2,1:3,slice] * 100
  rest <- cum_values[1,4:16,slice] - cum_values[2,4:16,slice]

  relative_differences <- rbind(relative_differences, c(magnitude, rest))
}

relative_differences <- as.data.frame(relative_differences)
relative_differences$basins <- dimnames(data)[[3]][interesting_stations]
differences_long <- relative_differences %>%
  pivot_longer(., cols=-basins) %>%
  mutate(plot_type = ifelse(name %in% c("mgn_l", "mgn_a", "mgn_h"), "magnitude (%)",
                     ifelse(name %in% c("frq_l", "frq_h"), "frequency (d)",
                     ifelse(name %in% c("dur_l", "dur_h"), "duration (d)",
                     ifelse(name %in% c("pearson", "sd", "mean"), "KGE components (-)",
                     ifelse(name %in% c("monthly_pearson", "monthly_sd", "monthly_mean"), "monthly KGE components (-)", "overall quality (-)"
                      )))))) %>%
  mutate(coloring = ifelse(name %in% c("mgn_l", "frq_l", "dur_l"), "lowflow",
                    ifelse(name %in% c("mgn_h", "frq_h", "dur_h"), "highflow",
                    ifelse(name %in% c("pearson", "monthly_pearson"), "pearson",
                    ifelse(name %in% c("sd", "monthly_sd"), "variation",
                    ifelse(name %in% c("mean", "monthly_mean"), "mean", name))))))


differences_long %>%
  dplyr::filter(! name %in% c("mgn_a", "monthly_nse")) %>%
  ggplot(., aes(x=basins, y = value, group=name, col=coloring)) +
    geom_point() +
    theme_classic() +
    facet_wrap(.~plot_type, scales="free_y") +
  scale_color_manual("type", values=hcl.colors(8, "Berlin")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("./plots/MC_flow_velocity_SI.png", units="cm", width=16, height=12, dpi=300)

################################################################################
# learning about the attributes
set.seed(12)

library(dplyr)
library(caret)
library(tidytable)

ordinals <- seq(1, length(unique(all_together$label)), 1)
names(ordinals) <- unique(all_together$label)

labeled_data <- all_together %>%
  mutate(group=
           ifelse(value >= 0.1, "better",
                  ifelse(value < 0.1 & value > 0.01, "moderately better",
                         ifelse(value <= 0.01 & value >= -0.01, "no_change",
                                ifelse(value < -0.01 & value > -0.1, "moderately worse", "worse"
                                ))))
  ) %>%
  mutate(group = factor(group, levels = c("worse", "moderately worse",
                                          "no_change",
                                          "moderately better", "better"))) %>%
  mutate(label_as_ordinal = as.numeric(ordinals[label]))

attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)
attributes$basin_id <- paste0("X", attributes$grdc_ids)
labeled_data <- merge(labeled_data, attributes, by="basin_id", all.x=T)


labeled_data$group[labeled_data$group == "moderately worse"] <- "worse"
labeled_data$group[labeled_data$group == "moderately better"] <- "better"
labeled_data$group <- factor(labeled_data$group, levels = c(
  "worse", "no_change", "better"))

#labeled_data[labeled_data == 0] <- 0.001
reduced_labeled_data <- distinct(labeled_data[sample(1:nrow(labeled_data)),],
                                 basin_id, .keep_all = T)
data_split <- rsample::initial_split(reduced_labeled_data, prop = 0.5)
train_data <- rsample::training(data_split)
test_data <- rsample::testing(data_split)
scaler <- caret::preProcess(train_data, method=c("center", "scale"))
scaled_train_data <- predict(scaler, train_data)
scaled_test_data <- predict(scaler, test_data)

table(train_data$group)
table(test_data$group)

ctrl <- trainControl(method = "repeatedcv",
                     number = 30,
                     repeats = 3,
                     classProbs = TRUE,
                     sampling="up",
)



elements_to_check <- c("open_water",
                       "basin_size",
                       "localLakes",
                       "aridity",
                       "mean_temperature",
                       "mean_precipitation_as_snow",
                       "sum_precipitation",
                       "batjes",
                       "localWetlands",
                       "globalLakes",
                       "reservoir_area",
                       "globalWaterbodies",
                       "label_as_ordinal")

variance <- list()
bias <- list()
label <- c()
model_in <- c("water_stress")
for (element in elements_to_check){
  model_in <- c(model_in, element)
  label <- c(label, paste(model_in, collapse="+"))
  print(model_in)
  ids_to_use <- which(names(train_data) %in% model_in)


  for (caret_model in c("multinom", "rf", "nnet")){
    print(sprintf("labels: %s - model: %s",label, caret_model))
    bias_in_model <- c()
    #for (i in 1:1){
      fit <- caret::train(y=scaled_train_data$group,
                          x= as.data.frame(scaled_train_data)[,ids_to_use],
                          method = caret_model,
                          verbose = FALSE,
                          trace=FALSE,
                          trControl = ctrl,
                          metric = "Accuracy")

      test_data$prediction_rf <-predict(fit, as.data.frame(scaled_test_data))
      metric <- caret::confusionMatrix(data=test_data$prediction_rf,
                                       reference=scaled_test_data$group)
      bias_in_model <- c(bias_in_model, metric$overall[1])
    #}
    if (is.null(bias[[caret_model]])){
      bias[[caret_model]] <- c(mean(bias_in_model))
      variance[[caret_model]] <- c(var(bias_in_model))
    } else {
      bias[[caret_model]] <- c(bias[[caret_model]], mean(bias_in_model))
      variance[[caret_model]] <- c(variance[[caret_model]], var(bias_in_model))
    }
  }
}

plot(1:length(elements_to_check), bias$multinom, col="firebrick", type="l", lwd=2, ylim=c(0., 0.9))
lines(1:length(elements_to_check), bias$rf, col="forestgreen", lwd=2)
lines(1:length(elements_to_check), bias$nnet, col="cornflowerblue", lwd=2)

#plot(1:length(elements_to_check), variance$multinom, col="firebrick", type="l", lwd=2, ylim=c(0.0, 0.1))
#lines(1:length(elements_to_check), variance$rf, col="forestgreen", lwd=2)
#lines(1:length(elements_to_check), variance$nnet, col="cornflowerblue", lwd=2)


ids_to_use <- which(names(train_data) %in% c("open_water",
                                             "basin_size",
                                             "label_as_ordinal"))

# to understand importance better
fit <- caret::train(y=scaled_train_data$group,
                    x=as.data.frame(scaled_train_data)[,ids_to_use],
                    method = "rf",
                    verbose = T,
                    trControl = ctrl,
                    metric = "Accuracy",
                    importance=T)


caret::varImp(fit)
test_data$prediction_rf <-predict(fit, as.data.frame(scaled_test_data))
metric <- caret::confusionMatrix(data=test_data$prediction_rf,
                                 reference=scaled_test_data$group)
metric$table
metric$overall

# to understand influence better
descision_tree <- rpart(y=as.data.frame(labeled_data)[,4],
                        x=as.data.frame(labeled_data)[, ids_to_use],
                        method = "class")
rpart.plot(descision_tree)

# to visualize most important attr
find_hull <- function(df) df[chull(df$open_water, df$basin_size), ]
hulls <- plyr::ddply(labeled_data, "group", find_hull)

labeled_data %>%
  ggplot(., aes(x=open_water, y=basin_size, col=group)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.4, aes(fill=group)) +
  scale_fill_manual(values=hcl.colors(5, "Hawaii")) +
  scale_colour_manual(values=hcl.colors(5, "Hawaii")) +
  theme_classic()
