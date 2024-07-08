rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)


ROOT <- "./data/KGE_results/cal_result_benchmarks_model_m%i.txt"
ROOT_100 <- "./data/KGE_results/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
models <- seq(8,19,1)


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


compare_models_reservoirs <- function(cal_results,
                                reference_column,
                                to_compare_column_1,
                                to_compare_column_2,
                                label_to_use,
                                min_quality = 0.2){


  station_sufficient_quality <- cal_results$station[
      cal_results[reference_column] > min_quality |
      cal_results[to_compare_column_1] > min_quality |
      cal_results[to_compare_column_2] > min_quality]

  selected_stations <- cal_results$station %in% station_sufficient_quality
  cal_results_intersting <- cal_results[selected_stations,]

  delta_1 <- cal_results_intersting[[to_compare_column_1]]  -
    cal_results_intersting[[reference_column]]

  delta_2 <- cal_results_intersting[[to_compare_column_2]]  -
    cal_results_intersting[[reference_column]]

  data_1 <- data.frame("value"= delta_1,
                     "label"= paste0(label_to_use, " schneider"),
                     "basin_id"= cal_results_intersting$station)

  data_2 <- data.frame("value"= delta_2,
                     "label"= paste0(label_to_use, " hansaki"),
                     "basin_id"= cal_results_intersting$station)

  return(rbind(data_1, data_2))
}


# water use
#reference: no snow -> positive values --> increasing perfomance!
data_1 <- compare_models_reservoirs(cal_results, "model_m8",
                                    "model_m16", "model_m12",
                                    "static (10d)")
data_2 <- compare_models_reservoirs(cal_results, "model_m9",
                                    "model_m17", "model_m13",
                                    "static + snow (10d)")
data_3 <- compare_models_reservoirs(cal_results, "model_m10",
                                    "model_m19", "model_m15",
                                    "variable (10d)")
data_4 <- compare_models_reservoirs(cal_results, "model_m11",
                                    "model_m18", "model_m14",
                                    "variable + snow (10d)")

data_1_ <- compare_models_reservoirs(cal_results, "model_m8_100",
                                    "model_m16_100", "model_m12_100",
                                    "static (100d)")
data_2_ <- compare_models_reservoirs(cal_results, "model_m9_100",
                                    "model_m17_100", "model_m13_100",
                                    "static + snow (100d)")
data_3_ <- compare_models_reservoirs(cal_results, "model_m10_100",
                                    "model_m19_100", "model_m15_100",
                                    "variable (100d)")
data_4_ <- compare_models_reservoirs(cal_results, "model_m11_100",
                                    "model_m18_100", "model_m14_100",
                                    "variable + snow (100d)")

all_together <- rbind(data_1, data_2, data_3, data_4,
                      data_1_, data_2_, data_3_, data_4_)


all_together %>%
  group_by(label) %>%
  mutate(count = n()) %>%
  mutate(mean = mean(value)) %>%
  filter(abs(value) > 0.01) %>%
  mutate(count_with_change = n()) %>%
  ggplot(., aes(x=label, y=value)) +
  geom_boxplot(fill="grey") +
  geom_violin(alpha=0.2) +
  geom_hline(yintercept=0, col="firebrick", linetype="dashed", linewidth=1.2) +
  scale_color_manual(values=hcl.colors(length(unique(all_together$label)), "Berlin")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_label(aes(label= count , y = 1.1), # mean + 0.1 <- change this to move label up and down
             size = 4, position = position_dodge(width = 0.75)) +
  geom_label(aes(label= count_with_change , y = 1), # mean + 0.1 <- change this to move label up and down
             size = 4, position = position_dodge(width = 0.75)) +
  labs(x="",
       y="\u0394KGE") +
  coord_cartesian(ylim=c(-0.4,1.2))

ggsave("./plots/MC_reservoirs.png", units="cm", width=16, height=12, dpi=300)

table <- all_together %>%
  mutate(group=
           ifelse(value >= 0.1, "better",
                  ifelse(value < 0.1 & value > 0.01, "moderately better",
                         ifelse(value <= 0.01 & value >= -0.01, "no change",
                                ifelse(value < -0.01 & value > -0.1, "moderately worse", "worse"
                                ))))
  ) %>%
  mutate(group = factor(group, levels = c("worse", "moderately worse",
                                          "no change",
                                          "moderately better", "better"))) %>%
  group_by(label) %>%
  count(group) %>%
  group_by(group) %>%
  summarise(min = min(n),
            Q25 = quantile(n, 0.25),
            median = median(n),
            mean = median(n),
            Q75 = quantile(n, 0.75),
            max = max(n))

dev.off()
png("./plots/MC_reservoirs_table.png", units="cm", width=16, height=12, res=300)
table_to_save <- tableGrob(table)
grid.draw(table_to_save)
dev.off()

#######################################
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
  scale_color_manual("type", values=hcl.colors(8, "viridis")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("./plots/MC_reservoirs_SI.png", units="cm", width=16, height=12, dpi=300)

#########################
#########################
# learning about the attributes
set.seed(12)

library(dplyr)
library(caret)


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
                                          "moderately better", "better")))

attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)
attributes$basin_id <- paste0("X", attributes$grdc_ids)
labeled_data <- merge(labeled_data, attributes, by="basin_id", all.x=T)

find_hull <- function(df) df[chull(df$reservoir_area/df$basin_size, df$sum_precipitation), ]
hulls <- plyr::ddply(labeled_data, "group", find_hull)

labeled_data %>%
  ggplot(., aes(x=reservoir_area/basin_size, y=sum_precipitation, col=group)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.4, aes(fill=group)) +
  scale_fill_manual(values=hcl.colors(5, "Hawaii")) +
  scale_colour_manual(values=hcl.colors(5, "Hawaii")) +
  theme_classic()

labeled_data$group <- as.character(labeled_data$group)
labeled_data$group[labeled_data$group == "better"] <- "change"
labeled_data$group[labeled_data$group == "worse"] <- "change"
labeled_data$group[labeled_data$group == "moderately worse"] <- "change"
labeled_data$group[labeled_data$group == "moderately better"] <- "change"
labeled_data$group <- factor(labeled_data$group, levels = c("no_change", "change"))

#labeled_data[labeled_data == 0] <- 0.001
reduced_labeled_data <- distinct(labeled_data[sample(1:nrow(labeled_data)),],
                                 basin_id, .keep_all = T)
data_split <- rsample::initial_split(reduced_labeled_data, prop = 0.5)
train_data <- rsample::training(data_split)
test_data <- rsample::testing(data_split)

#führt zu Verschlechterung der Ergebnisse!
scaler <- caret::preProcess(train_data, method=c("center", "scale"))
scaled_train_data <- predict(scaler, train_data)
scaled_test_data <- predict(scaler, test_data)
table(scaled_train_data$group)

ctrl <- trainControl(method = "repeatedcv",
                     number = 20,
                     repeats = 3,
                     classProbs = TRUE,
                     sampling="down",
)



elements_to_check <- c("mean_precipitation_as_snow",
                       "basin_size", "aridity", "sum_precipitation",
                       "mean_temperature", "batjes", "localWetlands",
                       "water_stress", "global_waterbodies")

variance <- list()
bias <- list()
label <- c()
model_in <- c("reservoir_area")
for (element in elements_to_check){
  model_in <- c(model_in, element)
  label <- c(label, paste(model_in, collapse="+"))
  print(model_in)
  ids_to_use <- which(names(train_data) %in% model_in)

  for (caret_model in c("multinom", "rf", "nnet"))
  {
    bias_in_model <- c()
    #for (i in 1:10){
    fit <- caret::train(y=scaled_train_data$group,
                        x=as.data.frame(scaled_train_data)[,ids_to_use],
                        method = caret_model,
                        verbose = FALSE,
                        trace=FALSE,
                        trControl = ctrl,
                        metric = "Accuracy")

    test_data$prediction_rf <-predict(fit, scaled_test_data)
    metric <- caret::confusionMatrix(data=predict(fit, scaled_test_data),
                                     reference=scaled_test_data$group)
    bias_in_model <- c(bias_in_model, metric$overall[1])
    # }
    if (is.null(bias[[caret_model]])){
      bias[[caret_model]] <- c(mean(bias_in_model))
      variance[[caret_model]] <- c(var(bias_in_model))
    } else {
      bias[[caret_model]] <- c(bias[[caret_model]], mean(bias_in_model))
      variance[[caret_model]] <- c(variance[[caret_model]], var(bias_in_model))
    }
  }
}

plot(1:length(elements_to_check), bias$multinom, col="firebrick", type="l", lwd=2, ylim=c(0.2, 0.9))
lines(1:length(elements_to_check), bias$rf, col="forestgreen", lwd=2)
lines(1:length(elements_to_check), bias$nnet, col="cornflowerblue", lwd=2)

# randomforest seem to be most adequate:
fit <- caret::train(y=scaled_train_data$group,
                    x=as.data.frame(scaled_train_data)[,ids_to_use],
                    method = "rf",
                    verbose = T,
                    trControl = ctrl,
                    metric = "Accuracy",
                    importance=T)

caret::varImp(fit)
test_data$prediction_rf <-predict(fit, scaled_test_data)
metric <- caret::confusionMatrix(data=predict(fit, scaled_test_data),
                                 reference=scaled_test_data$group)
metric$table
metric$overall #about 85%


# ROOT <- "./data/cal_result_appendix_variableflow.rds"
# data <- readRDS(ROOT)
#
# ROOT_HANASKI <- "./data/cal_result_appendix_variableflow_hanasaki.rds"
# data_hanasaki <- readRDS(ROOT_HANASKI)
#
# ROOT_SIMPLE_WB <- "./data/cal_result_appendix_variableflow_simple_wb.rds"
# data_simple_wb <- readRDS(ROOT_SIMPLE_WB)
#
#
# ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
# attributes <- read.table(ATTRIBUTES, header=T, sep="\t")
# attributes$grdc_ids <- paste0("X", attributes$grdc_ids)
#
# kge_all_schneider <- NULL
# variant_id <- 1
# for (variant in dimnames(data)[[3]]){
#   kge_val <- data.frame("kge" = data[2, 1, variant_id, ],
#                         "b" = data[2, 2, variant_id, ],
#                         "a" = data[2, 3, variant_id, ],
#                         "r" = data[2, 4, variant_id, ],
#                         "variant"=variant,
#                         "basin_id" = dimnames(data)[[4]])
#   variant_id <- variant_id + 1
#
#   kge_all_schneider <- rbind(kge_all_schneider, kge_val)
# }
#
# kge_all_hanasaki <- NULL
# variant_id <- 1
# for (variant in dimnames(data_hanasaki)[[3]]){
#   kge_val <- data.frame("kge" = data_hanasaki[2, 1, variant_id, ],
#                         "b" = data_hanasaki[2, 2, variant_id, ],
#                         "a" = data_hanasaki[2, 3, variant_id, ],
#                         "r" = data_hanasaki[2, 4, variant_id, ],
#                         "variant"=variant,
#                         "basin_id" = dimnames(data_hanasaki)[[4]])
#   variant_id <- variant_id + 1
#
#   kge_all_hanasaki <- rbind(kge_all_hanasaki, kge_val)
# }
#
# kge_all_simplewb <- NULL
# variant_id <- 1
# for (variant in dimnames(data_simple_wb)[[3]]){
#   kge_val <- data.frame("kge" = data_simple_wb[2, 1, variant_id, ],
#                         "b" = data_simple_wb[2, 2, variant_id, ],
#                         "a" = data_simple_wb[2, 3, variant_id, ],
#                         "r" = data_simple_wb[2, 4, variant_id, ],
#                         "variant"=variant,
#                         "basin_id" = dimnames(data_simple_wb)[[4]])
#   variant_id <- variant_id + 1
#
#   kge_all_simplewb <- rbind(kge_all_simplewb, kge_val)
# }
#
#
# library(watergap3data)
# ROOT <- "./data/G_RES_TYPE.UNF1"
# AREA <- "./data/G_RESAREA.UNF4"
# BASINS <- "./data/G_CALIB_BASIN.UNF2"
# ID_ZUORDNUNG <- "./data/STATION_LIST.OUT"
#
# CONTINENT <- "na"
#
# # get basins with reservoir type >= 2
# id_table <- watergap3data::txt.read_station_list(ID_ZUORDNUNG)
# reservoir_area <- unf.readunf(AREA, CONTINENT)
# reservoir_types <- unf.readunf(ROOT, CONTINENT)
# basins <- unf.readunf(BASINS, CONTINENT)
# basin_ids <- unique(abs(basins[basins != 0 & reservoir_area >= 2]))
# ids_affected <- paste0("X", id_table$station_name[id_table$internal_ids %in% basin_ids])
#
#
# kge_all <- rbind(kge_all_hanasaki, kge_all_schneider, kge_all_simplewb)
# include_basins <- kge_all %>%
#   tidyr::pivot_wider(values_from=c(kge, a, b, r), names_from=variant) %>%
#   filter_at(vars(starts_with("kge")), any_vars(. >= 0.2)) %>%
#   filter(basin_id  %in% ids_affected)
#
# kge_all %>%
#   filter(basin_id %in% include_basins$basin_id) %>%
#   left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
#   #filter(reservoir_area > 20) %>%
#   tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
#   filter(name %in% c("kge", "r")) %>%
#   filter(., variant %in% c("static_nowb",
#                            "static_hanasaki",
#                            "static_lakes",
#                            "static")) %>%
#   ggplot(., aes(x=variant, y=value)) +
#   geom_boxplot() +
#   facet_wrap(.~name) +
#   coord_cartesian(ylim = c(0, 1)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggsave("./plots/appendix_validation_result_reservoirs_kge_r.png", units="cm", width=16, height=12, dpi=300)
#
# kge_all %>%
#   filter(basin_id %in% include_basins$basin_id) %>%
#   left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
#   #filter(reservoir_area > 20) %>%
#   tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
#   filter(name %in% c("a", "b")) %>%
#   filter(., variant %in% c("static_nowb",
#                            "static_hanasaki",
#                            "static_lakes",
#                            "static")) %>%
#   ggplot(., aes(x=variant, y=value)) +
#   geom_boxplot() +
#   facet_wrap(.~name) +
#   coord_cartesian(ylim = c(0.5, 1.5)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#
# ggsave("./plots/appendix_validation_result_reservoirs_a_b.png", units="cm", width=16, height=12, dpi=300)
