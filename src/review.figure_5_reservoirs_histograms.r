rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")
source("./src/helper/comparison.r")


plot_name <- "./plots/review/figure5_reservoirs.png"
CEX = 7
behavioural_set <- read_kge_and_define_good_basins()
sensitive_basins <- get_sensitive_basins("reservoir")
sensitive_basins_r2 <- get_sensitive_basins("non-irrig")

ROOT_ATTRIBUTES <- "./data/basin_attributes.txt"
attributes <- read.table(ROOT_ATTRIBUTES, sep = "\t", header = TRUE)
data <- readRDS("./data/SI_original.rds")


reservoir1 <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))
reservoir <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))

columns <- c("pearson", "mean", "sd", "mgn_l", "mgn_h")
pattern_column <- "%i_wetlStorage100"

names <- c(rep("reservoir algorithm (V1)", length(reservoir1)),
           rep("reservoir algorithm (V2)", length(reservoir)))
all_to_check <- c(reservoir1, reservoir)
intervals_kge <- c(-100, -0.2, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.2, 100)
interval_kge_labels <- c("<-0.2", "-0.2 to -0.1", "-0.1 to -0.05", "-0.05 to -0.01",
                    "-0.01 to 0.01", "0.01 to 0.05", "0.05 to 0.1", "0.1 to 0.2", ">0.2")

intervals_mgn_h <- c(-100, -2, -0.5, -0.2, -0.01, 0.01, 0.2, 0.5, 2, 100)
interval_mgn_h_labels <- c("<-2", "-2 to -0.5", "-0.5 to -0.2", "-0.2 to -0.01",
                         "-0.01 to 0.01", "0.01 to 0.2", "0.2 to 0.5", "0.5 to 2", ">2")

result <- list()
for (column in columns){
  print(column)

  for (set_name in c("behavioural", "complete")){

    data <- readRDS("./data/SI_original.rds")
    data_frame <- setNames(
      data.frame(matrix(NA, ncol = 3,
                        nrow = length(names)*(length(intervals_kge)-1))),
      c("model_part", "interval", "count"))


    if (set_name == "behavioural"){
       data <- data[,,dimnames(data)[[3]] %in% behavioural_set$behavioural]
    } else {
       data <- data[,,!dimnames(data)[[3]] %in% behavioural_set$behavioural]
    }

    start_row <- 1
    end_row <- 1 + length(intervals_kge)-2

    for (entry in 1:length(names)){
      name <- names[entry]

      if (name == "snow on wetlands"){
        sensitive_basins <- get_sensitive_basins("snow")
      } else if (name == "reservoir algorithm (V1)"){
        sensitive_basins <- get_sensitive_basins("reservoir")
      } else if (name == "reservoir algorithm (V2)") {
        sensitive_basins <- get_sensitive_basins("non-irrig")
      } else {
        sensitive_basins <- get_sensitive_basins("river")
      }
      sensitive_data <- data[,,dimnames(data)[[3]] %in% sensitive_basins]

      intervals <- intervals_kge
      if (column == "mgn_h"){
        intervals <- intervals_mgn_h
      }

      reference_model <- sprintf(pattern_column, all_to_check[entry][[1]][1])
      comparison_model <- sprintf(pattern_column, all_to_check[entry][[1]][2])

      kge_components_ids <- which(dimnames(sensitive_data)[[2]] == column)
      ref_data <- sensitive_data[dimnames(sensitive_data)[[1]] == reference_model, kge_components_ids, ]
      compare_data <- sensitive_data[dimnames(sensitive_data)[[1]] == comparison_model, kge_components_ids, ]
      delta <- compare_data - ref_data

      cutted_diff <- cut(delta, intervals, include.lowest = TRUE)
      hist_values <- table(cutted_diff)

      name <- names[entry]

      data_frame[start_row:end_row, 1] <- name
      data_frame[start_row:end_row, 2] <- names(hist_values)
      data_frame[start_row:end_row, 3] <- as.numeric(hist_values)

      start_row <- start_row + length(intervals_kge)-1
      end_row <- end_row + length(intervals_kge)-1
    }

    summarized_values <- data_frame %>%
      group_by(model_part, interval) %>%
      dplyr::summarize(., lower=min(count), mean=median(count), upper=max(count)) %>%
      data.frame()
    summarized_values$set <- set_name
    summarized_values$si <- column

    result[[sprintf("%s_%s", set_name, column)]] <- summarized_values
  }
}

summarized_values <- do.call(rbind, result)
summarized_values$interval <- factor(
  summarized_values$interval,
  levels = c("[-100,-2]", "(-2,-0.5]", "(-0.5,-0.2]", "(-0.2,-0.01]", "[-100,-0.2]",
             "(-0.2,-0.1]", "(-0.1,-0.05]", "(-0.05,-0.01]", "(-0.01,0.01]", "(0.01,0.05]",
             "(0.05,0.1]",  "(0.01,0.2]", "(0.2,0.5]", "(0.1,0.2]",  "(0.2,100]", "(0.5,2]", "(2,100]"),
  labels = c("<-2", "-2 to -0.5", "-0.5 to -0.2", "-0.2 to -0.01", "<-0.2",
             "-0.2 to -0.1", "-0.1 to -0.05","-0.05 to -0.01", "-0.01 to 0.01",  "0.01 to 0.05",
             "0.05 to 0.1", "0.01 to 0.2", "0.2 to 0.5", "0.1 to 0.2", ">0.2", "0.5 to 2", ">2"))
summarized_values$model_part <- factor(
  summarized_values$model_part,
  levels=c("reservoir algorithm (V1)", "reservoir algorithm (V2)"),
  labels = c("reservoir~algorithm~(V1)", "reservoir~algorithm~(V2)"))
summarized_values$set <- factor(
  summarized_values$set,
  levels = c("complete", "behavioural"),
  labels=c("non-behav.", "behav."))
summarized_values$si <- factor(
  summarized_values$si,
  levels = columns,
  labels=c("Delta~timing~(Delta~Pearson~r)~'[-]'", "Delta~flow~volume~(Delta~alpha)~'[-]'", "Delta~variability~(Delta~beta)~'[-]'",
           "Delta~magnitude[low]~'[mm/d]'", "Delta~magnitude[high]~'[mm/d]'"))

behav <- which(summarized_values$set == "behav.")
non_behav <- which(summarized_values$set == "non-behav.")

summarized_values$cor_mean <- summarized_values$mean
summarized_values$cor_mean[non_behav] <- summarized_values$mean[behav] + summarized_values$mean[non_behav]
summarized_values$cor_lower <- summarized_values$lower
summarized_values$cor_lower[non_behav] <- summarized_values$mean[behav] + summarized_values$lower[non_behav]
summarized_values$cor_upper <- summarized_values$upper
summarized_values$cor_upper[non_behav] <- summarized_values$mean[behav] + summarized_values$upper[non_behav]

summarized_values$cor_mean <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_mean)
summarized_values$cor_upper <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_upper)
summarized_values$cor_lower <- ifelse(summarized_values$mean == 0, -1, summarized_values$cor_lower)

summarized_values %>%
  group_by(interval) %>%
  ggplot(aes(x = interval, y = mean, fill=set)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA)) +
  facet_wrap(model_part ~ si, scales="free_x", nrow=2, labeller = label_parsed) +
  geom_errorbar(aes(y = cor_mean, ymin = cor_lower, ymax = cor_upper), width = 0.4,
                position = position_dodge(width = 0.5), size=0.3) +
  geom_point(aes(y = cor_mean), position = position_dodge(width = 0.5), size=0.4) +
  scale_fill_manual(values = c(datylon_map[7],datylon_map[5])) +
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.95), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = NA, colour = NA),
    legend.title=element_blank(),
    legend.text = element_text(size=CEX, color="black"),
    legend.key.size = unit(c(0.5), c("line")),
  ) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) +

  xlab(bquote(Delta ~ .(strsplit(column, "_")[[1]][1]) ~ "[-]")) +
  ylab("Count [-]")

ggsave(plot_name, dpi = 600, units = "cm", width = 18, height = 12)

summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~timing~(Delta~Pearson~r)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[1:4]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))


summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~timing~(Delta~Pearson~r)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[6:9]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))

summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~flow~volume~(Delta~alpha)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[1:4]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))

summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~flow~volume~(Delta~alpha)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[6:9]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))


summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~variability~(Delta~beta)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[1:4]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))

summarized_values %>%
  filter(model_part == "reservoir~algorithm~(V1)") %>%
  filter(si == "Delta~variability~(Delta~beta)~'[-]'") %>%
  filter(interval %in% interval_kge_labels[6:9]) %>%
  group_by(set) %>%
  summarise(sum = sum(mean))

