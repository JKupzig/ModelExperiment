rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

data <- readRDS("./data/SI_original.rds")
PLOT_PATTERN <- "./plots/Figure3_%s.png"
MIN <- 0.5
MAX <- 1.5

additional <- FALSE
if (additional == TRUE) {
  min_qual <- NULL
  max_qual <- 0.4
  name <- "additional"
} else {
  min_qual <- 0.4
  max_qual <- NULL
  name <- "behavioural"
}

plot_name <- sprintf(PLOT_PATTERN, name)
behavioural_basins <- read_kge_and_define_good_basins(min_qual, max_qual)

reference_model <- "8_wetlStorage100"
comparison_models <- list(
  variable_flowvelocity = "11_wetlStorage100",
  hanasaki_reservoirs = "12_wetlStorage100",
  schneider_reservoirs = "16_wetlStorage100",
  snow_on_wetlands = "9_wetlStorage100"
)
kge_components_names <- c("pearson", "mean", "sd")
to_evaluate <- c(kge_components_names, "monthly_nse", "kge")

variants.labs <- c("variable flow velocity", "slower wetlands",
                   "reservoir algorithm (V1)", "reservoir algorithm (V2)",
                   "snow on wetlands")
names(variants.labs) <- c("variable_flowvelocity", "slower_wetlands",
                          "hanasaki_reservoirs", "schneider_reservoirs",
                          "snow_on_wetlands")

benchmark.labs <- c("timing (Pearson's r)*", "flow volume (alpha)", "variability (beta)")
names(benchmark.labs) <-  c("pearson", "mean", "sd")

reduced_data <- data[, , dimnames(data)[[3]] %in% behavioural_basins$behavioural]


kge_ref <- array(NA,
                     dim = c(
                       1,
                       length(to_evaluate),
                       length(dimnames(reduced_data)[[3]])
                     ),
                     dimnames = list(
                       c("ref"),
                       to_evaluate,
                       dimnames(reduced_data)[[3]])
)

kge_compare <- array(NA,
                     dim = c(
                       length(comparison_models),
                       length(to_evaluate),
                       length(dimnames(reduced_data)[[3]])
                     ),
                     dimnames = list(
                       names(comparison_models),
                       to_evaluate,
                       dimnames(reduced_data)[[3]])
)


run <- 1
for (model in names(comparison_models)){

  kge_components_ids <- which(dimnames(reduced_data)[[2]] %in% to_evaluate)
  ref_data <- reduced_data[dimnames(reduced_data)[[1]] == reference_model, kge_components_ids, ]
  compare_data <- reduced_data[dimnames(reduced_data)[[1]] == comparison_models[[model]], kge_components_ids, ]

  kge_ref[1, , ] <- ref_data
  kge_compare[run, , ] <- compare_data
  run <- run + 1
}

kge_compare_long <- kge_compare %>%
  as.data.frame.table(.) %>%
  as.data.frame(.)

kge_long <- kge_ref %>%
  as.data.frame.table(.) %>%
  as.data.frame(.) %>%
  left_join(., kge_compare_long,
            by = c("Var2", "Var3"))

# check for duplicates to highlight entries in plot
kge_long$var2_new <- ifelse(kge_long$Freq.x == kge_long$Freq.y, "no change", "no dupl.")

dummy <- data.frame(
  Var1.x = c(rep(c(0.3, 1), 4), rep(c(0.7, 1.75), 4), rep(c(0.2, 2.5), 4)),
  Var2 = c(rep("pearson", 8), rep("mean", 8), rep("sd", 8)),
  Var3 = 0,
  Freq.x = c(rep(c(0.3, 1), 4), rep(c(0.7, 1.75), 4), rep(c(0.2, 2.5), 4)),
  Var1.y = c(rep("variable_flowvelocity", 2), rep("hanasaki_reservoirs", 2),rep("schneider_reservoirs", 2),rep("snow_on_wetlands", 2),
             rep("variable_flowvelocity", 2), rep("hanasaki_reservoirs", 2),rep("schneider_reservoirs", 2),rep("snow_on_wetlands", 2),
             rep("variable_flowvelocity", 2), rep("hanasaki_reservoirs", 2),rep("schneider_reservoirs", 2),rep("snow_on_wetlands", 2)),
  Freq.y = c(rep(c(0.3, 1), 4), rep(c(0.7, 1.75), 4), rep(c(0.2, 2.5), 4)),
  var2_new = "no change"
)
dummy$Var2 <- factor(dummy$Var2, levels = c("pearson", "mean", "sd"))
dummy$Var1.y <- factor(dummy$Var1.y, levels = c("variable_flowvelocity", "hanasaki_reservoirs", "schneider_reservoirs", "snow_on_wetlands"))

kge_long %>%
  filter(Var2 %in% kge_components_names) %>%
  ggplot(.,
        aes(x = Freq.x, y = Freq.y, group = Var2, col = var2_new)) +
  geom_point(shape = 4, size = 2, stroke = 1.1) +
  scale_colour_manual("type", values = c(
    "no change" = grDevices::adjustcolor(datylon_map[3], alpha.f = 0.6),
    "no dupl." = grDevices::adjustcolor(datylon_map[1], alpha.f = 1.0),
    "dupl." = grDevices::adjustcolor(datylon_map[6], alpha.f = 1.0))) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_abline(slope = -1, intercept = 2, lty = 2) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(Var2 ~ Var1.y,
             scales = "free",
             labeller = labeller(Var1.y = variants.labs,
                                 Var2 = benchmark.labs)) +
  geom_blank(data = dummy) +
  xlab("Reference model") +
  ylab("Comparison model")

ggsave(plot_name, units = "cm", width = 33, height = 20, dpi = 300)


# getting basins with weird behaviour - highlighted in plot
interesting_ids_wetlands <- which(
  (kge_long$Freq.x < (kge_long$Freq.y - 0.1)) &
    as.character(kge_long$Var2) == "mean" &
    as.character(kge_long$Var1.y) == "snow_on_wetlands")
kge_long[interesting_ids_wetlands, ]

interesting_ids_reservoirs <- which(
  kge_long$Freq.x < (kge_long$Freq.y - 0.1) &
    as.character(kge_long$Var2) == "sd" &
    as.character(kge_long$Var1.y) %in% c("hanasaki_reservoirs","schneider_reservoirs"))
kge_long[interesting_ids_reservoirs, ]

#evaluation (numbers are present in text): variable_flowvelocity
kge_long[kge_long$Var2 == "pearson" &
         kge_long$Var1.y == "variable_flowvelocity", ] %>%
  mutate(is_better = ifelse((Freq.x + 0.01) <= Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - 0.05) >= Freq.y, 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

# comparison made by Verzano et al. (2009)
kge_long[kge_long$Var2 == "monthly_nse" &
         kge_long$Var1.y == "variable_flowvelocity", ] %>%
  mutate(is_better = ifelse((Freq.x + 0.0) < Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - 0.0) > Freq.y, 1, 0)) %>%
  summarise(sum_is_better_perc = sum(is_better) / length(unique(kge_long$Var3)) * 100.,
            sum_is_worse_perc = sum(is_worse) / length(unique(kge_long$Var3)) * 100.,
            sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

#evaluation: hanasaki_reservoirs
kge_long[kge_long$Var2 == "sd" &
         kge_long$Var1.y == "hanasaki_reservoirs", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.05) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.05) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long[kge_long$Var2 == "pearson" &
         kge_long$Var1.y == "hanasaki_reservoirs", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.05) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.05) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

# Comparison made by Döll et al. (2009)
kge_long[kge_long$Var2 == "monthly_nse" &
           kge_long$Var1.y == "hanasaki_reservoirs", ] %>%
  mutate(is_better = ifelse((Freq.x + 0.01) < Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - 0.01) > Freq.y, 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

#evaluation: schneider_reservoirs
kge_long[kge_long$Var2 == "sd" &
         kge_long$Var1.y == "schneider_reservoirs", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.05) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.05) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long[kge_long$Var2 == "pearson" &
         kge_long$Var1.y == "schneider_reservoirs", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.05) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.05) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

#evaluation: snow_on_wetlands
kge_long[kge_long$Var2 == "pearson" &
         kge_long$Var1.y == "snow_on_wetlands", ] %>%
  mutate(is_better = ifelse((Freq.x + 0.01) < Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - 0.01) > Freq.y, 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long[kge_long$Var2 == "sd" &
         kge_long$Var1.y == "snow_on_wetlands", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.01) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.01) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long[kge_long$Var2 == "kge" &
         kge_long$Var1.y == "snow_on_wetlands", ] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - 0.01) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + 0.01) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))
