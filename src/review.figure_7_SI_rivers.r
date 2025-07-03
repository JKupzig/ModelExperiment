rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/comparison.r")
source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

data <- readRDS("./data/SI_original.rds")
PLOT_PATTERN <- "./plots/review.Figure5_%s.png"
MIN <- 0.5
MAX <- 1.5

snow <- c(8, 9)
reservoir <- c(12, 16)
river <- c(8, 11)
reservoir1 <- c(8, 12)

models <- list("snow"=snow, "reservoir1" = reservoir1, "reservoir"=reservoir, "river"=river)
plot_name <- sprintf(PLOT_PATTERN, "river")
behavioural_basins <- read_kge_and_define_good_basins()

pattern_column <- "%i_wetlStorage100"

kge_components_names <- c("pearson", "mean", "sd")
to_evaluate <- c(kge_components_names, "monthly_nse", "kge")



kge_ref <- array(NA,
                     dim = c(
                       1,
                       length(to_evaluate),
                       length(dimnames(data)[[3]])
                     ),
                     dimnames = list(
                       c("ref"),
                       to_evaluate,
                       dimnames(data)[[3]])
)

kge_compare <- array(NA,
                     dim = c(
                       length(models),
                       length(to_evaluate),
                       length(dimnames(data)[[3]])
                     ),
                     dimnames = list(
                       names(models),
                       to_evaluate,
                       dimnames(data)[[3]])
)

kge_delta <- kge_compare

run <- 1
for (model in names(models)){

  reference_model <- sprintf(pattern_column, models[[model]][1])
  comparison_model <- sprintf(pattern_column, models[[model]][2])

  kge_components_ids <- which(dimnames(data)[[2]] %in% to_evaluate)
  ref_data <- data[dimnames(data)[[1]] == reference_model, kge_components_ids, ]
  compare_data <- data[dimnames(data)[[1]] == comparison_model, kge_components_ids, ]
  delta <- compare_data - ref_data
  kge_ref[1, , ] <- ref_data
  kge_compare[run, , ] <- compare_data
  kge_delta[run, , ] <- delta

  run <- run + 1
}

kge_compare_long <- kge_delta %>%
  as.data.frame.table(.) %>%
  as.data.frame(.)


# delete not-sensitive basins
sensitive_basins <- kge_compare_long %>%
  filter((Var1 == "snow" & Var3 %in% get_sensitive_basins("snow")) |
         (Var1 == "reservoir1" & Var3 %in% get_sensitive_basins("reservoir")) |
         (Var1 == "reservoir" & Var3 %in% get_sensitive_basins("reservoir")) |
         (Var1 == "river" & Var3 %in% get_sensitive_basins("river")))

# mark behavioural basins
sensitive_basins$set <- "non-behavioural"
sensitive_basins$set[sensitive_basins$Var3 %in% behavioural_basins$behavioural]  <- "behavioural"

sensitive_basins$Var1 <- factor(
  sensitive_basins$Var1,
  levels=levels(sensitive_basins$Var1),
  labels = c("snow on wetlands", "reservoir algorithm (V1)", "reservoir algorithm (V2)", "variable flow velocity"))

benchmark.labs <- c("Delta~timing~(Delta~Pearson~r)", "Delta~flow~volume~(Delta~alpha)", "Delta~variability~(Delta~beta)")
labs <-  c("pearson", "mean", "sd")

sensitive_basins$Var2 <- factor(
  sensitive_basins$Var2,
  levels = labs,
  labels=benchmark.labs
)



sensitive_basins %>%
  filter(Var2 %in% benchmark.labs) %>%
  filter(Var1 == "variable flow velocity") %>%
  ggplot(.,
         aes(y = Freq, fill=set)) +
  geom_hline(yintercept = 0, lwd=0.5, color="darkgrey") +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(.~Var2, scale="free_y", nrow=1,
             labeller = label_parsed) +
  ylab("Value") +
 theme(legend.position = c(0.5, 0.2),
       legend.background = element_blank(),
       legend.title = element_blank(),
       axis.text.x = element_blank(),
       axis.title.x = element_blank(),
       axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c(datylon_map[5],datylon_map[7]))



ggsave(plot_name, units = "cm", width = 16, height = 8, dpi = 300)




sensitive_basins[which(
  sensitive_basins$Var1 == "snow" &
    sensitive_basins$Var2 == "sd" &
    sensitive_basins$Freq < -0.02),]


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
