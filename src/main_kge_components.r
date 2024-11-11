rm(list=ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

MIN_QUAL <- 0.4 #0.2
MAX_QUAL <- NULL #0.4
plot_name <- sprintf("./plots/KGE_components_%f.png", MIN_QUAL)
data <- readRDS("./data/SI_original.rds")
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)

base_model <- "8_wetlStorage100"
comparison_models <- list(
  variable_flowvelocity = "11_wetlStorage100",
  hanasaki_reservoirs = "12_wetlStorage100",
  schneider_reservoirs = "16_wetlStorage100",
  snow_on_wetlands = "9_wetlStorage100"
)


reduced_data <- data[,,dimnames(data)[[3]] %in% behavioural_basins$behavioural]




kge_ref <- array(NA,
                     dim = c(
                       1,
                       4,
                       length(dimnames(reduced_data)[[3]])
                     ),
                     dimnames = list(
                       c("ref"),
                       c("pearson", "mean", "sd", "monthly_NSE"),
                       dimnames(reduced_data)[[3]])
)

kge_compare <- array(NA,
                     dim = c(
                       length(comparison_models),
                       4,
                       length(dimnames(reduced_data)[[3]])
                     ),
                     dimnames = list(
                       names(comparison_models),
                       c("pearson", "mean", "sd", "monthly_NSE"),
                       dimnames(reduced_data)[[3]])
)


run <- 1
for (model in names(comparison_models)){

  kge_components_id <- c(8:10, 12)
  ref_data <- reduced_data[dimnames(reduced_data)[[1]] == base_model,kge_components_id,]
  compare_data <- reduced_data[dimnames(reduced_data)[[1]] == comparison_models[[model]],kge_components_id,]

    kge_ref[1,,] <- ref_data
  kge_compare[run,,] <- compare_data
  run <- run + 1
}


kge_compare_long <- kge_compare %>%
  as.data.frame.table(.) %>%
  as.data.frame(.)

kge_long <- kge_ref %>%
  as.data.frame.table(.) %>%
  as.data.frame(.) %>%
  left_join(., kge_compare_long,
            by = c("Var2","Var3"))

kge_long$var2_new <- ifelse(kge_long$Freq.x == kge_long$Freq.y, "no change", "no dupl.")

MIN = 0.5
MAX = 1.5
excluded <- kge_long %>%
  filter(Freq.y > MAX | Freq.x > MAX | Freq.y < MIN | Freq.x < MIN)
length(unique(excluded$Var3))
min(excluded$Freq.y)

variants.labs <- c("variable flow velocity", "slower wetlands",
                   "reservoir algorithm (V1)", "reservoir algorithm (V2)",
                   "snow on wetlands")
names(variants.labs) <- c("variable_flowvelocity", "slower_wetlands",
                          "hanasaki_reservoirs", "schneider_reservoirs",
                          "snow_on_wetlands")

benchmark.labs <- c("timing (Pearson's r)*", "flow volume (alpha)", "variability (beta)")
names(benchmark.labs) <-  c("pearson", "mean", "sd")

kge_long <- kge_long[order(kge_long$Var1.y,decreasing = FALSE),]
kge_long_subset <- kge_long[kge_long$Var1.y == "hanasaki_reservoirs" |
           kge_long$Var1.y == "schneider_reservoirs",]
dupl = subset(kge_long_subset, select = -c(5) )
dupl <- duplicated(dupl)

ids_to_delete <- kge_long_subset$Var1.y == "schneider_reservoirs" & dupl
kge_long_subset$var2_new <- ifelse(ids_to_delete == TRUE & kge_long_subset$var2_new != "no change",
                                   "no dupl.", kge_long_subset$var2_new) #not used at the moment

kge_long_no_dupl <- rbind(kge_long[kge_long$Var1.y != "hanasaki_reservoirs" &
                                  kge_long$Var1.y != "schneider_reservoirs",],
                          kge_long_subset)

ggplot(kge_long_no_dupl, aes(x=Freq.x, y = Freq.y, group=Var2, col=var2_new)) +
  #geom_point(aes(shape = factor(var2_new))) +
  geom_point(shape = 4, size=2, stroke = 1.1) +
  scale_colour_manual("type", values = c(
    "no change"=grDevices::adjustcolor(datylon_map[3], alpha.f=0.6),
    "no dupl."= grDevices::adjustcolor(datylon_map[1], alpha.f=1.0),
    "dupl." = grDevices::adjustcolor(datylon_map[6], alpha.f=1.0))) +
  theme_bw() +
  geom_abline(slope=1, intercept=0, lty=2) +
  geom_abline(slope=-1, intercept=2, lty=2) +
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_grid(Var2 ~ Var1.y,
             scales = "free",
             labeller = labeller(Var1.y = variants.labs,
                                 Var2 = benchmark.labs)) +
  #xlim(MIN, MAX) +
  #ylim(MIN, MAX) +
  xlab("Reference model") +
  ylab("Comparison model")


ggsave(plot_name, units="cm", width=33, height=20, dpi=300)

VALUE <- 0.05
#evaluation: variable_flowvelocity hanasaki_reservoirs schneider_reservoirs snow_on_wetlands
kge_long_no_dupl[kge_long_no_dupl$Var2 == "pearson" &
                   kge_long_no_dupl$Var1.y == "variable_flowvelocity",] %>%
  mutate(is_better = ifelse((Freq.x + VALUE) <= Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - VALUE) >= Freq.y, 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))


kge_long_no_dupl[kge_long_no_dupl$Var2 == "sd" &
                   kge_long_no_dupl$Var1.y == "snow_on_wetlands",] %>%
  mutate(is_better = ifelse((abs(1-Freq.x) - VALUE) > abs(1-Freq.y), 1, 0)) %>%
  mutate(is_worse = ifelse((abs(1-Freq.x) + VALUE) < abs(1-Freq.y), 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long[kge_long$Var2 == "pearson" &
           kge_long$Var1.y == "hanasaki_reservoirs",] %>%
  mutate(is_better = ifelse((Freq.x + VALUE) <= Freq.y, 1, 0)) %>%
  mutate(is_worse = ifelse((Freq.x - VALUE) >= Freq.y, 1, 0)) %>%
  summarise(sum_is_better = sum(is_better),
            sum_is_worse = sum(is_worse))

kge_long_no_dupl[kge_long_no_dupl$Var2 == "monthly_NSE" &
                   kge_long_no_dupl$Var1.y == "hanasaki_reservoirs",] %>%
  mutate(is_bigger = (Freq.x + 0.3) < Freq.y) %>%
  mutate(is_smaller = (Freq.x - 0.3) > Freq.y) %>%
  summarise(sum_is_bigger = sum(is_bigger),
            sum_is_smaller = sum(is_smaller))


interesting_ids_wetlands <- which(
  (kge_long$Freq.x < (kge_long$Freq.y - 0.1)) &
    as.character(kge_long$Var2) == "mean" &
    as.character(kge_long$Var1.y) == "snow_on_wetlands")
kge_long[interesting_ids_wetlands,]

interesting_ids_reservoirs <- which(
  kge_long$Freq.x < kge_long$Freq.y &
    as.character(kge_long$Var2) == "sd" &
    as.character(kge_long$Var1.y) %in% c("hanasaki_reservoirs","schneider_reservoirs"))
kge_long[interesting_ids_reservoirs,][c(4,5),]

interesting_ids_reservoirs <- which(
  kge_long$Freq.x < (kge_long$Freq.y - 0.08)&
    as.character(kge_long$Var2) == "sd" &
    as.character(kge_long$Var1.y) %in% c("schneider_reservoirs"))
kge_long[interesting_ids_reservoirs,]
