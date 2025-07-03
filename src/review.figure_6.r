# Figure 4

rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/comparison.r")
source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

obs_data <- readRDS("./data/SI_obs_ref_as_m8_100d_complete_validation_period.rds")
data <- readRDS("./data/SI_original.rds")

reservoir1 <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))
reservoir <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))
col_pattern <- "%i_wetlStorage100"

to_analyse <- reservoir
plot_name_res <- sprintf("./plots/review.figure6_%s.png", "reservoir2")


sensitive_basins <- get_sensitive_basins("reservoir")
behavioural_basins <- read_kge_and_define_good_basins()

reduced_data <- data[, , dimnames(data)[[3]] %in% sensitive_basins]

delta_to_models <- array(NA, dim=c(length(to_analyse),
                                   length(dimnames(reduced_data)[[2]]),
                                   length(dimnames(reduced_data)[[3]])),
                         dimnames = list(names(to_analyse),
                                         dimnames(reduced_data)[[2]],
                                         dimnames(reduced_data)[[3]]))

run <- 1
for (model in 1:length(to_analyse)){
  model_pair <- to_analyse[[model]]
  ref_data <- reduced_data[dimnames(reduced_data)[[1]] == sprintf(col_pattern, model_pair[1]), , ]
  compare_data <- reduced_data[dimnames(reduced_data)[[1]] == sprintf(col_pattern, model_pair[2]), , ]
  delta_to_models[run, , ] <- (compare_data - ref_data)
  run <- run + 1
}


delta_long <- delta_to_models %>%
  as.data.frame.table(.) %>%
  as.data.frame(.) %>%
  mutate(
    plot_type = ifelse(Var2 %in% c("mgn_l", "mgn_a", "mgn_h"), "magnitude (mm/d)",
                ifelse(Var2 %in% c("max"), "max (mm/d)",
                ifelse(Var2 %in% c("frq_l", "frq_h", "lowflow_events", "highflow_events"), "frequency (d)",
                ifelse(Var2 %in% c("dur_l", "dur_h"), "duration (d)",
                ifelse(Var2 %in% c("pearson", "sd", "mean"), "KGE components (-)",
                ifelse(Var2 %in% c("monthly_pearson", "monthly_sd", "monthly_mean"), "monthly KGE components (-)",
                ifelse(Var2 %in% c("kge", "monthly_kge"), "KGE (-)",
                      "not defined")))))))) %>%
  mutate(
    coloring = ifelse(Var2 %in% c("mgn_l", "frq_l", "dur_l", "lowflow_events"), "low flow",
               ifelse(Var2 %in% c("mgn_h", "frq_h", "dur_h", "max", "highflow_events"), "high flow",
               ifelse(Var2 %in% c("pearson", "monthly_pearson"), "pearson",
               ifelse(Var2 %in% c("sd", "monthly_sd"), "variation",
               ifelse(Var2 %in% c("mean", "monthly_mean"), "mean",
               ifelse(Var2 %in% c("monthly_nse", "monthly_kge"), "monthly",
               ifelse(Var2 %in% c("kge"), "daily",
                      "not defined"))))))))

data_to_plot <- delta_long %>%
  dplyr::filter( Var2 %in% c("mgn_l", "mgn_h", "dur_l", "dur_h", "frq_l", "frq_h", "mean", "sd","pearson")) %>%
  dplyr::mutate(naming = factor(Var2,
                                levels = c("mgn_l", "dur_l", "frq_l", "mgn_h", "dur_h", "frq_h", "mean", "sd","pearson"),
                                labels = c("low~flow", "low~flow", "low~flow", "high~flow", "high~flow", "high~flow",
                                           "flow~volume~(Delta~alpha)", "variability~(Delta~beta)", "timing~(Delta~r)"))) %>%
  dplyr::mutate(plot_type = factor(plot_type,
                                   levels = c("magnitude (mm/d)", "frequency (d)",
                                            "duration (d)", "KGE components (-)"),
                                  labels = c("Delta~magnitude~'[mm/d]'", "Delta~frequency~'[d]'",
                                             "Delta~duration~'[d]'", "Delta~KGE~comp.~'[-]'")))

data_to_plot$set <- ifelse(data_to_plot$Var3 %in% behavioural_basins$behavioural, "behavioural", "non-behavioural")
data_to_plot$set <- factor(
  data_to_plot$set,
  levels = c("behavioural", "non-behavioural"),
  labels=c("behavioural", "non-behavioural"))
CEX=12
data_to_plot %>%
  dplyr::filter(Var1 == "A") %>%
  ggplot(., aes(x = Var1, y = Freq)) +
    geom_hline(yintercept = 0, col = "darkgrey", alpha = 1, linewidth = 0.8) +
    geom_boxplot(aes(group = set, fill=set), linewidth = 1.) +
    scale_fill_manual(name="", values=datylon_map[c(5,6)]) +
    theme_bw() +
    theme(
      legend.position = c(0.1, 0.2), #0.1, 0.1 -> res1 | 0.1, 0.2 --> res2
      legend.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
  theme(
      legend.text=element_text(size=CEX, color="black"),
      axis.text=element_text(size=CEX, color="black"),
      axis.title=element_text(size=CEX, color="black"),
      strip.text = element_text(size = CEX, color="black")) +
    facet_wrap(naming ~plot_type, scales = "free_y", labeller = label_parsed) +
    #ggh4x::facet_grid2(plot_type ~ coloring, scales = "free_y", independent = "y") +
    scale_color_manual("type", values = hcl.colors(5, "viridis")) +
    xlab("") +
    ylab("")

ggsave(plot_name_res, units = "cm", width = 30, height = 20, dpi = 300)


# 8 vs. 12 --> reservoir 1
obs_values <- data.frame(basin = dimnames(obs_data)[[3]],
           mgn_l = obs_data[1,1,],
           mgn_h = obs_data[1,3,])

sim_values_ref <- data.frame(
  basin = dimnames(data)[[3]],
  mgn_l = data[1,1,],
  mgn_h = data[1,2,])

sim_values_comp <- data.frame(
  basin = dimnames(data)[[3]],
  mgn_l = data[5,1,],
  mgn_h = data[5,2,])

ref <- merge(obs_values, sim_values_ref, by="basin", how="left")
comp <- merge(obs_values, sim_values_comp, by="basin", how="left")

rsq <- function (x, y) cor(x, y) ^ 2

for (col in c("h", "l")){
  col_x = sprintf("mgn_%s.x", col)
  col_y = sprintf("mgn_%s.y", col)

  fm <- as.formula(paste(col_x, "~", col_y, sep=""))

  reg_ref<-lm(fm, data = ref)
  reg_comp<-lm(fm, data = comp)

  max_xy <- max(ref[[col_x]], ref[[col_y]], comp[[col_x]], comp[[col_y]])
  plot(ref[[col_x]], ref[[col_y]], xlim=c(0,max_xy), ylim=c(0,max_xy), main=sprintf("res1: %s", col),
       xlab="observed", ylab="simulated")
  points(comp[[col_x]], comp[[col_y]], pch=19, col="navy")
  abline(a=0, b=1, col="firebrick")
  #abline(reg_ref, col="black")
  #abline(reg_comp, col="navy")

  print(sprintf("reference model (%s): %f", col, summary(reg_ref)$r.squared))
  print(sprintf("comparison model (%s): %f", col, summary(reg_comp)$r.squared))

}


# 12 vs. 16 --> reservoir 1
obs_values <- data.frame(basin = dimnames(obs_data)[[3]],
                         mgn_l = obs_data[1,1,],
                         mgn_h = obs_data[1,3,])

sim_values_ref <- data.frame(
  basin = dimnames(data)[[3]],
  mgn_l = data[5,1,],
  mgn_h = data[5,2,])

sim_values_comp <- data.frame(
  basin = dimnames(data)[[3]],
  mgn_l = data[9,1,],
  mgn_h = data[9,2,])

ref <- merge(obs_values, sim_values_ref, by="basin", how="left")
comp <- merge(obs_values, sim_values_comp, by="basin", how="left")


for (col in c("h", "l")){
  col_x = sprintf("mgn_%s.x", col)
  col_y = sprintf("mgn_%s.y", col)

  fm <- as.formula(paste(col_x, "~", col_y, sep=""))

  reg_ref<-lm(fm, data = ref)
  reg_comp<-lm(fm, data = comp)

  max_xy <- max(ref[[col_x]], ref[[col_y]], comp[[col_x]], comp[[col_y]])
  plot(ref[[col_x]], ref[[col_y]], xlim=c(0,max_xy), ylim=c(0,max_xy), main=sprintf("res2: %s", col),
       xlab="observed", ylab="simulated")
  points(comp[[col_x]], comp[[col_y]], pch=19, col="navy")
  #abline(reg_ref, col="black")
  #abline(reg_comp, col="navy")
  abline(a=0, b=1, col="firebrick")


  print(sprintf("reference model (%s): %f", col, summary(reg_ref)$r.squared))
  print(sprintf("comparison model (%s): %f", col, summary(reg_comp)$r.squared))

}


# #find weird reservoir algorithm (V1) basins
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "mgn_l") %>%
#   filter(Freq < -0.1)
#
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "frq_l") %>%
#   filter(Freq > 80)
#
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "dur_l") %>%
#   filter(Freq > 10)
#
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "mgn_h") %>%
#   filter(Freq >= 0.7)
#
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "frq_h") %>%
#   filter(Freq >= 1.5)
#
# delta_long %>%
#   filter(Var1 == "hanasaki_reservoirs") %>%
#   filter(Var2 == "dur_h") %>%
#   filter(Freq >= 1.5)
#
# #find weird reservoir algorithm (V2) basins
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "mgn_l") %>%
#   filter(Freq < -0.1)
#
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "frq_l") %>%
#   filter(Freq > 80)
#
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "dur_l") %>%
#   filter(Freq > 10)
#
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "mgn_h") %>%
#   filter(Freq >= 0.7)
#
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "frq_h") %>%
#   filter(Freq >= 1.5)
#
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "dur_h") %>%
#   filter(Freq >= 1.5)
#
# #find explanation
# delta_long %>%
#   filter(Var1 == "schneider_reservoirs") %>%
#   filter(Var2 == "lowflow_events") %>%
#   filter(Freq > 1)
#
