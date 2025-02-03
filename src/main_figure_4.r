# Figure 4

rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")

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

plot_name_res <- sprintf("./plots/figure4_%s.png", name)
data <- readRDS("./data/SI_original.rds")

base_model <- "8_wetlStorage100"
comparison_models <- list(
  variable_flowvelocity = "11_wetlStorage100",
  hanasaki_reservoirs = "12_wetlStorage100",
  schneider_reservoirs = "16_wetlStorage100",
  snow_on_wetlands = "9_wetlStorage100"
)


behavioural_basins <- read_kge_and_define_good_basins(min_kge=min_qual, max_kge=max_qual)

reduced_data <- data[, , dimnames(data)[[3]] %in% behavioural_basins$behavioural]

delta_to_models <- array(NA, dim=c(length(comparison_models),
                                   length(dimnames(reduced_data)[[2]]),
                                   length(dimnames(reduced_data)[[3]])),
                         dimnames = list(names(comparison_models),
                                         dimnames(reduced_data)[[2]],
                                         dimnames(reduced_data)[[3]]))

run <- 1
for (model in names(comparison_models)){
  ref_data <- reduced_data[dimnames(reduced_data)[[1]] == base_model, , ]
  compare_data <- reduced_data[dimnames(reduced_data)[[1]] == comparison_models[[model]], , ]
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
  dplyr::filter( Var2 %in% c("mgn_l", "mgn_h", "dur_l", "dur_h", "frq_l", "frq_h")) %>%
  dplyr::mutate(plot_type = factor(plot_type,
                                   levels = c("magnitude (mm/d)", "frequency (d)",
                                            "duration (d)", "KGE components (-)"))) %>%
  dplyr::mutate(Var1 = factor(
    Var1,
    labels = c("variable flow velocity", "reservoir algorithm (V1)",  "reservoir algorithm (V2)", "snow on wetlands"),
    levels = c("variable_flowvelocity", "hanasaki_reservoirs", "schneider_reservoirs", "snow_on_wetlands"),
  ))


data_to_plot %>%
  dplyr::filter(Var1 %in% c("reservoir algorithm (V1)", "reservoir algorithm (V2)")) %>%
  ggplot(., aes(x = Var1, y = Freq)) +
    geom_violin(aes(group = Var1), col = "black", scale = "area", linewidth = 1.) +
    geom_jitter(aes(y = Freq), col = "gray20",
                alpha = 0.3, show.legend = FALSE, shape = 4, size = 1., stroke = 1.5) +
    geom_hline(yintercept = 0, col = "firebrick", alpha = 0.6, linewidth = 1.) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    ggh4x::facet_grid2(plot_type ~ coloring, scales = "free_y", independent = "y") +
    scale_color_manual("type", values = hcl.colors(5, "viridis")) +
    xlab("") +
    ylab("")

ggsave(plot_name_res, units = "cm", width = 30, height = 20, dpi = 300)

#find weird reservoir algorithm (V1) basins
delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "mgn_l") %>%
  filter(Freq < -0.1)

delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "frq_l") %>%
  filter(Freq > 80)

delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "dur_l") %>%
  filter(Freq > 10)

delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "mgn_h") %>%
  filter(Freq >= 0.7)

delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "frq_h") %>%
  filter(Freq >= 1.5)

delta_long %>%
  filter(Var1 == "hanasaki_reservoirs") %>%
  filter(Var2 == "dur_h") %>%
  filter(Freq >= 1.5)

#find weird reservoir algorithm (V2) basins
delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "mgn_l") %>%
  filter(Freq < -0.1)

delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "frq_l") %>%
  filter(Freq > 80)

delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "dur_l") %>%
  filter(Freq > 10)

delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "mgn_h") %>%
  filter(Freq >= 0.7)

delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "frq_h") %>%
  filter(Freq >= 1.5)

delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "dur_h") %>%
  filter(Freq >= 1.5)

#find explanation
delta_long %>%
  filter(Var1 == "schneider_reservoirs") %>%
  filter(Var2 == "lowflow_events") %>%
  filter(Freq > 1)

