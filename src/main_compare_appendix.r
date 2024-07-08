
rm(list=ls())

library(ggplot2)
library(dplyr)


################################################################################
# calibrated version
################################################################################
ROOT <- "./data/cal_result_appendix_variableflow%s.rds"
ROOT_1 <- ""
ROOT_2 <- "_hanasaki"
ROOT_3 <- "_hanasaki_wetlStorage100"
ROOT_4 <- "_wetlStorage100"

variants <- list("schneider"=ROOT_1,
                 "hanasaki"=ROOT_2,
                 "hanasaki_100d"= ROOT_3,
                 "schneider_100d" = ROOT_4
                 )

results <- NULL
results_all = NULL
min_quality <- 0.2
for (variant in names(variants)){
  print(variant)
  appendix <- variants[[variant]]
  sim_results <- readRDS(sprintf(ROOT, appendix))
  val_static <- sim_results[2, 2, 1,]
  val_variable <- sim_results[2, 2, 2,]
  station_sufficient_quality <- (val_static > min_quality | val_variable > min_quality)
  print(sum(station_sufficient_quality))
  data = data.frame("static"=val_static[station_sufficient_quality],
                    "variable"=val_variable[station_sufficient_quality],
                    "variant"=variant)

  results <- rbind(results, data)

  data_all = data.frame("static"=val_static,
                    "variable"=val_variable,
                    "variant"=variant,
                    "basins"= names(val_static))

  results_all <- rbind(results_all, data_all)

}

results %>%
  mutate(delta = static - variable) %>%
  select(-c(static, variable)) %>%
  tidyr::pivot_longer(., cols=-variant) %>%
  mutate(variant = factor(variant, levels=names(variants))) %>%
  filter(abs(value) >= 0.01) %>%
  ggplot(., aes(x=variant, y=value, col=name)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw()

ggsave("./plots/appendix_validation_result_delta.png", units="cm", width=16, height=12, dpi=300)


results_all %>%
  tidyr::pivot_wider(., names_from = variant, values_from = c(static, variable)) %>%
  mutate(delta_static_reservoir = static_schneider - static_hanasaki) %>%
  mutate(delta_static_reservoir_100d = static_schneider_100d - static_hanasaki_100d) %>%
  mutate(delta_var_reservoir_100d = variable_schneider_100d - variable_hanasaki_100d) %>%
  mutate(delta_var_reservoir = variable_schneider - variable_hanasaki) %>%
  tidyr::pivot_longer(., cols=c(delta_static_reservoir,
                                delta_var_reservoir,
                                delta_var_reservoir_100d,
                                delta_static_reservoir_100d)) %>%
  filter(static_schneider > min_quality |
           static_hanasaki > min_quality |
           static_schneider_100d > min_quality |
           static_hanasaki_100d > min_quality |
           variable_schneider_100d > min_quality |
           variable_hanasaki_100d > min_quality |
           variable_schneider > min_quality |
           variable_hanasaki > min_quality) %>%
  filter(abs(value) >= 0.01) %>%
  mutate(variant = factor(variant, levels=names(variants))) %>%
  ggplot(., aes(x=name, y=value, col=name)) +
  #ggplot(., aes(y=value)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw()

my_table <- results_all %>%
  tidyr::pivot_wider(.,names_from = variant, values_from = c(static, variable)) %>%
  mutate(delta_static_reservoir = static_schneider - static_hanasaki) %>%
  mutate(delta_static_reservoir_100d = static_schneider_100d - static_hanasaki_100d) %>%
  mutate(delta_var_reservoir_100d = variable_schneider_100d - variable_hanasaki_100d) %>%
  mutate(delta_var_reservoir = variable_schneider - variable_hanasaki) %>%
  tidyr::pivot_longer(., cols=c(delta_static_reservoir,
                                delta_var_reservoir,
                                delta_var_reservoir_100d,
                                delta_static_reservoir_100d)) %>%
  group_by(name) %>%
  summarise(very_pos = sum(value > 0.1),
            very_neg = sum(value < -0.1),
            pos = sum(0.01 < value & value < 0.1),
            neg = sum(-0.1 < value & value < -0.01),
            no_change = sum(-0.01 < value & value < 0.01))

  ggsave("./plots/appendix_validation_result_reservoirs.png", units="cm", width=16, height=12, dpi=300)

