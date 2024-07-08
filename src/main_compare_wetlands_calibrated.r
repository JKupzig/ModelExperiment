
rm(list=ls())
library(ggplot2)
library(dplyr)
library(gridExtra)

ROOT <- "./data/cal_result_appendix_variableflow.rds"
data <- readRDS(ROOT)

ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
attributes <- read.table(ATTRIBUTES, header=T, sep="\t")
attributes$grdc_ids <- paste0("X", attributes$grdc_ids)
kge_all <- NULL
variant_id <- 1
for (variant in dimnames(data)[[3]]){
  kge_val <- data.frame("kge" = data[2, 1, variant_id, ],
                        "b" = data[2, 2, variant_id, ],
                        "a" = data[2, 3, variant_id, ],
                        "r" = data[2, 4, variant_id, ],
                        "variant"=variant,
                        "basin_id" = dimnames(data)[[4]])
  variant_id <- variant_id + 1

  kge_all <- rbind(kge_all, kge_val)
}

include_basins <- kge_all %>%
  tidyr::pivot_wider(values_from=c(kge, a, b, r), names_from=variant) %>%
  filter_at(vars(starts_with("kge")), any_vars(. >= 0.2))

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(localWetlands > 5) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("kge", "r")) %>%
  ggplot(., aes(x=variant, y=value)) +
    geom_boxplot() +
    facet_wrap(.~name) +
    coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./plots/appendix_validation_result_wetlands_kge_r.png", units="cm", width=16, height=12, dpi=300)

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(localWetlands > 5) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("a", "b")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/appendix_validation_result_wetlands_a_b.png", units="cm", width=16, height=12, dpi=300)

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(localWetlands > 20 & mean_precipitation_as_snow > 0.2) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("kge", "r")) %>%
  filter(., variant %in% c("static", "static_snow",
                           "static_snow_wetlStorage100",
                           "static_wetlStorage100")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/appendix_validation_result_snow_kge_r.png", units="cm", width=16, height=12, dpi=300)


kge_all %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  filter(localWetlands > 20 & mean_precipitation_as_snow > 0.2) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("a", "b")) %>%
  filter(., variant %in% c("static", "static_snow",
                           "static_snow_wetlStorage100",
                           "static_wetlStorage100")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0.1, 1.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/appendix_validation_result_snow_a_b.png", units="cm", width=16, height=12, dpi=300)



cal_results_10d <- read.table(sprintf(ROOT_10d, models[1]), sep="\t", header=T)
cal_results_10d <- data.frame(station=cal_results_10d[,names(cal_results_10d) %in% c("station")])
cal_results_100d <- cal_results_10d

for (model in models){
  model_result <- read.table(sprintf(ROOT_10d, model), sep="\t", header=T)
  cal_results_10d[[sprintf("model_m%i", model)]] <- model_result$kge_val

  model_result <- read.table(sprintf(ROOT_100d, model), sep="\t", header=T)
  cal_results_100d[[sprintf("model_m%i", model)]] <- model_result$kge_val
}

cal_results_100d <- merge(cal_results_100d, attributes,
                          by.x="station", by.y="grdc_ids",
                          all.x=T)
cal_results_100d <- cal_results_100d[order(cal_results_100d$station),]
cal_results_10d <- cal_results_10d[order(cal_results_10d$station),]



model_name <- "model_m18"
sufficient <- (cal_results_100d[[model_name]] > 0.2 |
                 cal_results_10d[[model_name]] > 0.2) & cal_results_100d$localWetlands > 5
red_100d <- cal_results_100d[[model_name]][sufficient]
red_10d <- cal_results_10d[[model_name]][sufficient]

boxplot(list("10d"=red_10d,
             "100d"=red_100d),
        ylim=c(0, 1))

delta <- red_10d - red_100d
sum(delta >= -0.1 & delta < -0.01)
sum(delta <= 0.1 & delta > 0.01)

sum(delta <= 0.01 & delta > -0.01)

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(open_water < 5) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("kge", "r")) %>%
  filter(., variant %in% c("static", "static_wetlStorage100",
                           "variable", "variable_wetlStorage100")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./plots/appendix_validation_result_velocity_kge_r.png", units="cm", width=16, height=12, dpi=300)

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(open_water < 5) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("a", "b")) %>%
  filter(., variant %in% c("static", "static_wetlStorage100",
                           "variable", "variable_wetlStorage100")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/appendix_validation_result_velocity_a_b.png", units="cm", width=16, height=12, dpi=300)
