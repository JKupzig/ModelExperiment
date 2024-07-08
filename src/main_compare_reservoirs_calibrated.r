
rm(list=ls())
library(ggplot2)
library(dplyr)
library(gridExtra)

ROOT <- "./data/cal_result_appendix_variableflow.rds"
data <- readRDS(ROOT)

ROOT_HANASKI <- "./data/cal_result_appendix_variableflow_hanasaki.rds"
data_hanasaki <- readRDS(ROOT_HANASKI)

ROOT_SIMPLE_WB <- "./data/cal_result_appendix_variableflow_simple_wb.rds"
data_simple_wb <- readRDS(ROOT_SIMPLE_WB)


ATTRIBUTES <- "./data/reduced_basin_attributes.txt"
attributes <- read.table(ATTRIBUTES, header=T, sep="\t")
attributes$grdc_ids <- paste0("X", attributes$grdc_ids)

kge_all_schneider <- NULL
variant_id <- 1
for (variant in dimnames(data)[[3]]){
  kge_val <- data.frame("kge" = data[2, 1, variant_id, ],
                        "b" = data[2, 2, variant_id, ],
                        "a" = data[2, 3, variant_id, ],
                        "r" = data[2, 4, variant_id, ],
                        "variant"=variant,
                        "basin_id" = dimnames(data)[[4]])
  variant_id <- variant_id + 1

  kge_all_schneider <- rbind(kge_all_schneider, kge_val)
}

kge_all_hanasaki <- NULL
variant_id <- 1
for (variant in dimnames(data_hanasaki)[[3]]){
  kge_val <- data.frame("kge" = data_hanasaki[2, 1, variant_id, ],
                        "b" = data_hanasaki[2, 2, variant_id, ],
                        "a" = data_hanasaki[2, 3, variant_id, ],
                        "r" = data_hanasaki[2, 4, variant_id, ],
                        "variant"=variant,
                        "basin_id" = dimnames(data_hanasaki)[[4]])
  variant_id <- variant_id + 1

  kge_all_hanasaki <- rbind(kge_all_hanasaki, kge_val)
}

kge_all_simplewb <- NULL
variant_id <- 1
for (variant in dimnames(data_simple_wb)[[3]]){
  kge_val <- data.frame("kge" = data_simple_wb[2, 1, variant_id, ],
                        "b" = data_simple_wb[2, 2, variant_id, ],
                        "a" = data_simple_wb[2, 3, variant_id, ],
                        "r" = data_simple_wb[2, 4, variant_id, ],
                        "variant"=variant,
                        "basin_id" = dimnames(data_simple_wb)[[4]])
  variant_id <- variant_id + 1

  kge_all_simplewb <- rbind(kge_all_simplewb, kge_val)
}




kge_all <- rbind(kge_all_hanasaki, kge_all_schneider, kge_all_simplewb)
include_basins <- kge_all %>%
  tidyr::pivot_wider(values_from=c(kge, a, b, r), names_from=variant) %>%
  filter_at(vars(starts_with("kge")), any_vars(. >= 0.2))

kge_all[kge_all$basin_id == "X4146281",]
kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(reservoir_area > 20) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("kge", "r")) %>%
  filter(., variant %in% c("static_nowb",
                           "static_hanasaki",
                           "static_lakes",
                           "static")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./plots/appendix_validation_result_reservoirs_kge_r.png", units="cm", width=16, height=12, dpi=300)

kge_all %>%
  filter(basin_id %in% include_basins$basin_id) %>%
  left_join(., attributes, join_by(basin_id==grdc_ids)) %>%
  filter(reservoir_area > 20) %>%
  tidyr::pivot_longer(., cols=c(kge, a, b, r)) %>%
  filter(name %in% c("a", "b")) %>%
  filter(., variant %in% c("static_nowb",
                           "static_hanasaki",
                           "static_lakes",
                           "static")) %>%
  ggplot(., aes(x=variant, y=value)) +
  geom_boxplot() +
  facet_wrap(.~name) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("./plots/appendix_validation_result_reservoirs_a_b.png", units="cm", width=16, height=12, dpi=300)
