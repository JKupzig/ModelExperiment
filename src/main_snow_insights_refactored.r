
MIN_QUAL <- 0.0
MAX_QUAL <- NULL
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)

ROOT_100_DISCHARGE <- "./data/evaluation/cal_result_discharges_model_m%i_wetlStorage100.txt"
model_m8_result <- read.table(sprintf(ROOT_100, 8), sep="\t", header=T)
model_m9_result <- read.table(sprintf(ROOT_100, 9), sep="\t", header=T)

attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)
attributes$basin_id <- paste0("X", attributes$grdc_ids)
attributes <- attributes[(attributes$mean_precipitation_as_snow > 0.2) &
                                       (attributes$localWetlands > 10),]

model_m8_result <- model_m8_result[model_m8_result$station %in% behavioural_basins$behavioural,]
model_m9_result <- model_m9_result[model_m9_result$station %in% behavioural_basins$behavioural,]
model_m8_result <- model_m8_result[model_m8_result$station %in% attributes$basin_id,]
model_m9_result <- model_m9_result[model_m9_result$station %in% attributes$basin_id,]

model_m8_with_attributes <- merge(model_m8_result, attributes, by.x="station", by.y="basin_id", all.x=T)
model_m9_with_attributes <- merge(model_m9_result, attributes, by.x="station", by.y="basin_id", all.x=T)

timing <- model_m9_with_attributes$r_val - model_m8_with_attributes$r_val
variability <- abs(1-model_m9_with_attributes$b_val) - abs(1-model_m8_with_attributes$b_val)
magnitude <- abs(1-model_m9_with_attributes$a_val) - abs(1-model_m8_with_attributes$a_val)
kge <- model_m9_with_attributes$KGE_val - model_m8_with_attributes$KGE_val

sensitive_basins <- data.frame(list("timing"=timing, "kge"=kge, "variability"=variability,
                                    "magnitude"=magnitude,
                                    "local_wetlands"=model_m8_with_attributes$localWetlands,
                                    "basin_id"=model_m8_with_attributes$station))

sensitive_basins %>%
  ggplot() +
  xlab("Fraction of smaller wetlands (%)") +
  ylab(("\u0394 (-)")) +
  geom_point(aes(x=local_wetlands, y=kge), color="cornflowerblue", alpha=0.7) +
  geom_point(aes(x=local_wetlands, y=timing), color="black") +
  geom_hline(yintercept=0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=-0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=0.1, col="darkgrey", lwd=.6) +
  xlim(10,75) +
  theme_classic()

sensitive_basins %>%
  ggplot() +
  xlab("Fraction of smaller wetlands (%)") +
  ylab(("\u0394 (-)")) +
  geom_point(aes(x=local_wetlands, y=magnitude), color="cornflowerblue", alpha=0.7) +
  geom_point(aes(x=local_wetlands, y=variability), color="black") +
  geom_hline(yintercept=0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=-0.01, col="darkgrey", lwd=.6) +
  geom_hline(yintercept=0.1, col="darkgrey", lwd=.6) +
  xlim(10,75) +
  theme_classic()

