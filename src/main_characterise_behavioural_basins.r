rm(list=ls())

library(dplyr)
library(ggplot2)
library(watergap3data)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

CONT <- "na"
MIN_QUAL <- 0.0
MAX_QUAL <- NULL
CHARACTERISTIC_INFORMATION <- r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3/data/NEW_x_orig.rds)"
MORE_ATTRIBUTES <- "./data/reduced_basin_attributes.txt"


ID_INFO <-  r"(C:\Users\jenny\MyProject_sciebo\_Nina\Regionalization\R\data_availability\regionalization_watergap3/data/NEW_y.rds)"
CHARACTERISTIC_LIST <- c("sum_prec", "mean_temp", "mean_precipitation_as_snow","aridity")
LABELS_TO_USE <- c("Annual~Precipitation~(mm)",
                   "Mean~Temperature~('°C')",
                   "Precipitation~as~snow~('%')",
                   "Aridity~Index~('-')")


data_to_plot = list()
lower_thresholds <- list(0, 0.4, 0)
higher_thresholds <- list(0.4,  NULL, NULL)
# additional, behavioural, all

for (basin_group in c(1,2,3)){
  behavioural <- read_kge_and_define_good_basins(
    lower_thresholds[[basin_group]],
    higher_thresholds[[basin_group]])
  behavioural_basins <- as.data.frame(list("station"=behavioural$behavioural))
  characteristics <- readRDS(CHARACTERISTIC_INFORMATION)
  id_info <- readRDS(ID_INFO)
  characteristics$station <- paste0("X", id_info$ID)
  characteristics$mean_sealedArea <- characteristics$mean_sealedArea * 100
  data <- merge(behavioural_basins, characteristics, by="station")


  attributes <- read.table(MORE_ATTRIBUTES, sep="\t", header=T)
  attributes$station <- paste0("X", attributes$grdc_ids)
  data <- merge(data, attributes, by="station")
  data$mean_precipitation_as_snow <- data$mean_precipitation_as_snow * 100.
  data$reservoir_area <- data$reservoir_area / data$basin_size * 1000

  data_long <- tidyr::pivot_longer(data, cols = all_of(CHARACTERISTIC_LIST))
  data_long$name <-  factor(data_long$name,
                            levels = CHARACTERISTIC_LIST,
                            labels = LABELS_TO_USE)
  data_to_plot[[basin_group]] <- data_long

  attributes <- attributes[attributes$station %in% behavioural_basins$station,]
  wetland_groups <- cut(attributes$localWetlands,
                        breaks = c(-0.1, 0.1, 5, 10, 20, 50, 100),
                        labels = c("no local wetlands", "1-5%", "5-10%", "10-20%",
                                   "20-50%", "50-100%"))
  wetland_groups_df <- as.data.frame(table(wetland_groups))


  lbls <- paste0(wetland_groups_df$wetland_groups, " (", wetland_groups_df$Freq, ")")
  par(bg=NA)

  pie(wetland_groups_df$Freq, labels=lbls,
      border="white", col=RColorBrewer::brewer.pal(6, "Blues"),
      radius=1.0, cex=1.9)
  # legend("topleft",
  #        legend = c("no smaller wetlands", "1-5%", "5-10%", "10-20%",
  #                            "20-50%", "50-100%"),
  #        fill =  RColorBrewer::brewer.pal(6, "Blues"))
  dev.copy(png,
           sprintf("./plots/characteristics_local_wetlands_%f_%i.png", MIN_QUAL, basin_group), res=300,
           width=40, height=40, unit="cm")
  dev.off()
}


# additional, behavioural, all
ggplot() +
  geom_histogram(data_to_plot[[2]], mapping = aes(value,
                                                  fill = "behavioural"),
                 alpha = 1) +
  geom_histogram(data_to_plot[[1]], mapping = aes(value,
                                                  fill = "additonal"),
                 alpha = 0.6) +
  facet_wrap(~name, scales = "free",
             labeller = label_parsed) +
  scale_fill_manual(name="", values=c("behavioural"=datylon_map[2],
                                      "additonal"=datylon_map[6])) +
  theme_bw() +
  ylab("Count (-)") +
  xlab("Characteristic value") +
  theme(axis.text=element_text(size=10)) +
  theme(panel.spacing = unit(1.5, "lines"))

ggsave("./plots/characteristics.png",
       dpi=300, units="cm", width=16, height=12)



data <- data_to_plot[[3]]
min(data$value[data$name == "Precipitation~as~snow~('%')"])
max(data$value[data$name == "Precipitation~as~snow~('%')"])
min(data$value[data$name == "Yearly~Precipitation~(mm)"])
max(data$value[data$name == "Yearly~Precipitation~(mm)"])
min(data$value[data$name == "Mean~Temperature~('°C')"])
max(data$value[data$name == "Mean~Temperature~('°C')"])
min(data$value[data$name == "Aridity~Index~('-')"])
max(data$value[data$name == "Aridity~Index~('-')"])


