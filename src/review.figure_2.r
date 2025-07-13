rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

BASEMAP <- "C:/Users/jenny/MyProject_sciebo_backup/SensitivityAnalysis/ne_110m_land"

plot_name1 <- "./plots/figure2b_histogram_affected_sets.png"
plot_name2 <- "./plots/figure2a_affected_sets_map.png"

ATTRIBUTES <- "./data/basin_attributes.txt"
CHARACTERISTIC_LIST <- c("sum_precipitation", "mean_temperature", "mean_precipitation_as_snow", "aridity", "localWetlands", "basin_size")
LABELS_TO_USE <- c(
  "Annual precipitation [mm]",
  "Mean temperature [°C]",
  "Precipitation as snow [%]",
  "Aridity Index [-]",
  "Local wetlands [%]",
  "Basin area [km²]"
)


data_to_plot <- list()

names <- c("snow", "reservoir", "river")

attributes <- read.table(ATTRIBUTES, sep = "\t", header = TRUE)
for (name in names) {
  affected_basins <- get_sensitive_basins(name = name)

  data <- attributes[attributes$grdc_ids %in% affected_basins, ]
  data$mean_precipitation_as_snow <- data$mean_precipitation_as_snow

  data_long <- tidyr::pivot_longer(data, cols = all_of(CHARACTERISTIC_LIST))
  data_long$name <- factor(data_long$name,
    levels = CHARACTERISTIC_LIST,
    labels = LABELS_TO_USE
  )
  data_long$set <- name
  data_to_plot[[name]] <- data_long
}

data_all <- do.call(rbind, data_to_plot)

CEX <- 10
ggplot(data_all) +
  stat_ecdf(geom = "step", aes(x = value, color = set), linewidth = 1) +
  facet_wrap(. ~ name, scales = "free") +
  theme_bw() +
  xlab("") +
  ylab("Cum. freq [-]") +
  scale_color_manual(name = "basin set:", values = c(datylon_map[c(2, 4, 6)])) +
  theme(axis.text = element_text(size = CEX)) +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(-10, -10, -10, -10),
    legend.box.spacing = unit(0, "pt")
  ) +
  theme(
    axis.text = element_text(size = CEX, color = "black"),
    axis.title = element_text(size = CEX, color = "black"),
    strip.text = element_text(size = CEX, color = "black"),
    legend.text = element_text(size = CEX, color = "black"),
    legend.title = element_text(size = CEX, color = "black")
  )

ggsave(plot_name1,
  dpi = 600, units = "cm", width = 16, height = 12
)

length(get_sensitive_basins("snow")) # 47
length(get_sensitive_basins("reservoir")) # 108
length(get_sensitive_basins("river")) # 226

# Numbers for text to describe versatility in basins
data <- data_to_plot[["river"]]
min(data$value[data$name == "Precipitation as snow [%]"])
max(data$value[data$name == "Precipitation as snow [%]"])
min(data$value[data$name == "Annual precipitation [mm]"])
max(data$value[data$name == "Annual precipitation [mm]"])
min(data$value[data$name == "Mean temperature [°C]"])
max(data$value[data$name == "Mean temperature [°C]"])
min(data$value[data$name == "Aridity Index [-]"])
max(data$value[data$name == "Aridity Index [-]"])


# spatially plotting basins
basin_polygons <- sf::st_read("./data/basin_sets.shp")

if (file.exists(BASEMAP)){
  wmap <- rgdal::readOGR(dsn = BASEMAP, layer = basename(BASEMAP))
  wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))
} else {
  basin_polygons <- as(basin_polygons, 'Spatial')
  basin_polygons <- sp::spTransform(basin_polygons, sp::CRS("+proj=robin"))
}

# plotting
basin_polygons$type <- factor(
  basin_polygons$type,
  levels = c(0, 1, 2, 3),
  labels = c("river", "reservoir+river", "snow+river", "snow+reservoir+river")
)
CEX <- 10
world_plot <- ggplot() +
  ggspatial::geom_sf() +
  { if(file.exists(BASEMAP))
    ggspatial::geom_sf(data = sf::st_as_sf(wmap_robin), fill = NA, col = "black", size = 0.2, lwd = 0.2)}+
  ggspatial::geom_sf(data = sf::st_as_sf(basin_polygons), aes(fill = type), lwd = 0.1) +
  coord_sf(
    expand = FALSE,
    xlim = c(-15000372.7, -2000000), #-20000
    ylim = c(-400000, 9235574)
  ) +
  scale_fill_manual(
    name = "basin set",
    values = datylon_map[c(1, 3, 6, 4)]
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom", # c(0.7, 0.11),
    legend.key.size = unit(0.3, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.background = element_rect(fill = NA, color = NA),
    legend.frame = element_blank(),
    legend.direction = "horizontal",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    axis.text = element_text(size = CEX, color = "black"),
    axis.title = element_text(size = CEX, color = "black"),
    strip.text = element_text(size = CEX, color = "black"),
    legend.text = element_text(size = CEX, color = "black"),
    legend.title = element_text(size = CEX, color = "black")
  )

ggsave(plot_name2,
  plot = world_plot,
  units = "cm", width = 20, height = 10, dpi = 300
)
