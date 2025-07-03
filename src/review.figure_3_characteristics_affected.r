rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")


ATTRIBUTES <- "./data/basin_attributes.txt"
CHARACTERISTIC_LIST <- c("sum_precipitation", "mean_temperature", "mean_precipitation_as_snow","aridity", "localWetlands", "basin_size")
LABELS_TO_USE <- c("Annual precipitation [mm]",
                   "Mean temperature [°C]",
                   "Precipitation as snow [%]",
                   "Aridity Index [-]",
                   "Local wetlands [%]",
                   "Basin area [km²]")


data_to_plot <- list()

names <- c("snow", "reservoir", "river")

attributes <- read.table(ATTRIBUTES, sep = "\t", header = TRUE)
for (name in names){
  affected_basins <- get_sensitive_basins(name=name)

  data <- attributes[attributes$grdc_ids %in% affected_basins,]
  data$mean_precipitation_as_snow <- data$mean_precipitation_as_snow

  data_long <- tidyr::pivot_longer(data, cols = all_of(CHARACTERISTIC_LIST))
  data_long$name <-  factor(data_long$name,
                            levels = CHARACTERISTIC_LIST,
                            labels = LABELS_TO_USE)
  data_long$set <- name
  data_to_plot[[name]] <- data_long
}

data_all <- do.call(rbind, data_to_plot)

CEX=10
ggplot(data_all) +
  stat_ecdf(geom = "step", aes(x=value, color=set), linewidth=1) +
  facet_wrap(.~name, scales = "free") +
  theme_bw() +
  xlab("") +
  ylab("Cum. freq [-]") +
  scale_color_manual(name="basin set:", values=c(datylon_map[c(2,4,6)])) +
  theme(axis.text = element_text(size = CEX)) +
  theme(legend.position = "bottom",
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.box.spacing = unit(0, "pt")) +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black"),
        legend.text = element_text(size=CEX, color="black"),
        legend.title = element_text(size=CEX, color="black"))

ggsave("./plots/review.Figure1_histogram_affected_sets.png",
       dpi = 600, units = "cm", width = 16, height = 12)

length(get_sensitive_basins("snow")) # 47
length(get_sensitive_basins("reservoir")) # 108
length(get_sensitive_basins("river")) #226

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
CONT <- "na"
SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2" # watergap-specific data!
STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"
BASEMAP <- "C:/Users/jenny/MyProject_sciebo_backup/SensitivityAnalysis/ne_110m_land"


spatial_data_basins <- watergap3data::unf.readunf(SPATIAL_INFO, CONT)
station_data <- watergap3data::txt.read_station_list(STATION_INFO)


MAX_NUMBER_IN_GRID <- 1000
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])

  if (grdc_id %in% attributes$grdc_ids){
    grdc_id_as_integer <- as.integer(strsplit(grdc_id, "X")[[1]][2])
    spatial_data_basins[abs(spatial_data_basins) == as.integer(internal_id)] <- MAX_NUMBER_IN_GRID + grdc_id_as_integer
  }
  setTxtProgressBar(pb, row)
}

spatial_data_basins <- (spatial_data_basins - MAX_NUMBER_IN_GRID)
spatial_data_basins[spatial_data_basins < -100] <- NA
spatial_layer_basins <- watergap3data::unf.vec2raster(as.vector(spatial_data_basins), 1, CONT)
polygons_behavioural <- raster::rasterToPolygons(spatial_layer_basins,
                                                 na.rm=TRUE, dissolve=TRUE)

snowish_basins <- get_sensitive_basins("snow")
reservoir_basins <- get_sensitive_basins("reservoir")
river_basins <- get_sensitive_basins("river")

basin_sets <- data.frame("basins" = river_basins)
basin_sets$type <- 0
basin_sets$type <- ifelse(basin_sets$basins %in% snowish_basins, 2, basin_sets$type)
basin_sets$type <- ifelse(basin_sets$basins %in% reservoir_basins, 1, basin_sets$type)
basin_sets$type <- ifelse(((basin_sets$basins %in% reservoir_basins) &
                          ( basin_sets$basins %in% snowish_basins)), 3, basin_sets$type)

basin_sets$layer <- sapply(basin_sets$basins, function(x){ as.integer(strsplit(x, "X")[[1]][2]) })
cal_result <- sp::merge(polygons_behavioural, basin_sets, by="layer", how="right")

sf::st_write(sf::st_as_sf(cal_result), "./data/basin_sets.shp", delete_layer=TRUE)

wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))

#plotting

cal_result$type <- factor(
  cal_result$type,
  levels=c(0,1,2,3),
  labels=c("river", "reservoir+river",  "snow+river", "snow+reservoir+river"))
CEX=10
world_plot <- ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.2, lwd=0.2) +
  ggspatial::geom_sf(data=sf::st_as_sf(cal_result), aes(fill = type), lwd = 0.1) +
  coord_sf(expand = FALSE,
           xlim = c(-15000372.7, -2000000), #-20000
           ylim = c(-400000, 9235574)) +
  scale_fill_manual(
    name="basin set",
    values=datylon_map[c(1,3,6,4)]) +
  theme_bw() +
  theme(legend.position = "bottom", #c(0.7, 0.11),
        legend.key.size = unit(0.3, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.background = element_rect(fill = NA, color = NA),
        legend.frame = element_blank(),
        legend.direction = "horizontal",
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black"),
        legend.text = element_text(size=CEX, color="black"),
        legend.title = element_text(size=CEX, color="black"))

ggsave("./plots/review.affected_sets_map.png", plot=world_plot,
       units="cm", width=20, height=10, dpi=300)

