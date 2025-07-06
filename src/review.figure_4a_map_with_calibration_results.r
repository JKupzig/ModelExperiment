# Figure 4a and 4b
rm(list=ls())

library(dplyr)
library(ggplot2)
library(watergap3data)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

CONT <- "na"
MIN_QUAL <- 0.4
SPATIAL_INFO <- "./data - Kopie/G_CALIB_BASIN.UNF2" # watergap-specific data!
STATION_INFO <- "./data - Kopie/STATION_LIST.OUT"
BASEMAP <- "C:/Users/jenny/MyProject_sciebo_backup/SensitivityAnalysis/ne_110m_land"

plot_name1 <- "./plots/review/figure4_calibration_result_map.png"
plot_name2 <- "./plots/review/figure4_calibration_result_map_histogram.png"

column <- "KGE_cal"
kge_info <- read_benchmarks_all(column)

model_to_analyse <- "model_m18_100"

spatial_data_basins <- watergap3data::unf.readunf(SPATIAL_INFO, CONT)
station_data <- watergap3data::txt.read_station_list(STATION_INFO)

MAX_NUMBER_IN_GRID <- 1000
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$stations[row])

  if (grdc_id %in% kge_info$station){
    #perfomance <- kge_info$model_m19_100[kge_info$station == grdc_id]
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


kge_info$layer <- sapply(kge_info$station, function(x){ as.integer(strsplit(x, "X")[[1]][2]) })
cal_result <- sp::merge(polygons_behavioural, kge_info, by="layer", how="right")

sf::st_write(sf::st_as_sf(cal_result), "./data/calibration_result.shp", delete_layer=TRUE)

wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))

#plotting
intervals <- c(-100, 0, 0.2, 0.4, 0.6, 0.8)
interval_labels <- c("-0.361 to 0", "0.0 to 0.2", "0.2 to 0.4",
                     "0.4 to 0.6", "0.6 to 0.75")
polygons_behavioural$cal_classes <- cut(cal_result[[model_to_analyse]],
                   breaks = intervals,
                   labels = interval_labels)


CEX=10
world_plot <- ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.2, lwd=0.2) +
  ggspatial::geom_sf(data=sf::st_as_sf(polygons_behavioural), aes(fill = cal_classes), lwd = 0.1) +
  coord_sf(expand = FALSE,
           xlim = c(-15000372.7, -2000000), #-20000
           ylim = c(-400000, 9235574)) +
  scale_fill_manual(
    name=bquote(.(strsplit(column, "_")[[1]][1]) ~ "[-]"),
    values=c(datylon_map[1:3],datylon_map[c(6:4)] )) +
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

ggsave(plot_name1, plot=world_plot,
       units="cm", width=20, height=10, dpi=300)


interval_names <- cut(kge_info[[model_to_analyse]], intervals, include.lowest = TRUE)
hist_values <- as.data.frame(table(interval_names))
hist_values$interval_names <- factor(
  hist_values$interval_names,
  levels=hist_values$interval_names,
  labels=interval_labels)

CEX=9
hist_plot <- ggplot(hist_values, aes(x = interval_names, y = Freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab(bquote(.(strsplit(column, "_")[[1]][1]) ~ "[-]")) +
  ylab("Count [-]") +
  theme(axis.text=element_text(size=CEX, color="black"),
        axis.title=element_text(size=CEX, color="black"),
        strip.text = element_text(size = CEX, color="black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("darkgrey")) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), #transparent legend panel
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
  )

ggsave(plot_name2, plot=hist_plot,
       units="cm", width=5, height=5, dpi=300)

# examine cal result
sort(kge_info[[model_to_analyse]])

