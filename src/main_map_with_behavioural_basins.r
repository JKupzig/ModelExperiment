rm(list=ls())

library(dplyr)
library(ggplot2)
library(watergap3data)

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

CONT <- "na"
MIN_QUAL <- 0.4
SPATIAL_INFO <- "./data/G_CALIB_BASIN.UNF2"
STATION_INFO <- "./data/STATION_LIST.OUT"
BASEMAP <- "C:/Users/jenny/MyProject_sciebo/SensitivityAnalysis/ne_110m_land"

COMPLEXITY_ORDER <- c(1,2,4,3,5,6,7,8,9,10,11,12)
kge_info <- read_benchmarks_all("KGE_cal")
behavioural_basins <- ifelse(kge_info[,2:ncol(kge_info)] >= MIN_QUAL, 1, 0) %>%
  as.data.frame() %>%
  rowwise %>%
  mutate(n_count = sum(c_across(where(is.numeric)))) %>%
  select(n_count)

kge_info$behavioural_basins <- behavioural_basins$n_count
sum(kge_info$behavioural_basins > 0)

additional_basins <- read_kge_and_define_good_basins(min_kge = 0., max_kge = 0.4)
additional_basins <- additional_basins$behavioural
examined_basins <- kge_info$station[kge_info$behavioural_basins > 0]

best_perfomance <- apply(kge_info[,2:ncol(kge_info)], 1, max)
best_models <- c()
for (row in 1:nrow(kge_info)){
  best_model = which(kge_info[row,2:ncol(kge_info)] == best_perfomance[row])
  lowest_complexity <- which(COMPLEXITY_ORDER %in% (best_model))[1]
  best_models <- c(best_models, COMPLEXITY_ORDER[lowest_complexity])
}
kge_info$best_model <- best_models

spatial_data_behavioural <- watergap3data::unf.readunf(SPATIAL_INFO, CONT)
station_data <- watergap3data::txt.read_station_list(STATION_INFO)
MAX_NUMBER_IN_GRID <- 331
pb = txtProgressBar(min = 0, max = nrow(station_data), initial = 0)
for (row in 1:nrow(station_data)) {
  internal_id <- station_data$internal_ids[row]
  grdc_id <- paste0("X", station_data$station_name[row])

  if (grdc_id %in% examined_basins){
    n_behavioural_basins <- kge_info$behavioural_basins[kge_info$station == grdc_id] + MAX_NUMBER_IN_GRID
    spatial_data_behavioural[abs(spatial_data_behavioural) == as.integer(internal_id)] <- n_behavioural_basins
  }

  if (grdc_id %in% additional_basins){
    identifier <- 999 + MAX_NUMBER_IN_GRID
    spatial_data_behavioural[abs(spatial_data_behavioural) == as.integer(internal_id)] <- identifier
  }

  setTxtProgressBar(pb, row)
}

spatial_data_behavioural <- (spatial_data_behavioural - MAX_NUMBER_IN_GRID)
spatial_data_behavioural[spatial_data_behavioural < 0] <- 0
spatial_layer_behavioural <- watergap3data::unf.vec2raster(as.vector(spatial_data_behavioural), 1, CONT)
polygons_behavioural <- raster::rasterToPolygons(spatial_layer_behavioural,
                                     na.rm=TRUE, dissolve=TRUE)

wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))

#plotting behavioural informaiton
palette <- c("gray87", (RColorBrewer::brewer.pal(3, "YlGnBu")), "darkgrey")
polygons_behavioural$colors <- cut(polygons_behavioural$layer,
                   breaks = c(-0.1, 0.1, 3, 7, 13, 1000),
                   labels = c("ungauged", "1-3", "4-7", "7-12",
                              "additional"))
simple_palette <- c("gray87", datylon_map[2], datylon_map[6])
polygons_behavioural$simple_color <- cut(polygons_behavioural$layer,
                                   breaks = c(-0.1, 0.1, 13, 1000),
                                   labels = c("ungauged", "behavioural", "additional"))


world_plot <- ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.25) +
  ggspatial::geom_sf(data=sf::st_as_sf(polygons_behavioural), aes(fill = simple_color)) +
  coord_sf(expand = FALSE,
           xlim = c(-15000372.7, -2000000), #-20000
           ylim = c(-400000, 9235574)) +
  scale_fill_manual(name="", values=simple_palette) +
  theme_bw() +
  theme(legend.position = c(0.5, 0.11),
        legend.key.size = unit(0.3, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.direction = "horizontal",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


ggsave("./plots/calibration_result_map.png", plot=world_plot,
       units="cm", width=20, height=10, dpi=300)

