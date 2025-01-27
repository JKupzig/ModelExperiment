# Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.

library(ggplot2)
source("./src/helper/color_ramps.r")

# Data can be found in https://www.naturalearthdata.com/downloads/110m-physical-vectors/110m-land/
BASEMAP <- "./data/ne_110m_land"
xlim <- c(-175, -60)
ylim <- c(15, 80)
if (file.exists(BASEMAP)) {
    wmap <- rgdal::readOGR(dsn = BASEMAP, layer = basename(BASEMAP))
    wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))
    xlim <- c(-15000372.7, -2000000)
    ylim <- c(-400000, 9235574)
}


polygons_behavioural <- raster::shapefile("./data/overview_map.shp")
simple_palette <- c(datylon_map[2], datylon_map[5])
polygons_behavioural$simple_color <- cut(
    polygons_behavioural$layer,
    breaks = c(0, 1.9, 3),
    labels = c("behavioural", "additional"))


world_plot <- ggplot() +
  ggspatial::geom_sf() +
  { if (file.exists(BASEMAP)) ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill="gray87", col="black", size=0.25) } +
  ggspatial::geom_sf(data=sf::st_as_sf(polygons_behavioural), aes(fill = simple_color)) +
  coord_sf(expand = FALSE,
           xlim = xlim,
           ylim = ylim) +
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


ggsave("./plots/Figure_1a_overview.png",
       plot = world_plot,
       units = "cm", width = 20, height = 10, dpi = 300)
