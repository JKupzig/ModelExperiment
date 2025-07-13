rm(list=ls())

library(dplyr)
library(ggplot2)
library(watergap3data)


source("./src/helper/color_ramps.r")
source("./src/helper/read_data.r")

BASEMAP <- "C:/Users/jenny/MyProject_sciebo_backup/SensitivityAnalysis/ne_110m_land"

cal_result<- sf::st_read("./data/calibration_result.shp")
wmap <- rgdal::readOGR(dsn=BASEMAP, layer=basename(BASEMAP))
wmap_robin <- sp::spTransform(wmap, sp::CRS("+proj=robin"))

plot_name1 <- "./plots/review/appendixA1a_world_plot.png"
plot_name2 <- "./plots/review/appendixA1a_histrogram.png"

kge_info <- read_benchmarks_all("KGE_cal")

model_to_analyse <- "model_m18_100"
model_to_analyse_shortened <- "m_18_10"
#plotting
intervals <- c(-100, 0, 0.2, 0.4, 0.6, 0.8)
interval_labels <- c("-0.361 to 0", "0.0 to 0.2", "0.2 to 0.4",
                     "0.4 to 0.6", "0.6 to 0.75")
cal_result$cal_classes <- cut(cal_result[[model_to_analyse_shortened]],
                   breaks = intervals,
                   labels = interval_labels)


CEX=10
world_plot <- ggplot() +
  ggspatial::geom_sf() +
  ggspatial::geom_sf(data= sf::st_as_sf(wmap_robin), fill=NA, col="black", size=0.2, lwd=0.2) +
  ggspatial::geom_sf(data=sf::st_as_sf(cal_result), aes(fill = cal_classes), lwd = 0.1) +
  coord_sf(expand = FALSE,
           xlim = c(-15000372.7, -2000000), #-20000
           ylim = c(-400000, 9235574)) +
  scale_fill_manual(
    name="KGE [-]",
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
  xlab("KGE [-]") +
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

