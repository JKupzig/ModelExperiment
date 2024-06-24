
library(watergap3data)
ROOT <- "./data/G_RES_TYPE.UNF1"
AREA <- "./data/G_RESAREA.UNF4"
BASINS <- "./data/G_CALIB_BASIN.UNF2"
CONTINENT <- "na"

reservoir_area <- unf.readunf(AREA, CONTINENT)
reservoir_types <- unf.readunf(ROOT, CONTINENT)
basins <- unf.readunf(BASINS, CONTINENT)

reservoirs_in_data <- reservoir_types[basins != 0 & reservoir_area > 0]
table(reservoirs_in_data)

# 1: hanasaki - irrigation
# 2-7: dynamicOpt_1

