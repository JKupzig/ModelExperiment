
rm(list=ls())
library(WaterGAPLite)
library(raster)

source("./src/helper/load_wgl.r")
source("./src/helper/color_ramps.r")

colors_to_use <- datylon_map[]
basin_info <- read.table("./data/weird_reservoir_basins.txt", sep=";", header=TRUE)
default_settings <- init.settings(water_use = "on")
res_settings <- init.settings(water_use = "on",
                              reservoir_algorithm ="Hanasaki")

target_2 <- "./plots/weir_basins_reservoirs_hansaki_discharge_%s.png"
target_1 <- "./plots/weird_basins_reservoirs_hansaki_storage_%s.png"


basin_to_simulate <- basin_info[1,]
example_basin <- list("grdc_no" = basin_to_simulate$grdc_no,
                      "corLong" = basin_to_simulate$long,
                      "corlat" = basin_to_simulate$lat,
                      "cont" = basin_to_simulate$cont,
                      "name" = basin_to_simulate$name)

loaded_basin <- load_wgl(example_basin, use_wateruse=1)

#loading oneida data
upstream_discharge <- Q.read_grdc(
  4136411,
  NULL,
  cont = "na",
  start = START,
  end = END,
  use_folder = DISCHARGE_DATA_PATH
)

upstream_discharge_mm <- Q.convert_m3s_mmday(upstream_discharge$Value, 3928.61478042603)


# get info about reservoirs
res_type <- loaded_basin$basin_object@G_RES_TYPE
res_type[res_type==1] #null - no irrigation reservoir
res_type[res_type>=2] <- 1
plot(basin.create_raster(
  res_type,
  loaded_basin$basin_object))

only_lakes <- runModel(
  loaded_basin$run_object$SimPeriod,
  loaded_basin$run_object,
  default_settings,
  10)

using_hanasaki <- runModel(
  loaded_basin$run_object$SimPeriod,
  loaded_basin$run_object,
  res_settings,
  10)

from <- 365*8 #only 88 amd 89
to <- length(loaded_basin$run_object$SimPeriod)

png(sprintf(target_2, example_basin$name), res=300, units="cm", width=20, height=15)
plot(loaded_basin$run_object$SimPeriod[from:to],
     loaded_basin$discharge_mm[from:to], type="l", lwd=2,
     ylab="Discharge [mm/d]",
     xlab="Simulation Period",
     ylim=c(0, 4))
lines(loaded_basin$run_object$SimPeriod[from:to],
      using_hanasaki$routing$River$Discharge[from:to], col="firebrick", lwd=2)
lines(loaded_basin$run_object$SimPeriod[from:to],
      only_lakes$routing$River$Discharge[from:to], col="cornflowerblue", lwd=2)
lines(loaded_basin$run_object$SimPeriod[from:to],
      upstream_discharge_mm[from:to], col="darkgrey", lwd=2)
legend("topleft", legend=c("Obs. Discharge", "Reservoir algorithm (V1)",
                           "Reservoir as lakes", "Upstream gauge"),
       col=c("black", "firebrick", "cornflowerblue", "darkgrey"), lty=1, lwd=2, cex=0.8)
dev.off()


max_reservoir <- which(loaded_basin$basin_object@G_RESAREA == max(loaded_basin$basin_object@G_RESAREA))

loaded_basin$basin_object@G_RES_TYPE[max_reservoir] <- 20.
res_info <- basin.create_raster(
  loaded_basin$basin_object@G_RES_TYPE,
  loaded_basin$basin_object)
raster::writeRaster(res_info, "./data/oswego_project.tif", format="GTiff", overwrite=T)

loaded_basin$basin_object@G_RES_TYPE[max_reservoir]
percentual_storage <- (using_hanasaki$routing$Res$Storage[,max_reservoir] /
                         (loaded_basin$run_object$G_STORAGE_CAPACITY[max_reservoir] * 0.85 * 1000 * 1000))

FACTOR = 25000

png(sprintf(target_1, example_basin$name), res=300, units="cm", width=20, height=15)
plot(loaded_basin$run_object$SimPeriod[from:to],
     percentual_storage[from:to],
      col="cornflowerblue", lty=1, type="l", ylim=c(0,1),
     xlab="Simulation period",
     ylab= "", lwd=2)


axis(4, col.axis="black", at=seq(0,1,0.2), labels=seq(0,FACTOR, 0.2*FACTOR))
mtext("Discharge (mm/d)", side = 4, las=3, line=3, col="black")
mtext("Percent (-)", side = 2, las=3, line=3, col="cornflowerblue")

lines(loaded_basin$run_object$SimPeriod[from:to],
      using_hanasaki$routing$Res$Overflow[,max_reservoir][from:to]/FACTOR,
      col="black", lty=2, lwd=2)

lines(loaded_basin$run_object$SimPeriod[from:to],
      using_hanasaki$routing$Res$Inflow[,max_reservoir][from:to]/FACTOR,
      col="firebrick", lty=1, lwd=2)

lines(loaded_basin$run_object$SimPeriod[from:to],
      (using_hanasaki$routing$Res$Outflow[,max_reservoir][from:to] +
      using_hanasaki$routing$Res$Overflow[,max_reservoir][from:to])/FACTOR,
      col="black", lty=1, lwd=2)


legend("topright",
       legend=c("Reservoir filling",
                "Outflow", "Overflow", "Inflow"),
       col=c("cornflowerblue", "black", "black", "firebrick"),
       lty=c(1,1,3,1), cex=0.8)

dev.off()
loaded_basin$basin_object@G_RESAREA[loaded_basin$basin_object@G_RESAREA > 0]
ids <- which(loaded_basin$basin_object@G_RESAREA > 0)
plot(loaded_basin$run_object$SimPeriod,
     cumsum(using_hanasaki$routing$Res$Overflow[,max_reservoir]), type="l", col="blue",
     ylim = c(0, 1000000),
     ylab="Cum. overflow form reservoir",
     xlab="Simulation period")
for (id in ids[2:length(ids)]){
  lines(loaded_basin$run_object$SimPeriod,
        cumsum(using_hanasaki$routing$Res$Overflow[,id]))
}

