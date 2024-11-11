
rm(list=ls())
library(WaterGAPLite)
library(raster)
library(dplyr)
library(ggplot2)
source("./src/helper/load_wgl.r")
source("./src/helper/color_ramps.r")

target = "./plots/weird_basin_reservoir_%s.png"
basin_info <- read.table("./data/weird_reservoir_basins.txt", sep=";", header=TRUE)
basin_to_simulate <- basin_info[2,]
example_basin <- list("grdc_no" = basin_to_simulate$grdc_no,
                      "corLong" = basin_to_simulate$long,
                      "corlat" = basin_to_simulate$lat,
                      "cont" = basin_to_simulate$cont,
                      "name" = basin_to_simulate$name)

loaded_basin <- load_wgl_init_basin(example_basin)


# get info about reservoirs
res_type <- loaded_basin@G_RES_TYPE
res_type[res_type > 1]#only one hydropower

max_reservoir <- which(loaded_basin@G_RESAREA == max(loaded_basin@G_RESAREA))
loaded_basin@G_STORAGE_CAPACITY[max_reservoir] # 0.2775
loaded_basin@G_MEAN_INFLOW[max_reservoir] #0.4629505
c_ratio <- (loaded_basin@G_STORAGE_CAPACITY[max_reservoir] /
              (loaded_basin@G_MEAN_INFLOW[max_reservoir] * 12)) #ca. 0.05



# now comparing streams:
no_res_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt",
                               sep="\t", header=T)
hanasaki_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m12_wetlStorage100.txt",
                                   sep="\t", header=T)
schneider_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m16_wetlStorage100.txt",
                                   sep="\t", header=T)

no_res_discharge <- no_res_discharge_all[[paste0("X", loaded_basin@id)]]
hanasaki_discharge <- hanasaki_discharge_all[[paste0("X", loaded_basin@id)]]
schneider_discharge <- schneider_discharge_all[[paste0("X", loaded_basin@id)]]
date <- as.Date(schneider_discharge_all[["date"]], format="%Y-%m-%d")

observed_discharge <- Q.read_grdc(
  loaded_basin@id,
  NULL,
  cont = loaded_basin@cont@contName,
  start = min(date),
  end = max(date),
  use_folder = DISCHARGE_DATA_PATH
)

no_leap_year <- which(format(observed_discharge$Date, "%d-%m") != "29-02")

names_of_simtype <- c("Reservoirs as lakes", "Reservoir algorithm (V1)",
                      "Reservoir algorithm (V2)", "Observed discharge")
values=c(colors_to_use, "black")
names(values) <- c(names_of_simtype)
data_all <- data.frame(date=date,
                       no_res_discharge=no_res_discharge,
                       hanasaki_discharge=hanasaki_discharge,
                       schneider_discharge=schneider_discharge,
                       obs=observed_discharge$Value[no_leap_year])
names(data_all) <- c("Date", names_of_simtype)
data_all %>%
  mutate(month = lubridate::month(date)) %>%
  tidyr::pivot_longer(., cols= names_of_simtype) %>%
  group_by(name, month) %>% summarise(monthly_mean=mean(value)) %>%
  ggplot(.) +
    geom_line(aes(x=month, y=monthly_mean, col=name), lwd=1) +
    scale_color_manual(values=values) +
    ylab("Monthly mean of discharge (m3/s)") +
    xlab("Month of year") +
   scale_x_continuous(breaks = scales::breaks_pretty()) +
    theme_bw()

colors_to_use <- datylon_map[c(1,4,7)]
LWD = 1.5
from <- 9*365+80
to <- 10*365+50
png(sprintf(target, example_basin$name), res=300, units="cm", width=20, height=15)
plot(data_all$Date[from:to],
     data_all$`Observed discharge`[from:to], type="l", lwd=LWD,
     ylab="Discharge [m3/s]",
     xlab="",
     ylim=c(0, 1200))

lines(data_all$Date[from:to],
      data_all$`Reservoirs as lakes`[from:to], col=colors_to_use[1], lwd=LWD)

lines(data_all$Date[from:to],
      data_all$`Reservoir algorithm (V1)`[from:to], col=colors_to_use[2], lwd=LWD)
lines(data_all$Date[from:to],
      data_all$`Reservoir algorithm (V2)`[from:to], col=colors_to_use[3], lwd=LWD)

legend("topleft", legend=c("Obs. Discharge","Reservoir as lakes",
                           "Reservoir algorithm (V1)", "Reservoir algorithm (V2)"),
       col=c("black", colors_to_use), cex=0.8, lwd=2, lty=1)

dev.off()

