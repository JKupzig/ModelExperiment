
rm(list=ls())
library(WaterGAPLite)
library(raster)
library(dplyr)
library(ggplot2)
source("./src/helper/load_wgl.r")
source("./src/helper/color_ramps.r")

target = "./plots/weird_basin_reservoir_%s.png"
basin_info <- read.table("./data/weird_reservoir_basins.txt", sep=";", header=TRUE)
colors_to_use <- datylon_map[c(1,4,7)]


basin_list <- list()
for (row in 3:5){
  basin_to_simulate <- basin_info[row,] #3-5 wierd in schneider
  example_basin <- list("grdc_no" = basin_to_simulate$grdc_no,
                        "corLong" = basin_to_simulate$long,
                        "corlat" = basin_to_simulate$lat,
                        "cont" = basin_to_simulate$cont,
                        "name" = basin_to_simulate$name)

  loaded_basin <- load_wgl_init_basin(example_basin)

  # get info about reservoirs
  res_type <- loaded_basin@G_RES_TYPE

  basin_info_str <- "reservoir types (%i): %s"
  print(sprintf(basin_info_str, basin_to_simulate$grdc_no, res_type[res_type > 1]))

  max_reservoir <- which(loaded_basin@G_RESAREA == max(loaded_basin@G_RESAREA))
  loaded_basin@G_STORAGE_CAPACITY[max_reservoir] # 0.2775
  loaded_basin@G_MEAN_INFLOW[max_reservoir] #0.4629505
  c_ratio <- (loaded_basin@G_STORAGE_CAPACITY[max_reservoir] /
                (loaded_basin@G_MEAN_INFLOW[max_reservoir] * 12)) #ca. 0.05

  basin_list[[as.character(basin_to_simulate$grdc_no)]] <- loaded_basin
}

# look at tobique river - is there storage with overflow upstream of the reservoir?
waterbodies <- (basin_list$"4125050"@G_GLOWET)
waterbodies[basin_list$"4125050"@G_LAKAREA > 0] <- -1
waterbodies[basin_list$"4125050"@G_RESAREA > 0] <- -2
raster::plot(WaterGAPLite::basin.create_raster(waterbodies, basin_list$"4125050"))
raster::plot(WaterGAPLite::basin.create_raster( basin_list$"4125050"@G_ALTITUDE, basin_list$"4125050"))

# now comparing streams:
no_res_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt",
                               sep="\t", header=T)
hanasaki_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m12_wetlStorage100.txt",
                                   sep="\t", header=T)
schneider_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m16_wetlStorage100.txt",
                                   sep="\t", header=T)

limits <- c(NA, NA, 2000, 4000, 8000)
for (basin_nr in c(3,4,5)){
  loaded_basin_id <- basin_info[basin_nr,1]

  no_res_discharge <- no_res_discharge_all[[paste0("X", loaded_basin_id)]]
  hanasaki_discharge <- hanasaki_discharge_all[[paste0("X", loaded_basin_id)]]
  schneider_discharge <- schneider_discharge_all[[paste0("X", loaded_basin_id)]]
  date <- as.Date(schneider_discharge_all[["date"]], format="%Y-%m-%d")

  plot(seq(0.9, 1.0, 0.005), quantile(no_res_discharge, seq(0.9, 1.0, 0.005)), ylim=c(100, limits[basin_nr]),
       main=loaded_basin_id, ylab="Discharge", xlab="Percentiles", type="l",
       log = "y")
  lines(seq(0.9, 1.0, 0.005), quantile(schneider_discharge, seq(0.9, 1.0, 0.005)), col="firebrick")
  lines(seq(0.9, 1.0, 0.005), quantile(hanasaki_discharge, seq(0.9, 1.0, 0.005)), col="blue")
  abline(v=0.95)

}

basin_nr <- 3
loaded_basin_id <- basin_info[basin_nr,1]

no_res_discharge <- no_res_discharge_all[[paste0("X", loaded_basin_id)]]
hanasaki_discharge <- hanasaki_discharge_all[[paste0("X", loaded_basin_id)]]
schneider_discharge <- schneider_discharge_all[[paste0("X", loaded_basin_id)]]
date <- as.Date(schneider_discharge_all[["date"]], format="%Y-%m-%d")


"%>%" <- magrittr::"%>%"

df <- data.frame(list("Date"=date,
           "Sim"=schneider_discharge))

value1 <- df %>%
  dplyr::mutate(year = format(.data$Date, "%Y")) %>%
  dplyr::group_by(.data$year) %>%
  dplyr::group_modify(~ {
    quantile(.x$Sim, 0.99) %>%
      tibble::enframe()
  }) %>%
  dplyr::ungroup()

df <- data.frame(list("Date"=date,
                      "Sim"=no_res_discharge))

value<- df %>%
  dplyr::mutate(year = format(.data$Date, "%Y")) %>%
  dplyr::group_by(.data$year) %>%
  dplyr::group_modify(~ {
    quantile(.x$Sim, 0.99) %>%
      tibble::enframe()
  }) %>%
  dplyr::ungroup()

plot(value$year, value$value, col="black")
lines(value$year, value1$value, col="red")

observed_discharge <- Q.read_grdc(

  loaded_basin_id,
  NULL,
  cont = "na",
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

data_all %>%
  tidyr::pivot_longer(., cols= names_of_simtype) %>%
  ggplot(., aes(value, col=name)) +
  stat_ecdf(geom = "step") +
  scale_color_manual(values=values) +
  scale_x_continuous(trans='log10') +
  geom_hline(yintercept=0.95, linetype="solid",
             color = "firebrick", size=0.8) +
  theme_bw()

# max_reservoir <- which(loaded_basin@G_RESAREA == max(loaded_basin@G_RESAREA))
# loaded_basin@G_STORAGE_CAPACITY[max_reservoir] # 0.1195
# loaded_basin@G_MEAN_INFLOW[max_reservoir] #0.01552473
# loaded_basin@G_BANKFULL[max_reservoir] #36.26392
# loaded_basin@G_MEAN_INFLOW[max_reservoir] * 1000000000. / (12*24*3600) #14.97
#
# loaded_basin@G_RESAREA
# loaded_basin@G_GLOWET
# loaded_basin@routeOrder

LWD = 2
from <- 11*365+290 #12*365+100 #11*365+290
to <- 12*365-55 #from + 60 #12*365+55

from <- 11*365
to = 14*365
png(sprintf(target, names(basin_list)[[basin_nr]]), res=300, units="cm", width=20, height=15)
plot(data_all$Date[from:to],
     data_all$`Observed discharge`[from:to], type="l", lwd=LWD,
     ylab="Discharge [m3/s]",
     xlab="",
     ylim=c(0, 2000))

lines(data_all$Date[from:to],
      data_all$`Reservoirs as lakes`[from:to], col=colors_to_use[1], lwd=LWD)

lines(data_all$Date[from:to],
      data_all$`Reservoir algorithm (V1)`[from:to], col=colors_to_use[2], lwd=LWD)
lines(data_all$Date[from:to],
      data_all$`Reservoir algorithm (V2)`[from:to], col=colors_to_use[3], lwd=LWD)
#abline(h=loaded_basin@G_BANKFULL[max_reservoir], col="firebrick")

legend("topright", legend=c("Obs. Discharge","Reservoir as lakes",
                           "Reservoir algorithm (V1)", "Reservoir algorithm (V2)"),
       col=c("black", colors_to_use), cex=0.8, lwd=2, lty=1)

dev.off()

