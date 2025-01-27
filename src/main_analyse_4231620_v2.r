
rm(list=ls())
library(WaterGAPLite)
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
source("./src/helper/load_wgl.r")
source("./src/helper/color_ramps.r")

routing_formula_original <- function(storage, inflow, K=2){
  q_out <-  inflow * (1 - K * (1. - exp(-1./K ))) + storage * (1-exp(-1./K))
  new_storage = storage + inflow - q_out
  return(list("storage"=new_storage, "outflow"=q_out))
}

routing_formula_els <- function(former_outflow, inflow, storage, K=2){
  q_out <-  inflow * (1. - exp(-1./K )) + former_outflow * exp(-1./K)
  new_storage = storage + inflow - q_out
  return(list("storage"=new_storage, "outflow"=q_out))
}

routing_formula <- function(storages, inflow, approx="off", increase_inflow=NULL){
  riverVelocity <- 1 * (3600*24) / 1000 #1m/s in km/d = 86.4 km/d
  riverLength <- 10 # assumed 9 km
  K = riverLength / riverVelocity #d = km/(km/d)

  transportedVolumeRaster <- rep(0, length(storages))
  outflow <- c() #in last cell
  for (timestep in 1:length(inflow)){
    inflow_cell <- inflow[timestep]
    if (!is.null(increase_inflow)){
      inflow_cell <- inflow_cell + increase_inflow[timestep]
    }

    for (cell in 1:length(storages)) {
      old_storage = storages[cell];
      new_storage <- old_storage
      if (approx == "on"){
        transportedVolume <- inflow_cell * K + old_storage
        new_storage <- old_storage + inflow_cell - transportedVolume
      } else if (approx=="original"){
        former_outflow <- transportedVolumeRaster[cell]
        transportedVolume <-  inflow_cell * (1. - exp(-1./K )) + former_outflow * exp(-1./K)
        transportedVolumeRaster[cell] <- transportedVolume
      } else {
        out <- routing_formula_original(old_storage, inflow_cell, K=K)
        transportedVolume <-  out$outflow
        new_storage = out$storage
      }

      inflow_cell <- transportedVolume
      storages[cell] <- new_storage
    }
    outflow <- c(outflow, inflow_cell)
  }

  return(outflow)
}


hline <- function(y = 0, color = "black") {

  list(

    type = "line",

    x0 = 0,

    x1 = 1,

    xref = "paper",

    y0 = y,

    y1 = y,

    line = list(color = color)

  )

}

target = "./plots/weird_basin_reservoir_%s.png"
files_to_plot_availability <- list.files(ROOT, pattern="*availablity.csv")
ROOT <- "./data/evaluation/reservoirs"
START <- as.Date("01.01.1986", format="%d.%m.%Y")
END <- as.Date("31.12.1994", format="%d.%m.%Y")

basin_info <- read.table("./data/weird_reservoir_basins.txt", sep=";", header=TRUE)
colors_to_use <- datylon_map[c(1,4,7)]

basin_to_simulate <- basin_info[3,]
example_basin <- list("grdc_no" = basin_to_simulate$grdc_no,
                      "corLong" = basin_to_simulate$long,
                      "corlat" = basin_to_simulate$lat,
                      "cont" = basin_to_simulate$cont,
                      "name" = basin_to_simulate$name)

loaded_basin <- load_wgl_init_basin(example_basin)

# look at tobique river - is there storage with overflow upstream of the reservoir?
waterbodies <- (loaded_basin@G_GLOWET)
waterbodies[loaded_basin@G_LAKAREA > 0] <- -1
waterbodies[loaded_basin@G_RESAREA > 0] <- -2
raster::writeRaster(WaterGAPLite::basin.create_raster(waterbodies, loaded_basin),
                    "data/res_position_tobique.tiff", overwrite=T)

# now comparing streams:
no_res_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m8_wetlStorage100.txt",
                               sep="\t", header=T)
hanasaki_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m12_wetlStorage100.txt",
                                   sep="\t", header=T)
schneider_discharge_all <- read.table("./data/evaluation/cal_result_discharges_model_m16_wetlStorage100.txt",
                                      sep="\t", header=T)
schneider_discharge_all_18 <- read.table("./data/evaluation/cal_result_discharges_model_m18_wetlStorage100.txt",
                                   sep="\t", header=T)

period <- (7*365+1):(16*365)
basin_id <- "4231620"
basin <- paste0("X", basin_id)
no_res_discharge <- no_res_discharge_all[[basin]][period]
hanasaki_discharge <- hanasaki_discharge_all[[basin]][period]
schneider_discharge <- schneider_discharge_all[[basin]][period]
date <- as.Date(schneider_discharge_all[["date"]], format="%Y-%m-%d")[period]

observed_discharge <- Q.read_grdc(

  basin_id,
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
names(values) <- names_of_simtype
x <- as.POSIXct(hanasaki_discharge_all$date)[period]

data <- data.frame(x=x,
                   date=date,
                   aslakes=no_res_discharge,
                   hanasaki=hanasaki_discharge,
                   schneider=schneider_discharge,
                   obs=observed_discharge$Value[no_leap_year])

names(data) <- c("x", "Date", names_of_simtype)
data %>%
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



ylim <- c(min(c(data$`Reservoirs as lakes`, data$`Reservoir algorithm (V1)`,
                data$`Reservoir algorithm (V2)`, data$`Observed discharge`)),
          max(c(data$`Reservoirs as lakes`, data$`Reservoir algorithm (V1)`,
                data$`Reservoir algorithm (V2)`, data$`Observed discharge`))*1.2)


threshold_high_ref <- as.numeric(quantile(data$`Reservoirs as lakes`, 0.95))
threshold_high_schneider <- as.numeric(quantile(data$`Reservoir algorithm (V2)`, 0.95))

threshold_high <- stats::median(no_res_discharge_all[[basin]]) * 9
threshold_low <- mean(no_res_discharge_all[[basin]]) * 0.2

names(data) <- c("x", "Date", "aslakes", "hanasaki", "schneider", "obs")
fig <- plotly::plot_ly(data, x = ~x, y=~aslakes,  mode = 'lines', type = 'scatter', name = 'as_lakes',
                       line = list(color = grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~schneider, name = 'schneider',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[5], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~hanasaki, name = 'hanasaki',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[4], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~obs, name = 'obs',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7)))

fig <- fig %>%  plotly::layout(shapes = list(hline(threshold_high), hline(threshold_low),
                                             hline(threshold_high_ref, color=datylon_map[1]),
                                             hline(threshold_high_schneider, color=datylon_map[5])),
                               title = basin,
                               xaxis = list(title = ""),
                               yaxis = list (title = "Discharge"))

shapes = list(hline(threshold_high), hline(threshold_low),
              hline(threshold_high_ref, color=datylon_map[1]),
              hline(threshold_high_schneider, color=datylon_map[5]))


print(fig)

storage <- read.table(file.path(ROOT, "22_reservoirs_availablity.csv"), sep=";", header=T)

copied_storage <- storage
names(copied_storage) <- c("date", "res_as_lakes", "res_hanasaki", "res_schneider")
copied_storage$date <- as.Date(copied_storage$date, format="%Y-%m-%d")
copied_storage <- copied_storage[order(copied_storage$date),]
copied_storage <- copied_storage[copied_storage$date >= START &
                                   copied_storage$date <= END, ]
copied_storage$x <- as.POSIXct(copied_storage$date)

storage$date <- as.Date(storage$date, format="%Y-%m-%d")
storage <- storage[order(storage$date),]
storage <- storage[storage$date >= START &
                     storage$date <= END, ]
storage$x <- as.POSIXct(storage$date)


all <- cbind(copied_storage, data)
all$delta <- all$schneider - all$aslakes

all$res_delta <- (all$res_schneider - all$res_as_lakes)* 1000^3 / (24*3600)


fig <- plotly::plot_ly(all, x = ~x, y=~delta,  mode = 'lines', type = 'scatter', name = 'difference_outlet',
                      line = list(color = grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~res_delta, name = 'difference_reservoir',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[8], alpha.f = 0.7)))

print(fig)

fig <- plotly::plot_ly(storage, x = ~x, y=~as_lakes,  mode = 'lines', type = 'scatter', name = 'as_lakes',
                       line = list(color = grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~schneider, name = 'schneider',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[5], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~hanasaki, name = 'hanasaki',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[4], alpha.f = 0.7)))
print(fig)


# examine river routing
start_experiment <- as.Date("1991-04-20")
end_experiment <- as.Date("1991-06-01")
n_storages <- 10
storages <- rnorm(n_storages, 0.0001, 0.001)
schneider_storages <- storages

experiment_date <- storage$x[storage$x <= (end_experiment + 100) & data$Date >= start_experiment]
inflow_aslakes <- storage$as_lakes[storage$x <= end_experiment & data$Date >= start_experiment]
inflow_aslakes <- c(inflow_aslakes, rep(0, 100))
increase <- c(rep(0.04, 8), rep(0.001, 2), rep(0.06, 10), rep(0.002, 2), rep(0, 143-22))

outflow_aslakes <- routing_formula(storages, inflow_aslakes)
outflow_aslakes_incr <- routing_formula(storages, inflow_aslakes, increase_inflow=increase)
outflow_aslakes_orig <- routing_formula(storages, inflow_aslakes,approx="original")

inflow_schneider <- storage$schneider[storage$x <= end_experiment & data$Date >= start_experiment]
sim_schneider <-  data$schneider[data$x <= end_experiment + 100 & data$Date >= start_experiment]
sim_aslakes  <-  data$aslakes[data$x <= end_experiment + 100 & data$Date >= start_experiment]

inflow_schneider <- c(inflow_schneider, rep(0, 100))
outflow_schneider <- routing_formula(schneider_storages, inflow_schneider)
outflow_schneider_incr <- routing_formula(storages, inflow_schneider, increase_inflow=increase)
outflow_schneider_orig <- routing_formula(storages, inflow_schneider, approx="original")

plot(experiment_date, outflow_aslakes, type="l", col="pink", lwd=2, ylim=c(0.0, 0.007))
lines(experiment_date, outflow_aslakes_orig, col="orange")
lines(experiment_date, inflow_aslakes, col="firebrick")

lines(experiment_date, inflow_schneider, col="blue")
lines(experiment_date, outflow_schneider, col="blue")
lines(experiment_date, outflow_schneider_orig, col="cornflowerblue")
#lines(experiment_date, outflow_schneider_incr, col="forestgreen", lwd=2)


lines(experiment_date, sim_schneider/(1000^3)*3600*24*0.1, col="black")
lines(experiment_date, sim_aslakes/(1000^3)*3600*24*0.1, col="gray")

inflow <- c(1,2,15,8,5,9,0,0,0,0)
inflow <- c(0,10,10,10,0,0,0,0,0,0)
for (k_value in c(0.3)){
  storage_els <- 0
  former_outflow <- 0.
  line_storage_els <- c(storage_els)
  line_outflow <- c(former_outflow)
  for (day in 1:length(inflow)){
    out <- routing_formula_els(former_outflow, inflow[day], storage_els, K=k_value)
    former_outflow <- out$outflow
    storage_els <- out$storage
    line_storage_els <- c(line_storage_els, out$storage)
    line_outflow <- c(line_outflow, out$outflow)

  }

  storage_original <- 0
  former_outflow <- 0.0
  line_storage_original <- c(storage_original)
  line_outflow_original <- c(former_outflow)
  for (day in 1:length(inflow)){
    out <- routing_formula_original(storage_original, inflow[day], K=k_value)
    former_outflow <- out$outflow
    storage_original <- out$storage

    line_storage_original <- c(line_storage_original, out$storage)
    line_outflow_original <- c(line_outflow_original, out$outflow)
  }

  x = 1:length(inflow)
  x_1 = 0:length(inflow)
  plot(x, c(inflow), type="l", xlim=c(0, length(inflow)), main=sprintf("K=%f", k_value))
  #lines(x_1, line_storage_els, col="blue")
  lines(x_1, line_outflow, col="cornflowerblue")
  #lines(x_1, line_storage_original, col="firebrick")
  lines(x_1, line_outflow_original, col="orange")
}

max(line_outflow_original)
max(line_outflow)
