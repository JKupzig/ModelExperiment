
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

fig <- plotly::plot_ly(storage, x = ~x, y=~as_lakes*1000^3 / (24*3600),  mode = 'lines', type = 'scatter', name = 'as_lakes',
                       line = list(color = grDevices::adjustcolor(datylon_map[1], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~schneider*1000^3 / (24*3600), name = 'schneider',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[5], alpha.f = 0.7)))
fig <- fig %>% plotly::add_trace(y = ~hanasaki*1000^3 / (24*3600), name = 'hanasaki',mode = 'lines',
                                 line = list(color = grDevices::adjustcolor(datylon_map[4], alpha.f = 0.7)))
print(fig)

dev.off()
boxplot((storage$schneider - storage$as_lakes)*1000^3 / (24*3600))
boxplot((data$schneider - data$aslakes))



par(mfrow = c(2, 2))
for (plot_nr in c(1,2,3,4)){
  inflow <- c(1,2,15,8,5,9,0,0,0,0)
  if (plot_nr <= 2){
    inflow <- c(0,10,10,10,0,0,0,0,0,0)
    #inflow <- c(11,12,25,18,15,19,0,0,0,0)
  }

  plot(x, c(inflow), type="l", xlim=c(0, length(inflow)), main=sprintf("K=%f", k_value))

  for (k_value in c(seq(0.06, 0.6, 0.02), 10/86.4)){
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

    if (plot_nr %in% c(1,3)){
      if (k_value == 10./86.4){
        lines(x_1, line_outflow_original, col="firebrick", lwd=3)
        if (plot_nr <= 2){
          original <- line_outflow_original
        } else {
          original_v2 <- line_outflow_original
        }
      }
      lines(x_1, line_outflow_original, col="orange")
    } else {
      if (k_value == 10./86.4){
        lines(x_1, line_outflow, col="firebrick", lwd=3)
        if (plot_nr <= 2){
          new <- line_outflow
        } else {
          new_v2 <- line_outflow
        }
      }
      lines(x_1, line_outflow, col="cornflowerblue")
    }
  }
}


max(new)
max(original)

max(new_v2)
max(original_v2)

source("./src/helper/read_data.r")
behavioural <- read_kge_and_define_good_basins(min_kge = 0.4, max_kge = NULL)
attributes <- read.table("./data/reduced_basin_attributes.txt", sep="\t", header=T)
attributes$grdc_ids <- paste0("X", attributes$grdc_ids)
basins_with_res <- attributes[attributes$reservoir_area > 0, ]
bheavioural_basins_with_res <-  basins_with_res[basins_with_res$grdc_ids %in% behavioural$behavioural,]
