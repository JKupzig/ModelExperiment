library(dplyr)
library(ggplot2)

library(WaterGAPLite)

#### Global variables #####
MODEL_INPUT_PATH <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)"
DISCHARGE_DATA_PATH <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"
START <- "01.01.1992"
END <- "31.12.2001"

settings <- init.settings()
warmup_years <- 10

#### Example Basin #####


example_basin <- list("grdc_no" = 3650470,
                      "corlat" = -5.29168695,
                      "corLong" = -42.70864205,
                      "cont" = "sa", "name" = "brazil")

basin <- init.model(
  grdc_number = example_basin$grdc_no,
  lat = example_basin$corlat,
  long = example_basin$corLong,
  cont = example_basin$cont,
  base = MODEL_INPUT_PATH
)

basin.climate <- init.climate(
  basin,
  start = START, end = END
  )

basin.water_use <- init.wateruse(
  basin,
  sim_start = START, sim_end = END,
  wateruse_setting = 0
  )

basin.run <- basin.prepare_run(basin, basin.climate, basin.water_use)

observed_discharge <- Q.read_grdc(
  example_basin$grdc_no,
  NULL,
  cont = example_basin$cont,
  start = START,
  end = END,
  use_folder = DISCHARGE_DATA_PATH
)

observed_discharge_mm <- Q.convert_m3s_mmday(observed_discharge$Value, sum(basin.run$GAREA))

run_standard_run <- basin.run

standard_run <- runModel(
  SimPeriod = run_standard_run$SimPeriod,
  ListConst = run_standard_run,
  Settings = settings,
  nYears = warmup_years
)
discharge_standard_run <- Q.convert_mmday_m3s(
  standard_run$routing$River$Discharge, sum(basin@GAREA)
)

lwd <- 1.5
y_max <- max(max(observed_discharge$Value), max(discharge_standard_run)) * 1.1
plot(observed_discharge$Date, observed_discharge$Value, type = "l",
     ylab = ("Q in m3/s"), xlab = "", col = "black", lwd = 2,
     ylim = c(0, y_max))

lines(observed_discharge$Date, discharge_standard_run, col = "maroon", lwd = lwd)
