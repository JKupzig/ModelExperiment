library(dplyr)
library(ggplot2)

library(WaterGAPLite)

source(r"(src\helper\io_adjustment.r)", chdir = TRUE)
source(r"(src\helper\modify_run.r)", chdir = TRUE)

#### Global variables #####
MODEL_INPUT_PATH <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)"
DISCHARGE_DATA_PATH <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"
START <- "01.01.1980"
END <- "31.12.1989"

settings_with_snow_in_wetlands <- init.settings(snow_in_wetlands = "on")
settings_no_snow_in_wetlands <- init.settings(snow_in_wetlands = "off")
warmup_years <- 10

#### Example Basin #####

example_basin <- list("grdc_no" = 4214420,
                      "corlat" = 52.45806315,
                      "corLong" = -90.20837225,
                      "cont" = "na", "name" = "canada")

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
  sim_start = START, sim_end =END,
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


#### Prepare Runs #####


less_snow <- list(
  "snow_threshold" = -10,
  "max_degree_days" = 20,
  "loc_storageFactor" = 1
)

more_snow <- list(
  "snow_threshold" = 0,
  "max_degree_days" = 5,
  "loc_storageFactor" = 1
)

ref_only_storage <- list(
  "loc_storageFactor" = 1
)

run_less_snow <- modify_run(basin.run, less_snow)
run_more_snow <- modify_run(basin.run, more_snow)
run_standard_run_faster_storage <- modify_run(basin.run, ref_only_storage)
run_standard_run <- basin.run

#### Simulation Runs #####

standard_run <- runModel(
  SimPeriod = run_standard_run$SimPeriod,
  ListConst = run_standard_run,
  Settings = settings_no_snow_in_wetlands,
  nYears = warmup_years
)
discharge_standard_run <- Q.convert_mmday_m3s(
  standard_run$routing$River$Discharge, sum(basin@GAREA)
)

standard_run_fast <- runModel(
  SimPeriod = run_standard_run_faster_storage$SimPeriod,
  ListConst = run_standard_run_faster_storage,
  Settings = settings_no_snow_in_wetlands,
  nYears = warmup_years
)
discharge_standard_run_fast <- Q.convert_mmday_m3s(
  standard_run_fast$routing$River$Discharge, sum(basin@GAREA)
)


less_snow <- runModel(
  SimPeriod = run_less_snow$SimPeriod,
  ListConst = run_less_snow,
  Settings = settings_with_snow_in_wetlands,
  nYears = warmup_years
)
discharge_ref_less_snow <- Q.convert_mmday_m3s(
  less_snow$routing$River$Discharge, sum(basin@GAREA)
)

more_snow <- runModel(
  SimPeriod = run_more_snow$SimPeriod,
  ListConst = run_more_snow,
  Settings = settings_with_snow_in_wetlands,
  nYears = warmup_years
)
discharge_ref_more_snow <- Q.convert_mmday_m3s(
  more_snow$routing$River$Discharge, sum(basin@GAREA)
)

#### Plotting #####

png("./plots/snow_storage_wetlands.png")
x_axis <- 1:length(basin.run$SimPeriod)

plot(x_axis, apply(more_snow$routing$locWetland$Snow, 1, mean),
    type = "l", col = "cornflowerblue", main = "snow storage")
lines(x_axis, apply(less_snow$routing$locWetland$Snow, 1, mean),
      col = "maroon")
lines(x_axis, apply(standard_run$routing$locWetland$Snow, 1, mean),
      col = "forestgreen")
legend(1, 230, legend=c("higher snow storage", "lower snow storage", "no snow storage"),
       col = c("cornflowerblue", "maroon", "forestgreen"), lty = 1, cex = 0.8)
dev.off()

lwd <- 1.5

png("./plots/discharges_storage_wetlands.png", width=15, height=12, units="cm", res=300)
plot(qobs$Date, qobs$Value, type = "l", ylim = c(0, 600),
     ylab = ("Q in m3/s"), xlab = "", col = "black", lwd=2)

lines(qobs$Date, discharge_ref_less_snow, col = "maroon", lwd = lwd)
lines(qobs$Date, discharge_ref_more_snow, col = "cornflowerblue", lwd = lwd)
lines(qobs$Date, discharge_standard_run, col = "forestgreen", lwd = lwd)
lines(qobs$Date, discharge_standard_run_fast, col = "green", lwd = lwd)


legend("topleft",
       c("observed", "higher snow storage", "lower snow storage", "standard run", "standard run (fast)"),
       col = c("black", "maroon", "cornflowerblue", "forestgreen", "green"),
       lty = 1,
       horiz = FALSE, cex = 0.8, bty = "n")
dev.off()

#### KGE evaluation #####

simulated_discharges <- list(
  "higher snow storage" = discharge_ref_less_snow,
  "lower snow storage" = discharge_ref_more_snow,
  "standard run" = discharge_standard_run,
  "standard run (fast)" = discharge_standard_run_fast)

table <- data.frame(matrix(nrow = length(names(simulated_discharges)),
                           ncol = 5))
row <- 1
for (sim in simulated_discharges){
  df_sim <- data.frame(Date = qobs$Date, Sim = sim)
  kge <- Q.calc_quality(df_obs=qobs, df_sim = df_sim, type = "KGE")
  table[row, 1] <- names(simulated_discharges)[row]
  table[row, 2] <- kge$a
  table[row, 3] <- kge$b
  table[row, 4] <- kge$r
  table[row, 5] <- kge$KGE
  row <- row + 1
}

colnames(table) <- c("sim_type", "a", "b", "r", "kge")
print(table)

### seasonal evaluation ####

complete_data <- data.frame(date = qobs$Date,
                            observed = qobs$Value,
                            simulated_discharges)

seasonal_data <- complete_data %>%
  mutate(month = format(date, "%m")) %>%
  group_by(month) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  select(!c("date")) %>%
  tidyr::pivot_longer(cols = -c("month")) %>%
  ggplot() +
  geom_line(aes(x = month, y = value, group = name, color=name)) +
  scale_colour_manual(
    values = c("blue", "#00AFBB", "#E7B800", "#FC4E07", "black")) +
  theme_bw()

  ggsave("./plots/seasonal_evaluation_snow_in_wetlands.png")

