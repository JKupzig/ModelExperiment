rm(list=ls())

source("./src/helper/read_data.r")

MIN_QUAL <- 0.4
MAX_QUAL <- NULL
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)

ROOT_100 <- "./data/evaluation/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_100_DISCHARGE <- "./data/evaluation/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"
ROOT_INCREASE_BF <- "./data/OUTPUT_UNCERTAINTY_M11/STATION_DISCHARGE_DAILY_uncertainty2_wetlStorage100.OUT"
ROOT_DECREASE_BF <- "./data/OUTPUT_UNCERTAINTY_M11/STATION_DISCHARGE_DAILY_uncertainty3_wetlStorage100.OUT"


#find basin with increase and decrease in timing
cal_results_m8 <- read.table(sprintf(ROOT_100, 8), sep="\t", header=T)
cal_results_m11 <- read.table(sprintf(ROOT_100, 11), sep="\t", header=T)
cal_results_m8 <- cal_results_m8[cal_results_m8$station %in% behavioural_basins$behavioural,]
cal_results_m11 <- cal_results_m11[cal_results_m11$station %in% behavioural_basins$behavioural,]
delta <- cal_results_m8$r_val - cal_results_m11$r_val
good <- cal_results_m8$station[delta > 0.05][1] #decreased timing


#select simulated discharge
discharge_m8 <- read.table(sprintf(ROOT_100_DISCHARGE, 8), sep="\t", header=T)
discharge_m11 <- read.table(sprintf(ROOT_100_DISCHARGE, 11), sep="\t", header=T)
selected_discharge_m8 <- discharge_m8[,names(discharge_m8) %in% c(good, "date")]
selected_discharge_m11 <- discharge_m11[,names(discharge_m11) %in% c(good, "date")]

#read grdc discharge
observed_good <- WaterGAPLite::Q.read_grdc(
  substr(good, 2, 10),
  NULL,
  "na",
  start=as.Date(min(selected_discharge_m8$date)),
  end=as.Date(max(selected_discharge_m8$date)),
  use_folder=ROOT_GRDC)


# better performing basins
correlation_static <- c()
correlation_variable <- c()
for (period_id in seq(1,180,1)){
  period <- (31*period_id):(31*period_id+72)
  static <- selected_discharge_m8[[good]][period]
  variable <- selected_discharge_m11[[good]][period]
  obs <- observed_good[period,]
  date <- selected_discharge_m11$date[period]
  correlation_static <- c(correlation_static, cor(static, obs$Value))
  correlation_variable <- c(correlation_variable, cor(variable, obs$Value))
}

id <- which((correlation_static-correlation_variable)==max(correlation_static-correlation_variable))
plot(seq(1,180,1), correlation_static, type="l", ylim=c(1, -1))
abline(v=id)
lines(seq(1,180,1), correlation_variable, col="firebrick")

cal_selected_discharge_m8 <- selected_discharge_m8[as.Date(selected_discharge_m8$date) < as.Date("1989-01-01"),]
val_selected_discharge_m8 <- selected_discharge_m8[as.Date(selected_discharge_m8$date) >= as.Date("1989-01-01"),]

cal_selected_discharge_m11 <- selected_discharge_m11[as.Date(selected_discharge_m11$date) < as.Date("1989-01-01"),]
val_selected_discharge_m11 <- selected_discharge_m11[as.Date(selected_discharge_m11$date) >= as.Date("1989-01-01"),]

observed_simperiod <- WaterGAPLite::Q.read_grdc(
  substr(good, 2, 10),
  NULL,
  "na",
  start=as.Date("1979-01-01"),
  end=as.Date("1994-12-31"),
  use_folder=ROOT_GRDC)
cal_selected_discharge_obs <- observed_simperiod[as.Date(observed_simperiod$Date) < as.Date("1989-01-01"),]
val_selected_discharge_obs <- observed_simperiod[as.Date(observed_simperiod$Date) >= as.Date("1989-01-01"),]
cal_selected_discharge_obs <- cal_selected_discharge_obs[format(cal_selected_discharge_obs$Date, "%m-%d") != "02-29",]
val_selected_discharge_obs <- val_selected_discharge_obs[format(val_selected_discharge_obs$Date, "%m-%d") != "02-29",]

cor(cal_selected_discharge_obs$Value, cal_selected_discharge_m11$X4145900, method = "pearson")
cor(cal_selected_discharge_obs$Value, cal_selected_discharge_m11$X4145900, method = "spearman")
cor(val_selected_discharge_obs$Value, val_selected_discharge_m11$X4145900, method = "pearson")
cor(val_selected_discharge_obs$Value, val_selected_discharge_m11$X4145900, method = "spearman")

cor(cal_selected_discharge_obs$Value, cal_selected_discharge_m8$X4145900, method = "pearson")
cor(cal_selected_discharge_obs$Value, cal_selected_discharge_m8$X4145900, method = "spearman")
cor(val_selected_discharge_obs$Value, val_selected_discharge_m8$X4145900, method = "pearson")
cor(val_selected_discharge_obs$Value, val_selected_discharge_m8$X4145900, method = "spearman")


period <- (id*31):(id*31+72)
static <- selected_discharge_m8[[good]][period]
variable <- selected_discharge_m11[[good]][period]
obs <- observed_good[period,]
date <- selected_discharge_m11$date[period]

discharge_increase_bf <- watergap3data::txt.read_station_discharge_daily(ROOT_INCREASE_BF)
discharge_decrease_bf <- watergap3data::txt.read_station_discharge_daily(ROOT_DECREASE_BF)

KM3_IN_M3 <- 1000 * 1000 * 1000
D_IN_S <- 60 * 60 * 24
MULTIPLIER <- KM3_IN_M3 / D_IN_S

sel_discharge_increase_bf <- discharge_increase_bf[[good]][period] * MULTIPLIER
sel_discharge_decrease_bf <-discharge_decrease_bf[[good]][period] * MULTIPLIER
min_a <- pmin(sel_discharge_increase_bf, sel_discharge_decrease_bf, variable)
max_a <- pmax(sel_discharge_increase_bf, sel_discharge_decrease_bf, variable)

ymin <- min(c(static, variable, sel_discharge_increase_bf, sel_discharge_decrease_bf, obs$Value))
ymax <- max(c(static, variable, sel_discharge_increase_bf, sel_discharge_decrease_bf, obs$Value))

png("./plots/variable_flow_velocity.png", width=25, height=18, units="cm", res=300)
plot(as.Date(date), static,
     type="l", col="cornflowerblue", ylim=c(ymin, ymax), ylab="Discharge (m3/s)", xlab="")
lines(as.Date(date), variable, col="firebrick")
polygon(c(as.Date(date), rev(as.Date(date))), c(max_a ,rev(min_a)),
        col = grDevices::adjustcolor( "firebrick", alpha.f = 0.5), border=FALSE)


lines(as.Date(obs$Date), obs$Value, col="black")
legend("topright", col=c("firebrick", "cornflowerblue", "black"), lty=1,
       legend=c("Discharge simulated (variable)",
                "Discharge simulated (static)", "Discharge observed"))
dev.off()

cor(static, obs$Value, method = "pearson")
cor(variable, obs$Value, method = "pearson")

my_data <- data.frame(list("static"=static, "variable"=variable, "obs"=obs$Value))
ggpubr::ggscatter(my_data, x = "obs", y = "static",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "observed", ylab = "simulated (static)")
ggpubr::ggscatter(my_data, x = "obs", y = "variable",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "observed", ylab = "simulated (variable)")


#read grdc discharge
observed_good <- WaterGAPLite::Q.read_grdc(
  substr(good, 2, 10),
  NULL,
  "na",
  start=as.Date("1961-01-01"),
  end=as.Date("2016-12-31"),
  use_folder=ROOT_GRDC)
mean(observed_good$Value)
min(observed_good$Value)
max(observed_good$Value)

observed_good$year <- format(observed_good$Date, "%Y")
observed_good_yearly <- aggregate(observed_good$Value,
                                  by = list(observed_good$year), max)

fitted_gev <- fExtremes::gevFit(observed_good_yearly$x)
ks.test(observed_good_yearly$x,
        fExtremes::rgev(10000,
                        xi=fitted_gev@fit$par.ests[1],
                        mu=fitted_gev@fit$par.ests[2],
                        beta=fitted_gev@fit$par.ests[3]))
fExtremes::qgev(1/(1.5),
                xi=fitted_gev@fit$par.ests[1],
                mu=fitted_gev@fit$par.ests[2],
                beta=fitted_gev@fit$par.ests[3],
                lower.tail=F)

fExtremes::qgev(0.1,
                xi=fitted_gev@fit$par.ests[1],
                mu=fitted_gev@fit$par.ests[2],
                beta=fitted_gev@fit$par.ests[3],
                lower.tail=F)

fExtremes::qgev(0.01,
                xi=fitted_gev@fit$par.ests[1],
                mu=fitted_gev@fit$par.ests[2],
                beta=fitted_gev@fit$par.ests[3],
                lower.tail=F)

#get bankfull flow used in simulation
bf <- watergap3data::unf.readunf("./data/G_BANKFULL.UNF0", "na")
station_info <- watergap3data::txt.read_station_list("./data/STATION_LIST.OUT")
cell_id <- station_info$cell_number[paste0("X", station_info$stations) == good]
bf[as.integer(cell_id)] #1447.501
