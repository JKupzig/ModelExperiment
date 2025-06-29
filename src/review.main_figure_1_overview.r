
source("./src/helper/read_data.r")
METRIC <- "KGE_val"
snow <- list(c(12,13), c(14, 15), c(16, 17), c(18, 19), c(8,9), c(11, 10))
reservoir <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))
river <- list(c(12, 14), c(13, 15), c(16, 18), c(17, 19), c(8, 11), c(9, 10))

reservoir2 <- list(c(8,16), c(9, 17), c(11, 18), c(10, 19))
reservoir1 <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))

MIN_X = -0.2
MAX_X = 0.2
CEX = 0.6

ROOT_ATTRIBUTES <- "./data/basin_attributes.txt"
ROOT_BENCHMARK <- "./data/cal_result_benchmarks_model_m%i_wetlStorage100.txt"
ROOT_SIMULATED <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"
ROOT_GRDC <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten)"
attributes <- read.table(ROOT_ATTRIBUTES, sep = "\t", header = TRUE)
behavioural_set <- read_kge_and_define_good_basins(min_kge = 0.4, max_kge = NULL)

runs_to_analyse <- list("snow"=snow,
                   "reservoir" = reservoir,
                   "reservoir1" = reservoir1,
                   "river"= river)

for (name in names(runs_to_analyse)){

  results <- list()
  break_points <- NULL

  for (type in c("behavioural", "all")){
    run_to_analyse <- runs_to_analyse[[name]]

    for (run in seq(1,length(run_to_analyse))){

      reference_discharge_all <- read.table(sprintf(ROOT_SIMULATED, run_to_analyse [[run]][1]), sep = "\t", header = TRUE)
      comparison_discharge_all <- read.table(sprintf(ROOT_SIMULATED, run_to_analyse[[run]][2]), sep = "\t", header = TRUE)

      cal_results_reference <- read.table(sprintf(ROOT_BENCHMARK, run_to_analyse[[run]][1]), sep = "\t", header = TRUE)
      cal_results_comparison <- read.table(sprintf(ROOT_BENCHMARK, run_to_analyse[[run]][2]), sep = "\t", header = TRUE)

      cal_results <- data.frame(
        basin_id=cal_results_reference$station,
        reference=cal_results_reference[[METRIC]],
        comparison=cal_results_comparison[[METRIC]],
        delta = cal_results_comparison[[METRIC]] - cal_results_reference[[METRIC]]
      )

      data <- merge(cal_results, attributes, by.x="basin_id", by.y="grdc_ids", how="left")

      if (name == "snow"){
        sensitive_data <- data[
          (data$mean_precipitation_as_snow > 0) &
            (data$localWetlands > 0),]
      } else if (name == "reservoir") {
        sensitive_data <- data[data$reservoir_area > 0,]
      } else {
        sensitive_data <- data
      }

      if (type == "behavioural"){
        sensitive_data <- sensitive_data[sensitive_data$basin_id %in% behavioural_set$behavioural,]
      }

      if (is.null(break_points)){
        break_points <-seq(-20, 20, by=0.01)
      }
      closest_to_zero <- which(abs(break_points) == min(abs(break_points)))
      break_points[closest_to_zero] <- 0
      data_transform = cut(sensitive_data$delta, break_points, right=FALSE)
      freq_table = table(data_transform)

      cumulative_frequency <- c(0, cumsum(freq_table))
      cumulative_frequency_percent <- cumulative_frequency / nrow(sensitive_data) * 100

      results[[type]][[run]] <- cumulative_frequency_percent
    }
  }


  x <- break_points
  y <- results[["all"]]

  png(sprintf("./plots/review.figure3_%s.png", name), units="cm", bg="white", res=600, width=8, height=8)
  par(mgp=c(1.5,0.5,0),mar=c(2.5,2,0,0)+0.1)
  plot(x, y[[1]] , type="l",
       xlim=c(MIN_X, MAX_X),
       xlab= expression(Delta*KGE),
       ylab="Cumulative frequency [%]", cex.axis=CEX, cex.lab=CEX)
  grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = 1)

  abline(v = 0, col="firebrick", lwd=1, lty=1)

  if (length(y) == 4){
    y[[5]] <- y[[1]]
    y[[6]] <- y[[1]]
  }
  y_min <- pmin(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], y[[6]])
  y_max <- pmax(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], y[[6]])

  polygon(c(x, rev(x)), c(y_min, rev(y_max)),
          col = adjustcolor("darkgrey",alpha.f=0.5), border=NA)

  lines(x, y[[1]], lwd=1.2)

  zero <- which(abs(x) == min(abs(x)))
  y_pos <- y[[1]][zero]+2
  arrows(0, y[[1]][zero], MIN_X, y[[1]][zero], angle=45, length=0.03)
  text(x = MIN_X + 0.05, y = , # Coordinates
       label = sprintf("%.1f %%", y[[1]][zero]), cex=CEX)

  y <- results[["behavioural"]]
  y_min <- pmin(y[[1]], y[[2]], y[[3]], y[[4]])
  y_max <- pmax(y[[1]], y[[2]], y[[3]], y[[4]])
  polygon(c(x, rev(x)), c(y_min, rev(y_max)),
          col = adjustcolor("cornflowerblue", alpha.f=0.5), border=NA)
  lines(x, y[[1]], lwd=2, col=adjustcolor("cornflowerblue",alpha.f=0.5))

  text(x = MIN_X + 0.05, y = y_pos+10, # Coordinates
       label = sprintf("(%.1f %%)", y[[1]][zero]), col="cornflowerblue", cex=CEX)

  dev.off()
}

