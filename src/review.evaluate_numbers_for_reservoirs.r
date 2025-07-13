# Figure 4

rm(list = ls())

library(dplyr)
library(ggplot2)

source("./src/helper/comparison.r")
source("./src/helper/read_data.r")
source("./src/helper/color_ramps.r")

obs_data <- readRDS("./data/SI_obs_ref_as_m8_100d_complete_validation_period.rds")
sim_data <- readRDS("./data/SI_original.rds")

END_CAL <- as.Date("1988-12-31")

file_pattern <- "./data/cal_result_discharges_model_m%i_wetlStorage100.txt"


set_names <- c("reservoir algorithm (V1)", "reservoir algorithm (V2)", "snow on wetlands", "variable flow velocity")

overall_results <- list()
for (name in set_names){

  print(name)
  if (name == "reservoir algorithm (V1)"){
    to_analyse <- list(c(8,12), c(9, 13), c(11, 14), c(10, 15))
    sensitive_basins <- get_sensitive_basins("reservoir")
  } else if (name == "reservoir algorithm (V2)") {
    to_analyse <- list(c(12,16), c(13, 17), c(14, 18), c(15, 19))
    sensitive_basins <- get_sensitive_basins("non-irrig")
  } else if (name == "snow on wetlands") {
    to_analyse <- list(c(8,9), c(12,13), c(14, 15), c(16, 17), c(18, 19), c(11, 10))
    sensitive_basins <- get_sensitive_basins("snow")
  } else {
    to_analyse <- list(c(8, 11), c(12, 14), c(13, 15), c(16, 18), c(17, 19), c(9, 10))
    sensitive_basins <- get_sensitive_basins("river")
  }

  ref_r2 <- list()
  comp_r2 <- list()

  for (entry in seq_along(to_analyse)){

    m1 <- to_analyse[[entry]]
    ref_sim <- read.csv(sprintf(file_pattern, m1[1]), sep="\t")
    ref_sim_val <- ref_sim[ref_sim$date > END_CAL,]
    ref_mean <- apply(ref_sim_val[,colnames(ref_sim_val) %in% sensitive_basins], 2, mean)
    ref_sd <- apply(ref_sim_val[, colnames(ref_sim_val) %in% sensitive_basins], 2, sd)

    comp_sim <- read.csv(sprintf(file_pattern, m1[2]), sep="\t")
    comp_sim_val <- comp_sim[comp_sim$date > END_CAL,]
    com_mean <- apply(comp_sim_val[, colnames(comp_sim_val) %in% sensitive_basins], 2, mean)
    com_sd <- apply(comp_sim_val[, colnames(comp_sim_val) %in% sensitive_basins], 2, sd)

    obs_mean <- obs_data[1,11,dimnames(obs_data)[[3]] %in% sensitive_basins]
    obs_sd <- obs_data[1,12,dimnames(obs_data)[[3]] %in% sensitive_basins]


    obs_values <- data.frame(basin = names(obs_mean),
                            mean = obs_mean,
                            sd = obs_sd)
    obs_values <- obs_values[order(obs_values$basin), ]

    comp_values <- data.frame(basin = names(com_mean),
                              mean = com_mean,
                              sd = com_sd)
    comp_values <- comp_values[order(comp_values$basin), ]

    ref_values <- data.frame(basin = names(ref_mean),
                              mean = ref_mean,
                              sd = ref_sd)
    ref_values <- ref_values[order(ref_values$basin), ]

    # mean plot
    plot(obs_values$mean, ref_values$mean, main="mean", log='xy', pch=19, col=scales::alpha("black", 0.8),
         ylab="simulation", xlab="observation")
    points(obs_values$mean, comp_values$mean, col=scales::alpha("cornflowerblue", 0.8), pch=19)
    abline(a=0, b=1, col="darkgrey")
    legend("bottomright", c("reference", "comparison"), pch=c(19,19), col=c("black", "cornflowerblue"))

    ref.lm = lm(ref_values$mean ~ obs_values$mean)
    comp.lm = lm(comp_values$mean ~ obs_values$mean)
    mtext(sprintf("%.2f -> %.2f", summary(ref.lm)$r.squared, summary(comp.lm)$r.squared) , 3, line=-2)

    ref_r2[["mean"]] <- c(ref_r2[["mean"]], summary(ref.lm)$r.squared)
    comp_r2[["mean"]] <- c(comp_r2[["mean"]], summary(comp.lm)$r.squared)

    # sd plot
    plot(obs_values$sd, ref_values$sd, main="sd", log='xy', pch=19, col=scales::alpha("black", 0.8),
         ylab="simulation", xlab="observation")
    points(obs_values$sd, comp_values$sd, col=scales::alpha("cornflowerblue", 0.8), pch=19)
    abline(a=0, b=1, col="darkgrey")
    legend("bottomright", c("reference", "comparison"), pch=c(19,19), col=c("black", "cornflowerblue"))

    ref.lm = lm(ref_values$sd ~ obs_values$sd)
    comp.lm = lm(comp_values$sd ~ obs_values$sd)
    mtext(sprintf("%.2f -> %.2f", summary(ref.lm)$r.squared, summary(comp.lm)$r.squared) , 3, line=-2)

    ref_r2[["sd"]] <- c(ref_r2[["sd"]], summary(ref.lm)$r.squared)
    comp_r2[["sd"]] <- c(comp_r2[["sd"]], summary(comp.lm)$r.squared)


    si_names <- c("mgn_h", "mgn_l", "frq_l", "frq_h", "dur_l", "dur_h")
    obs_data_si <- obs_data[1,dimnames(obs_data)[[2]] %in% si_names ,dimnames(obs_data)[[3]] %in% sensitive_basins]
    sim_data_si <- sim_data[dimnames(sim_data)[[1]] %in% sprintf("%i_wetlStorage100", m1)
                            ,dimnames(sim_data)[[2]] %in% si_names,
                            dimnames(sim_data)[[3]] %in% sensitive_basins]


    for (col in si_names){

      obs <- obs_data_si[dimnames(obs_data_si)[[1]] == col,]
      comp <- sim_data_si[1, dimnames(sim_data_si)[[2]] == col,]
      ref <- sim_data_si[2, dimnames(sim_data_si)[[2]] == col,]

      reg_ref<-lm(obs ~ comp)
      reg_comp<-lm(obs ~ ref)

      plot(obs, ref, main=col, log='xy', pch=19, col=scales::alpha("black", 0.8),
           ylab="simulation", xlab="observation")
      points(obs, comp, col=scales::alpha("cornflowerblue", 0.8), pch=19)
      abline(a=0, b=1, col="darkgrey")
      legend("bottomright", c("reference", "comparison"), pch=c(19,19), col=c("black", "cornflowerblue"))

      mtext(sprintf("%.2f -> %.2f", summary(reg_ref)$r.squared, summary(reg_comp)$r.squared) , 3, line=-2)

      ref_r2[[col]] <- c(ref_r2[[col]], summary(reg_ref)$r.squared)
      comp_r2[[col]] <- c(comp_r2[[col]], summary(reg_comp)$r.squared)

    }
  }

  ref_df <- data.frame(ref_r2)
  comp_df <- data.frame(comp_r2)

  ref_median <- apply(ref_df, 2, median)
  ref_min <- apply(ref_df, 2, min)
  ref_max <- apply(ref_df, 2, max)

  comp_median <- apply(comp_df, 2, median)
  comp_min <- apply(comp_df, 2, min)
  comp_max <- apply(comp_df, 2, max)

  overall_results[[name]][["ref"]][["median"]] <- ref_median
  overall_results[[name]][["ref"]][["min"]] <- ref_min
  overall_results[[name]][["ref"]][["max"]] <- ref_max

  overall_results[[name]][["comp"]][["median"]] <- comp_median
  overall_results[[name]][["comp"]][["min"]] <- comp_min
  overall_results[[name]][["comp"]][["max"]] <- comp_max

}

round(data.frame(overall_results[["snow on wetlands"]][["comp"]]) - data.frame(overall_results[["snow on wetlands"]][["ref"]]),2)

round(data.frame(overall_results[["reservoir algorithm (V1)"]][["comp"]]) - data.frame(overall_results[["reservoir algorithm (V1)"]][["ref"]]),2)

round(data.frame(overall_results[["reservoir algorithm (V2)"]][["comp"]]) - data.frame(overall_results[["reservoir algorithm (V2)"]][["ref"]]),2)

round(data.frame(overall_results[["variable flow velocity"]][["comp"]]) - data.frame(overall_results[["variable flow velocity"]][["ref"]]),2)

