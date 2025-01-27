
read_kge_and_define_good_basins <- function(min_kge = 0.4, max_kge = NULL) {
  ROOT <- "./data"
  files_to_read <- list.files(ROOT, pattern="cal_result_benchmarks_model_.*wetlStorage100*")

  all_kges <- NULL
  for (file_to_read in files_to_read) {
    cal_result <- read.table(file.path(ROOT, file_to_read), sep = "\t", header = TRUE)
    cal_result$variant <- substr(file_to_read, 23, 40)
    all_kges <- rbind(all_kges,  cal_result)
  }

  basins <- c(unique(all_kges$station))
  basins_to_return <- c()

  if (!is.null(min_kge)) {
    basins_to_return <- unique(all_kges$station[all_kges$KGE_cal >= min_kge])
  }

  if (!is.null(max_kge)) {
    for (basin in basins) {
      best_kge <- max(all_kges$KGE_cal[all_kges$station == basin])
      if (best_kge < max_kge) {
        basins_to_return <- c(basins_to_return, basin)
      }
    }
  }

  if (length(basins_to_return) > 0) {
    basins <- basins_to_return
  }

  return(list("behavioural" = basins, "data" = all_kges))
}


read_benchmarks_all <- function(column = "kge_val") {
  FOLDER <- "./data"
  ROOT_100 <- file.path(FOLDER, "cal_result_benchmarks_model_m%i_wetlStorage100.txt")
  models <- seq(8, 19, 1)

  cal_results <- read.table(sprintf(ROOT_100, models[1]), sep = "\t", header = TRUE)
  cal_results <- data.frame(station=cal_results[,GROUP_NAMES(cal_results) %in% c("station")])

  for (model in models){
    model_result <- read.table(sprintf(ROOT_100, model), sep = "\t", header = TRUE)
    cal_results[[sprintf("model_m%i_100", model)]] <- model_result[[column]]
  }

  return(cal_results)
}

read_benchmarks_uncertainty <- function(column = "kge_val", uncertainty_type = 1){
  ROOT <- "./data/cal_result_model_m%i_uncertainty.rds"
  COLUMN <- sprintf("uncertainty%i_wetlStorage100", uncertainty_type)
  results <- NULL
  models <- seq(8, 19, 1)
  for (model_type in models){
    cal_results <- readRDS(sprintf(ROOT, model_type))
    idx_dim_1 <- which(lapply(dimnames(cal_results)[[1]], function(x) grepl(x, column)) == TRUE)
    idx_dim_2 <- which(lapply(dimnames(cal_results)[[2]], function(x) grepl(x, column, ignore.case = T)) == TRUE)[1]
    idx_dim_3 <- which(dimnames(cal_results)[[3]] %in% c(COLUMN))

    cal_results_wanted <- cal_results[idx_dim_1, idx_dim_2, idx_dim_3, ]
    cal_results_wanted_df <- as.data.frame(t(cal_results_wanted))
    GROUP_NAMES(cal_results_wanted_df) <- c(sprintf("model_m%i_100", model_type))

     if (is.null(results)) {
      results <- cal_results_wanted_df
    } else {
      results <- cbind(results, cal_results_wanted_df)
    }
    results$station <- rownames(results)
  }

  return(results)
}
