compare_models_reservoirs <- function(cal_results,
                                      reference_column,
                                      to_compare_column_1,
                                      to_compare_column_2,
                                      label_to_use,
                                      min_quality = NULL){


  station_sufficient_quality <- cal_results$station

  if (!is.null(min_quality)) {
    station_sufficient_quality <- cal_results$station[
      cal_results[reference_column] > min_quality |
        cal_results[to_compare_column_1] > min_quality |
        cal_results[to_compare_column_2] > min_quality]
  }


  selected_stations <- cal_results$station %in% station_sufficient_quality
  cal_results_intersting <- cal_results[selected_stations,]

  delta_1 <- cal_results_intersting[[to_compare_column_1]]  -
    cal_results_intersting[[reference_column]]

  delta_2 <- cal_results_intersting[[to_compare_column_2]]  -
    cal_results_intersting[[reference_column]]

  data_1 <- data.frame("value"= delta_1,
                       "label"= paste0(label_to_use, " schneider"),
                       "basin_id"= cal_results_intersting$station)

  data_2 <- data.frame("value"= delta_2,
                       "label"= paste0(label_to_use, " hanasaki"),
                       "basin_id"= cal_results_intersting$station)

  return(rbind(data_1, data_2))
}


compare_models <- function(cal_results,
                           reference_column,
                           to_compare_column,
                           label_to_use,
                           min_quality = NULL){

  station_sufficient_quality <- cal_results$station

  if (!is.null(min_quality)) {
    station_sufficient_quality <- cal_results$station[
      cal_results[reference_column] > min_quality |
      cal_results[to_compare_column] > min_quality]
  }

  selected_stations <- cal_results$station %in% c(station_sufficient_quality)
  cal_results_intersting <- cal_results[selected_stations,]
  cal_results_others <- cal_results[!selected_stations,]

  # positive --> alternative better than reference!
  delta <- (cal_results_intersting[[to_compare_column]]  -
              cal_results_intersting[[reference_column]])

  data <- data.frame("value"= delta,
                     "label"= label_to_use,
                     "basin_id"=cal_results_intersting$station)

  return(data)
}


get_behavioral_data <- function(cal_results,
                           reference_column,
                           to_compare_column,
                           label_to_use,
                           min_quality = NULL,
                           only_sensitive=F){

  station_sufficient_quality <- cal_results$station

  if (!is.null(min_quality)) {
    station_sufficient_quality <- cal_results$station[
      cal_results[reference_column] > min_quality |
      cal_results[to_compare_column] > min_quality]
  }

  selected_stations <- cal_results$station %in% c(station_sufficient_quality)
  cal_results_intersting <- cal_results[selected_stations,]

  if (only_sensitive){
    delta <- cal_results_intersting[reference_column] -
      cal_results_intersting[to_compare_column]
    mask <- (abs(delta) > 0.01)
    cal_results_intersting <- cal_results_intersting[mask,]
  }
  data <- data.frame(
    "reference"= cal_results_intersting[reference_column],
    "alternative" = cal_results_intersting[to_compare_column],
    "label"= label_to_use,
    "basin_id"= cal_results_intersting$station)

  return(data)
}
