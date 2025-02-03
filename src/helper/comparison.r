
compare_models <- function(cal_results,
                           reference_column,
                           to_compare_column,
                           label_to_use,
                           min_quality = NULL) {

  station_sufficient_quality <- cal_results$station

  if (!is.null(min_quality)) {
    station_sufficient_quality <- cal_results$station[
      cal_results[reference_column] > min_quality |
      cal_results[to_compare_column] > min_quality]
  }

  selected_stations <- cal_results$station %in% c(station_sufficient_quality)
  cal_results_interesting <- cal_results[selected_stations, ]

  # positive --> alternative better than reference!
  delta <- (cal_results_interesting[[to_compare_column]]  -
              cal_results_interesting[[reference_column]])

  data <- data.frame("value" = delta,
                     "label" = label_to_use,
                     "basin_id" =cal_results_interesting$station)

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
