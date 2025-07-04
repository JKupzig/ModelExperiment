# reads meta data from grcd and plots data availability
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(raster)
library(watergap3data)



root <- r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\AdjustStationPosition)"
root_data <-  r"(C:\Users\jenny\MyProject_sciebo_backup\GRDC_2020\Rohdaten)"
routing_path <- r"(C:\Users\jenny\MyProject_sciebo_backup\WaterGAPlite\data\routing)"

plot_name <- "./plots/review/figure_internal_data_availability.png"

cont <- "na"
min_area <- 3000
min_missing <- 0.0

#reading adjusted stations
file2read <- file.path(root, cont, paste0("GRDC_Stations_", cont, "_Adj.csv"))
stations <- read.csv(file2read, sep = "\t", header = TRUE, skip = 2)
dim(stations) #1875

stations <- stations[stations$corArea > min_area, ]
dim(stations) #953

#only headbasins - mit dieser funktion stimmt was nicht, noch einmal ?berpr?fen!
search_inflow <- function(cell, basinForCell, g_inflc) {
  for(i in 1:9) {
    if(g_inflc[i, cell] > 0) {
      basinForCell<-search_inflow(g_inflc[i, cell], basinForCell, g_inflc)
    }
  }
  return(c(basinForCell, cell))
}

g_inflc <- unf.readunf(file.path(routing_path, cont, "G_INFLC.9.UNF4"), cont)

stations <- stations[order(stations$corArea), ] #small --> big
stations$use <- rep(FALSE, nrow(stations))
pb <- txtProgressBar(min = 1, max =  nrow(stations), style = 3)
for (j in seq_along(stations$grdc_no)) {
  basin_for_cell <- NULL
  basin_for_cell <- search_inflow(stations$corGCRC[j], basin_for_cell, g_inflc=g_inflc)
  out <- (!sum(basin_for_cell %in% stations$corGCRC) > 1) #no other basin included
  stations$use[j] <- out
  setTxtProgressBar(pb, j)
}

stations <- stations[stations$use == TRUE, ]
dim(stations) #462 head basins
to_less_data <- c()

#yearly data-availability
#min data per year = 50%, then 1 otherwise 0
results <- data.frame(year = seq(1979, 2016, 1),
                     count = rep(0, length(seq(1979, 2016, 1)))
                    )
pb <- txtProgressBar(min = 1, max =  nrow(stations), style = 3)
for (i in seq_along(stations$grdc_no)) {
  setTxtProgressBar(pb, i)
  file2read <- file.path(root_data, paste0(stations[i, 1], "_Q_day.Cmd.txt"))

  if (!file.exists(file2read)){
    to_less_data <- c(to_less_data, stations[i, 1])
    next
  } else {
    data <- read.csv2(
        file2read, skip = 37, sep = ";", header=FALSE,
        dec = ".", colClasses = c("Date", NA, "numeric"))
    colnames(data) <- c("Date", "Time", "Value")

    timeseq <- data.frame(
        "Date" = seq(as.Date("1979-01-01"), as.Date("2016-12-31"), by = 1))
    sData <- merge(timeseq, data, by = "Date", all.x = TRUE)
    sData$Value[sData$Value < 0] <- NA

    missing_days <- sData %>%
      mutate(year = as.integer(format(Date, "%Y"))) %>%
      group_by(year) %>%
      summarise(percent_missing = sum(is.na(Value)) / length(Value)) %>%
      as.data.frame()

    results$count <- results$count + (missing_days$percent_missing <= min_missing)
  }
}


ggplot(results, aes(x = year, y = count)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 226, linetype = "solid") +
  geom_vline(xintercept = 1979, linetype = "dashed", color = "maroon") +
  geom_vline(xintercept = 1994, linetype = "dashed", color = "maroon") +
  theme_bw() +
  annotate(
    geom = "text", x = 2010, y = 760,
    label = "All grdc stations fitting the requirements",
           color = "black") +
  xlab("Year") +
  ylim(0, 765) +
  ylab("Stations with no missing data in the year")

ggsave(plot_name,  device="png",
       width = 25, height = 12, units = "cm", dpi = 300)
