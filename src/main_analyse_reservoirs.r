
rm(list=ls())
source("src/helper/color_ramps.r")

ROOT <- "./data/evaluation/reservoirs"
TARGET <- "./plots/reservoirs/%i_info.png"
START <- as.Date("01.01.1986", format="%d.%m.%Y")
END <- as.Date("31.12.1993", format="%d.%m.%Y")

files_to_plot_availability <- list.files(ROOT, pattern="*availablity.csv")
files_to_plot_storages <- list.files(ROOT, pattern="*storage.csv")

count <- 1
for (file_to_plot_availability in files_to_plot_availability){
  print(count)

  file_to_plot_storages <- files_to_plot_storages[count]

  res_id <- as.integer(strsplit(file_to_plot_storages, "_")[[1]][1])
  target <- sprintf(TARGET, res_id)
  count <- count + 1


  # plotting and reading
  ylabs <- c("Outflow from Cell (km3/d)", "Mean monthly storage (km3)")
  files_to_plot <- c(file_to_plot_availability, file_to_plot_storages)


  png(target, units="cm", width=35, height=20, res=300)
  par(mfrow=2:1)
  for (plot_nr in c(1,2)){
    storage <- read.table(file.path(ROOT, files_to_plot[plot_nr]), sep=";", header=T)
    storage$date <- as.Date(storage$date, format="%Y-%m-%d")
    storage <- storage[order(storage$date),]
    storage <- storage[storage$date >= START &
                         storage$date <= END, ]

    y_min <- min(storage$hanasaki, storage$schneider)
    y_max <- max(storage$hanasaki, storage$schneider)

    if (plot_nr == 1){
      y_min <- min(storage$hanasaki, storage$schneider, storage$as_lakes)
      y_max <- max(storage$hanasaki, storage$schneider, storage$as_lakes)*1.2
    }

    plot(storage$date, storage$hanasaki, type="l", col=alpha(datylon_map[4], 1.0), lwd=2,
         ylim=c(y_min, y_max), ylab=ylabs[plot_nr], xlab="")

    if (plot_nr == 1){
      lines(storage$date, storage$as_lakes, col=alpha(datylon_map[6], 0.8), lwd=2)
    }

    lines(storage$date, storage$schneider, col=alpha(datylon_map[1], 0.7), lwd=2)



    if (plot_nr == 1){
      legend("topleft", legend=c("reservoir algorithm (V1)", "reservoir algorithm (V2)", "as lakes"),
             col=c(datylon_map[4], datylon_map[1], datylon_map[6]), lty=1, lwd=2, cex=0.8)
      q95_lake <- round(quantile(storage$as_lakes, 0.95), 4)
      q95_schneider <- round(quantile(storage$schneider, 0.95), 4)
      q95_hanasaki <- round(quantile(storage$hanasaki, 0.95), 4)

      q99_lake <- round(quantile(storage$as_lakes, 0.99), 4)
      q99_schneider <- round(quantile(storage$schneider, 0.99), 4)
      q99_hanasaki <- round(quantile(storage$hanasaki, 0.99), 4)

      max_lake <- round(max(storage$as_lakes), 4)
      max_schneider <- round(max(storage$schneider), 4)
      max_hanasaki <- round(max(storage$hanasaki), 4)

      legend(
        'topright', ncol = 4L, title = '',
        cex = 0.6,
        legend = c(
          '', 'as lakes', 'hanasaki', 'schneider',
          'Q95', c(q95_lake, q95_hanasaki, q95_schneider),
          'Q99', c(q99_lake, q99_hanasaki, q99_schneider),
          'max', c(max_lake, max_hanasaki, max_schneider)
        )
      )
    }
  }

  dev.off()
}



