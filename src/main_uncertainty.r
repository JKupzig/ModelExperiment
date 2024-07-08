
model <- 18
uncertain_KGE <- readRDS("./data/cal_result_model_m18_uncertainty.rds")
calibrated_KGE <- read.table("./data/KGE_results/cal_result_benchmarks_model_m18.txt",
                             header=T)
calibrated_KGE_wetl100 <- read.table("./data/KGE_results/cal_result_benchmarks_model_m18_wetlStorage100.txt",
                                     header=T)

uncertain_KGE_better <- readRDS("./data/cal_result_model_m13_uncertainty.rds")

stations_to_use <- which(
  (uncertain_KGE_better[2,1,1,] > 0.2 | uncertain_KGE[2,1,1,] > 0.2) &
    uncertain_KGE_better[2,1,1,] - uncertain_KGE[2,1,1,] > 0.01)


par(mfrow=c(1,6))
boxplot(list("m13"=uncertain_KGE_better[2,1,1,stations_to_use],
             "m18"=uncertain_KGE[2,1,1,stations_to_use]),
        ylim=c(0.0, 1), main="10d for smaller wetlands")
boxplot(list("m13"=uncertain_KGE_better[2,1,2,stations_to_use],
             "m18"=uncertain_KGE[2,1,2,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,3,stations_to_use],
             "m18"=uncertain_KGE[2,1,3,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,4,stations_to_use],
             "m18"=uncertain_KGE[2,1,4,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,5,stations_to_use],
             "m18"=uncertain_KGE[2,1,5,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,6,stations_to_use],
             "m18"=uncertain_KGE[2,1,6,stations_to_use]),
        ylim=c(0.0, 1))
dev.off()

par(mfrow=c(1,6))
boxplot(list("m13"=uncertain_KGE_better[2,1,7,stations_to_use],
             "m18"=uncertain_KGE[2,1,7,stations_to_use]),
        ylim=c(0.0, 1), main="100d for smaller wetlands")
boxplot(list("m13"=uncertain_KGE_better[2,1,8,stations_to_use],
             "m18"=uncertain_KGE[2,1,8,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,9,stations_to_use],
             "m18"=uncertain_KGE[2,1,9,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,10,stations_to_use],
             "m18"=uncertain_KGE[2,1,10,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,11,stations_to_use],
             "m18"=uncertain_KGE[2,1,11,stations_to_use]),
        ylim=c(0.0, 1))
boxplot(list("m13"=uncertain_KGE_better[2,1,12,stations_to_use],
             "m18"=uncertain_KGE[2,1,12,stations_to_use]),
        ylim=c(0.0, 1))
dev.off()

# ist der unterschied zwischen den KGEs m13 uncertainty kleiner als zw. m13 und m18?
boxplot(list("m13"=uncertain_KGE_better[2,1,1,stations_to_use],
             "m18"=uncertain_KGE[2,1,12,stations_to_use]),
        ylim=c(0.0, 1))
dev.off()

metric_to_look <- 1
ylim_to_use <- 1
following <- order(uncertain_KGE_better[2,metric_to_look,1,stations_to_use])
x_entries <- seq(1,length(stations_to_use),1)
y_better <- uncertain_KGE_better[2,metric_to_look,1,stations_to_use][following]
y_better_min <- apply(uncertain_KGE_better[2,metric_to_look,1:6,],2, min)[stations_to_use][following]
y_better_max <- apply(uncertain_KGE_better[2,metric_to_look,1:6,],2, max)[stations_to_use][following]
plot(x_entries, uncertain_KGE_better[2,metric_to_look,1,stations_to_use][following],
     ylim=c(0,ylim_to_use), pch=19)
abline(h=1)
arrows(x_entries, y_better_min,
       x_entries, y_better_max,
       length=0.05, angle=90, code=3, lwd=2,
       col="darkgrey")

#lines(x_entries, uncertain_KGE[2,1,1,stations_to_use][following], col="firebrick", lwd=2)
points(x_entries, uncertain_KGE[2,metric_to_look,1,stations_to_use][following], col="firebrick", pch=17)

not_sensitive_at_all <- which(apply(uncertain_KGE_better[2,metric_to_look,1:6,], 2, sd) == 0)
uncertain_KGE[2,,,not_sensitive_at_all] #sich ausgleichende Effekte!
