library(watergap3data)

ROOT_STANDARD = "./data/OUTPUT_RIVERVELOCITY_BUGFIX"
ROOT_OLD = "./data/OUTPUT_RIVERVELOCITY_NOBUGFIX"

mask <- watergap3data::unf.readunf(file.path(ROOT_STANDARD, "G_CALIB_BASIN.UNF2"), cont="na")
files_to_read <- list.files(ROOT_STANDARD, pattern="G_RIVER_VELO*")


standard <- NULL
old <- NULL

for (file_to_read in files_to_read) {
  standard_data <- watergap3data::unf.readunf(file.path(ROOT_STANDARD, file_to_read), cont="na")
  standard <- rbind(standard, standard_data[, mask > 0])

  old_data <- watergap3data::unf.readunf(file.path(ROOT_OLD, file_to_read), cont="na")
  old <- rbind(old, old_data[, mask > 0])
}


probs <- seq(0.25,1.0,0.05)
probs25 <- which(probs == 0.25)
probs50 <- which(probs == 0.5)
probs100 <- which(probs == 1.0)
old_quantiles <- matrixStats::colQuantiles(old, probs = probs)
standard_quantiles <- matrixStats::colQuantiles(standard,probs = probs)

old_mean_comparison <- colMeans(old)
stanard_mean_comparison <- colMeans(standard)

# most extreme cell
plot(probs, standard_quantiles[58675,], type="l", ylim=c(0,0.6),
     ylab="river velocity [m/s]", xlab="percentile")
lines(probs, old_quantiles[58675,], col="firebrick")

# general effect
boxplot(list("no bugfix"=as.vector(old_mean_comparison),
             "bugfix"=as.vector(stanard_mean_comparison)),
        ylab="river velocity [m/s]", main="mean velocity")
abline(h=1, col="firebrick", lwd=2)

boxplot(list("no bugfix"=as.vector(old_quantiles[, probs100]),
             "bugfix"=as.vector(standard_quantiles[,probs100])),
        ylab="river velocity [m/s]", main="maximal velocity")
abline(h=1, col="firebrick", lwd=2)

boxplot(list("no bugfix"=as.vector(old_quantiles[, probs25]),
             "bugfix"=as.vector(standard_quantiles[,probs25])),
        ylab="river velocity [m/s]", main="low flow velocity (25th percentile)")
abline(h=1, col="firebrick", lwd=2)

