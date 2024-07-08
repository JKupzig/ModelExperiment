rm(list=ls())

library(ggplot2)
library(dplyr)

ROOT <- "./data/KGE_results"
files_to_read <- list.files(ROOT, pattern=".txt")

all_kges <- NULL
for (file in files_to_read){

  cal_result <- read.table(file.path(ROOT, file), sep="\t", header=T)
  cal_result$variant <- substr(file, 23, 40)
  all_kges <- rbind(all_kges,  cal_result)
}

medians <- all_kges %>%
  group_by(variant) %>%
  dplyr::summarize(median = median(kge_val, na.rm=TRUE))
plot(medians$median)
abline(h=0.2)

kge <- function(a, b, r){
  value <- 1 - sqrt( (1-a)**2 + (1-b)**2 + (1-r)**2 )
  return(value)
}

a <- runif(10000, min=0.1, max=2)
b <- runif(10000, min=0.1, max=2)
r <- runif(10000, min=0, max=1)
results <- kge(a,b,r)

png("./plots/acceptable_kge.png", units="cm", width=24, height=12, res=300)
par(mfrow=c(1,3))
plot(a, b, pch=15)
points(a[results>0.2], b[results>0.2], col="firebrick", pch=15)
plot(a, r, pch=15)
points(a[results>0.2], r[results>0.2], col="firebrick", pch=15)
plot(b, r, pch=15)
points(b[results>0.2], r[results>0.2], col="firebrick", pch=15)
dev.off()
# selected threshold is in between the minimum and maximum values of the
# median KGE values of all model variants (same condition as in Massmann et al. 2020)

median(all_kges$kge_cal)
quantile(all_kges$kge_cal, seq(0, 1, 0.1))


ggplot(all_kges, aes(y=kge_val, col=variant)) +
        stat_ecdf() +
        coord_cartesian(ylim=c(-1, 1))

