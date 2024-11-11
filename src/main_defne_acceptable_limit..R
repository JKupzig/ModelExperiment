rm(list=ls())

library(ggplot2)
library(dplyr)

source("./src/helper/read_data.r")

MIN_QUAl <- 0.0
MAX_QUAL <- 0.4

validation_set <- read_kge_and_define_good_basins(MIN_QUAl, MAX_QUAL)
print(length(validation_set$behavioural))

kge_info <- read_kge_and_define_good_basins(MAX_QUAL)
print(length(kge_info$behavioural))

ggplot(kge_info$data) +
  geom_boxplot(aes(y=KGE_val, x=variant)) +
  ylim(c(0, 1)) +
  geom_hline(yintercept = MIN_QUAl)

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
points(a[results>MIN_QUAl], b[results>MIN_QUAl], col="firebrick", pch=15)
plot(a, r, pch=15)
points(a[results>MIN_QUAl], r[results>MIN_QUAl], col="firebrick", pch=15)
plot(b, r, pch=15)
points(b[results>MIN_QUAl], r[results>MIN_QUAl], col="firebrick", pch=15)
dev.off()
# selected threshold is in between the minimum and maximum values of the
# median KGE values of all model variants (same condition as in Massmann et al. 2020)

