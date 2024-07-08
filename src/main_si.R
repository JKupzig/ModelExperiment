
rm(list=ls())
library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("./data/SI.rds")

static_models <- c(8, 9, 12, 13, 16, 17,
                 8+12, 9+12, 12+12, 13+12, 16+12, 17+12)-7

variable_models <- c(10, 11, 14, 15, 18, 19,
                     10+12, 11+12, 14+12, 15+12, 18+12, 19+12) - 7


si_id <- 1
good_stations <- which(apply(data[,11,], 2, max) > 0.2)
cum_values <- data[c(1, 2, 4, 5, 9, 13), si_id, good_stations]
cum_values <- as.data.frame(cum_values)
cum_values$runtype <- row.names(cum_values)
cum_values_long <- cum_values %>%
  pivot_longer(., cols=-runtype)

ggplot(cum_values_long, aes(x = value, col=runtype)) +
  stat_ecdf(geom="line") +
  theme_bw() +
  scale_color_manual(labels = c("8"="static (10d)",
                                "12"="static (10d) + hanasaki",
                                "16"="static (10d) + schneider",
                                "9"="static (10d) + snow",
                                "8_wetlStorage100"="static (100d)",
                                "11"="variable (10d)"),
                     values = c("cornflowerblue",
                                "firebrick",
                                "forestgreen",
                                "orange",
                                "navy",
                                "lightgreen"))

static_values <- as.vector(data[static_models, si_id, good_stations])

variable_values <- as.vector(data[variable_models, si_id, good_stations])
q25 <- quantile(c(static_values, variable_models), 0.25)
q75 <- quantile(c(static_values, variable_models), 0.75)
median <- q75 <- quantile(c(static_values, variable_models), 0.5)

min_value <- median - 1.5*(q75-q25)
max_value <- median + 1.5*(q75-q25)

plot(static_values, variable_values, type="p", ylim=c(min_value, max_value), xlim=c(min_value, max_value))
abline(a=0, b=1, col="firebrick", lwd=2)
abline(h=1, col="lightgrey", lwd=1)
boxplot(list("static"=static_values, "variable"=variable_values), ylim=c(min_value, max_value))
abline(h=1, col="firebrick", lwd=2)

data[1,2,5]
data[1,3,5]
