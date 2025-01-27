#Appendix S3

rm(list=ls())


source("./src/helper/read_data.r")

MIN_QUAL <- 0.4 #0.2
MAX_QUAL <- NULL #0.4
ROOT <- "./data/evaluation/cal_result_model_m%i_uncertainty.rds"
TARGET = "./plots/supplement/uncertainty_%s_%f.png"
behavioural_basins <- read_kge_and_define_good_basins(MIN_QUAL, MAX_QUAL)

best <- c(8,8,8,8) #schneider, hanasaki, static, snow
standard <- c(16, 12, 11, 9) #no res, variable, no snow-on-wetlands
names <- c("schneider", "hanasaki", "variable", "no snow")

for (comparison_id in 1:length(names)){

  uncertain_KGE_best <- readRDS(sprintf(ROOT, best[comparison_id]))
  uncertain_KGE_standard <- readRDS(sprintf(ROOT, standard[comparison_id]))

  benchmark_to_examine <- "KGE"
  stations_to_use <- which(dimnames(uncertain_KGE_best)[[4]] %in% behavioural_basins$behavioural)
  benchmark_to_use <- which(dimnames(uncertain_KGE_best)[[2]] == benchmark_to_examine)

  png(sprintf(TARGET, names[comparison_id], MIN_QUAL),
      res=300, units="cm", width=16, height=12)
  par(mfrow=c(1,6), mar=c(5,2,3,2)+.1)

  uncertainty_label<- c("reference", "x_p", "+x_hyd", "-x_hyd", "x_p and +x_hyd", "x_p and -x_hyd")
  for (uncertainty_id in seq(0,5,1)){

    uncertainty_type <- sprintf("uncertainty%i_wetlStorage100", uncertainty_id)
    uncertainty_to_use <- which(dimnames(uncertain_KGE_best)[[3]] == uncertainty_type)

    recommended_run <- uncertain_KGE_best[2,
                                          benchmark_to_use,
                                          uncertainty_to_use,
                                          stations_to_use]

    standard_run <- uncertain_KGE_standard[2,
                                           benchmark_to_use,
                                           uncertainty_to_use,
                                           stations_to_use]


    interesting_ids <- which(abs(standard_run - recommended_run)>0.01)
    boxplot(list("delta"=standard_run[interesting_ids]-recommended_run[interesting_ids]),
            ylim=c(-0.5,0.5),
            xlab=uncertainty_label[uncertainty_id+1],
            ylab("\u0394KGE"))

    if (uncertainty_id == 1){
      title(main=sprintf("number of basins: %i", length(interesting_ids)), cex.main=0.7)
    }
    if (uncertainty_id == 0){
      title(main=sprintf("difference: %s", names[comparison_id]), cex.main=0.7)

    }


    abline(h=0, col="firebrick")
  }
  dev.off()

}

