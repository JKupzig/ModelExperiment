#Appendix S3

rm(list = ls())

source("./src/helper/read_data.r")

# TODO update uncertainty data, for some basins not the calibrated gamma value where used in model runs!
ROOT <- "./data/cal_result_model_m%i_uncertainty.rds"
TARGET <- "./plots/supplement/supplement_s3_%s_%s.png"

REFERENCE_MODEL <- c(8, 8, 8, 8)
COMPARISON_MODEL <- c(16, 12, 11, 9)
GROUP_NAMES <- c("schneider", "hanasaki", "variable", "no snow")
LABELS <- c("reference",
            "x_p", "+x_hyd", "-x_hyd", "x_p and +x_hyd", "x_p and -x_hyd")

behavioural_basins <- read_kge_and_define_good_basins(NULL, NULL)

for (comparison_id in seq_along(GROUP_NAMES)){

  uncertain_KGE_best <- readRDS(sprintf(ROOT, REFERENCE_MODEL[comparison_id]))
  uncertain_KGE_standard <- readRDS(sprintf(ROOT, COMPARISON_MODEL[comparison_id]))

  benchmark_to_examine <- "KGE"
  stations_to_use <- which(dimnames(uncertain_KGE_best)[[4]] %in% behavioural_basins$behavioural)
  benchmark_to_use <- which(dimnames(uncertain_KGE_best)[[2]] == benchmark_to_examine)

  #png(sprintf(TARGET, GROUP_NAMES[comparison_id], name),
  #    res = 300, units = "cm", width = 16, height = 12)
  par(mfrow = c(1, 6), mar = c(5, 2, 3, 2) + .1)

  interesting_ids <- NULL
  for (uncertainty_id in seq_along(LABELS)){

    recommended_run <- uncertain_KGE_best[2,
                                          benchmark_to_use,
                                          uncertainty_id,
                                          stations_to_use]

    standard_run <- uncertain_KGE_standard[2,
                                           benchmark_to_use,
                                           uncertainty_id,
                                           stations_to_use]

    if (is.null(interesting_ids)) {
      interesting_ids <- which(abs(standard_run - recommended_run) > 0.01)
    }

    boxplot(list(
      "delta" = standard_run[interesting_ids] - recommended_run[interesting_ids]),
            ylim = c(-0.5, 0.5),
            xlab = LABELS[uncertainty_id],
            ylab("\u0394KGE"))

    if (uncertainty_id == 1) {
      title(main = sprintf("number of basins: %i", length(interesting_ids)),
            cex.main = 0.7)
    }

    abline(h = 0, col = "firebrick")
  }
  #dev.off()
}
