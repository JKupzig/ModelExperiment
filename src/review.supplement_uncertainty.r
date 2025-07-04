#Appendix S3

rm(list = ls())

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

ROOT <- "./data/cal_result_model_m%i_uncertainty.rds"
TARGET <- "./plots/review/supplement_s3_%s.png"

REFERENCE_MODEL <- c(12, 8, 8, 8)
COMPARISON_MODEL <- c(16, 12, 11, 9)
GROUP_NAMES <- c("schneider", "hanasaki", "variable", "no snow")
lables_for_affected <- c("reservoir", "reservoir", "river", "snow")
LABELS <- c("original",
            expression(x[p]), expression(+x[hyd]), expression(-x[hyd]),
            expression(paste(x[p], " & ", +x[hyd])), expression(paste(x[p], " & ", -x[hyd])))

behavioural_basins <- read_kge_and_define_good_basins()

for (comparison_id in seq_along(GROUP_NAMES)){

  uncertain_KGE_best <- readRDS(sprintf(ROOT, REFERENCE_MODEL[comparison_id]))
  uncertain_KGE_standard <- readRDS(sprintf(ROOT, COMPARISON_MODEL[comparison_id]))

  benchmark_to_examine <- "KGE"
  benchmark_to_use <- which(dimnames(uncertain_KGE_best)[[2]] == benchmark_to_examine)

  png(sprintf(TARGET, GROUP_NAMES[comparison_id]),
      res = 300, units = "cm", width = 16, height = 12)
  par(mfrow = c(1, 6), mgp=c(2,1,0), mar = c(3, 3, 0, 0) + .1)

  interesting_ids <- NULL
  for (uncertainty_id in seq_along(LABELS)){

    recommended_run <- uncertain_KGE_best[2,
                                          benchmark_to_use,
                                          uncertainty_id,
                                          ]

    standard_run <- uncertain_KGE_standard[2,
                                           benchmark_to_use,
                                           uncertainty_id,
                                           ]

    if (is.null(interesting_ids)) {
      interesting_ids <- get_sensitive_basins(lables_for_affected[comparison_id])
    }
    behavioural_ids <- behavioural_basins$behavioural[behavioural_basins$behavioural %in% interesting_ids]


    ylab = ""
    if (uncertainty_id == 1) {
      boxplot(list(
        "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids],
        "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids]),
        col= datylon_map[c(7,5)],
        ylim = c(-0.5, 0.5),
        xlab = LABELS[uncertainty_id],
        ylab = "\u0394KGE",
        xaxt='n')
    } else {
      boxplot(list(
        "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids],
        "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids]),
        col= datylon_map[c(7,5)],
        ylim = c(-0.5, 0.5),
        xlab = LABELS[uncertainty_id],
        ylab = "",
        xaxt="n",
        yaxt = "n")
    }

    grid(nx = NULL, ny = NULL,
         lty = 1,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width

    abline(h = 0, col = "firebrick", lwd=1.0)

    boxplot(list(
      "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids],
      "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids]),
      col= datylon_map[c(7,5)],
      xaxt="n",
      yaxt = "n",
      add=TRUE)

  }
  dev.off()
}

# verprobung:
# ROOT1 <- "./data/cal_result_model_m8_uncertainty.rds"
# ROOT2 <- "./data/cal_result_benchmarks_model_m8_wetlStorage100.txt"
# new_data <- readRDS(ROOT1)
# ref_data <- read.csv(ROOT2, sep = "\t")
# sum(new_data[2,1,1, ] == ref_data$KGE_val) / length(ref_data$KGE_val) #100 %


