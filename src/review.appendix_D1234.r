# Appendix S3

rm(list = ls())

source("./src/helper/read_data.r")
source("./src/helper/comparison.r")
source("./src/helper/color_ramps.r")

ROOT <- "./data/cal_result_model_m%i_uncertainty.rds"
TARGET <- "./plots/review/appendixD_%s.png"

REFERENCE_MODEL <- c(12, 8, 8, 8)
COMPARISON_MODEL <- c(16, 12, 11, 9)
GROUP_NAMES <- c("schneider", "hanasaki", "variable", "no snow")
lables_for_affected <- c("reservoir", "reservoir", "river", "snow")
LABELS <- c(
  "original",
  expression(x[p]), expression(+x[hyd]), expression(-x[hyd]),
  expression(paste(x[p], " & ", +x[hyd])), expression(paste(x[p], " & ", -x[hyd]))
)

behavioural_basins <- read_kge_and_define_good_basins()

for (comparison_id in seq_along(GROUP_NAMES)) {
  uncertain_KGE_best <- readRDS(sprintf(ROOT, REFERENCE_MODEL[comparison_id]))
  uncertain_KGE_standard <- readRDS(sprintf(ROOT, COMPARISON_MODEL[comparison_id]))

  benchmark_to_examine <- "KGE"
  benchmark_to_use <- which(dimnames(uncertain_KGE_best)[[2]] == benchmark_to_examine)

  png(sprintf(TARGET, GROUP_NAMES[comparison_id]),
    res = 300, units = "cm", width = 16, height = 8
  )
  par(mfrow = c(1, 6), mgp = c(2, 1, 0), mar = c(3, 3, 0, 0.2) + .1)

  interesting_ids <- NULL
  for (uncertainty_id in seq_along(LABELS)) {
    recommended_run <- uncertain_KGE_best[
      2,
      benchmark_to_use,
      uncertainty_id,
    ]

    standard_run <- uncertain_KGE_standard[
      2,
      benchmark_to_use,
      uncertainty_id,
    ]

    if (is.null(interesting_ids)) {
      interesting_ids <- get_sensitive_basins(lables_for_affected[comparison_id])
    }
    behavioural_ids <- behavioural_basins$behavioural[behavioural_basins$behavioural %in% interesting_ids]


    ylab <- ""
    if (uncertainty_id == 1) {
      boxplot(
        list(
          "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids],
          "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids]
        ),
        col = datylon_map[c(5, 7)],
        ylim = c(-0.5, 0.5),
        xlab = LABELS[uncertainty_id],
        ylab = "\u0394KGE",
        xaxt = "n"
      )
    } else {
      boxplot(
        list(
          "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids],
          "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids]
        ),
        col = datylon_map[c(5, 7)],
        ylim = c(-0.5, 0.5),
        xlab = LABELS[uncertainty_id],
        ylab = "",
        xaxt = "n",
        yaxt = "n"
      )
    }

    grid(
      nx = NULL, ny = NULL,
      lty = 1, # Grid line type
      col = "gray", # Grid line color
      lwd = 0.5
    ) # Grid line width

    abline(h = 0, col = "firebrick", lwd = 1.0)

    boxplot(
      list(
        "behavioural" = standard_run[behavioural_ids] - recommended_run[behavioural_ids],
        "non-behavioural" = standard_run[interesting_ids] - recommended_run[interesting_ids]
      ),
      col = datylon_map[c(5, 7)],
      xaxt = "n",
      yaxt = "n",
      add = TRUE
    )

    if (uncertainty_id == 1) {
      legend("bottom",
        c("behav.", "non-behav."),
        box.lty = 0, fill = datylon_map[c(5, 7)], cex = 0.75, xpd = TRUE, bty = "n"
      )
    }
  }
  dev.off()
}
