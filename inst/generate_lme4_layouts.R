# Generate the lme4 reference layouts used by tests/testthat/test_initialize.R
#
# tidyposterior parses mixed-model formulas with its own code (see
# R/initialization.R) so that the package does not depend on lme4. The test
# suite verifies that parser against layouts that were generated with
# lme4::lFormula(), stored as a base-R fixture in
# tests/testthat/fixtures/lme4-layouts.rds so that neither lme4 nor Matrix is
# needed to run the tests.
#
# To recreate or extend the fixture (e.g., after adding a supported model
# structure, or to spot-check a new lme4 version), run this script from the
# package source directory:
#
#   Rscript inst/generate_lme4_layouts.R
#
# lme4 is required to run this script but is not a package dependency.

helper_path <- file.path("tests", "testthat", "helper-initialization.R")
if (!file.exists(helper_path)) {
  stop("Run this script from the tidyposterior package source directory.")
}
source(helper_path)

lme4_layout <- function(formula, data) {
  # the identifiability checks are disabled so that layouts can be computed
  # even for structures that stan_glmer() refuses to fit (heterogeneous
  # variances with nested resamples)
  ctrl <- lme4::lmerControl(
    check.nobs.vs.nRE = "ignore",
    check.nobs.vs.nlev = "ignore",
    check.nobs.vs.rankZ = "ignore"
  )
  lf <- lme4::lFormula(formula, data = data, control = ctrl)
  asgn <- attr(lf$reTrms$flist, "assign")

  trms <- vector("list", length(lf$reTrms$cnms))
  for (i in seq_along(trms)) {
    fct <- lf$reTrms$flist[[asgn[i]]]
    p <- length(lf$reTrms$cnms[[i]])
    # dense per-observation design values, reconstructed from lme4's sparse
    # (level-major) transposed design matrix
    z_t <- lf$reTrms$Ztlist[[i]]
    design <- matrix(0, ncol(z_t), p)
    for (l in seq_len(nlevels(fct))) {
      in_lvl <- which(as.integer(fct) == l)
      if (length(in_lvl) > 0) {
        block <- ((l - 1) * p + 1):(l * p)
        design[in_lvl, ] <- t(as.matrix(z_t[block, in_lvl, drop = FALSE]))
      }
    }
    trms[[i]] <- list(
      name = names(lf$reTrms$cnms)[i],
      coef_names = lf$reTrms$cnms[[i]],
      levels = levels(fct),
      codes = as.integer(fct),
      design = design
    )
  }
  # everything is stored as base R objects so that loading the fixture does
  # not require lme4 or Matrix
  list(X = unname(as.matrix(lf$X)), terms = trms)
}

cv_data <- initialize_cv_data()
rcv_data <- initialize_rcv_data()
p4_data <- initialize_p4_data()

layouts <- list(
  hetero = lme4_layout(statistic ~ model + (model + 0 | id), cv_data),
  homogeneous = lme4_layout(statistic ~ model + (1 | id), cv_data),
  nested = lme4_layout(statistic ~ model + (1 | id2 / id), rcv_data),
  hetero_nested = lme4_layout(
    statistic ~ model + (model + 0 | id2 / id),
    rcv_data
  ),
  four_models = lme4_layout(statistic ~ model + (model + 0 | id), p4_data)
)
attr(layouts, "lme4_version") <- as.character(utils::packageVersion("lme4"))
attr(layouts, "generated") <- format(Sys.Date())

path <- file.path("tests", "testthat", "fixtures", "lme4-layouts.rds")
dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
saveRDS(layouts, path, version = 2)
cat(
  "Wrote",
  path,
  "with",
  length(layouts),
  "layouts using lme4",
  attr(layouts, "lme4_version"),
  "\n"
)
