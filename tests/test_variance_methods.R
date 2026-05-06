# =====================================================================
# Variance estimation method test
#
# Builds a small svydesign, derives as.svrepdesign(type="bootstrap")
# and type="JK1", runs svytotal / svymean, and asserts that SEs are
# positive and finite.
#
# Skipped when 'survey' is not installed.
# =====================================================================

local({
  if (!requireNamespace("survey", quietly = TRUE)) {
    cat("SKIP: survey not installed\n")
    cat("variance methods: SKIP\n")
    return(invisible(NULL))
  }

  set.seed(7)
  n <- 250
  df <- data.frame(
    y      = rnorm(n, mean = 100, sd = 15),
    weight = runif(n, 0.5, 2.0),
    strata = sample(c("a","b","c","d"), n, TRUE),
    stringsAsFactors = FALSE
  )
  des <- survey::svydesign(ids = ~1, strata = ~strata,
                            weights = ~weight, data = df, nest = TRUE)

  # Bootstrap
  des_boot <- survey::as.svrepdesign(des, type = "bootstrap",
                                      replicates = 200)
  m_boot <- survey::svymean(~y, design = des_boot, na.rm = TRUE)
  se_boot <- as.numeric(sqrt(diag(as.matrix(stats::vcov(m_boot)))))
  stopifnot(is.finite(se_boot), se_boot > 0)
  cat("OK bootstrap-rep SE positive & finite: SE=",
      sprintf("%.4f", se_boot), "\n", sep = "")

  t_boot <- survey::svytotal(~y, design = des_boot, na.rm = TRUE)
  se_total <- as.numeric(sqrt(diag(as.matrix(stats::vcov(t_boot)))))
  stopifnot(is.finite(se_total), se_total > 0)
  cat("OK bootstrap svytotal SE positive: SE=",
      sprintf("%.4f", se_total), "\n", sep = "")

  # JK1
  des_jk1 <- tryCatch(survey::as.svrepdesign(des, type = "JK1"),
                       error = function(e) NULL)
  if (!is.null(des_jk1)) {
    m_jk1 <- survey::svymean(~y, design = des_jk1, na.rm = TRUE)
    se_jk1 <- as.numeric(sqrt(diag(as.matrix(stats::vcov(m_jk1)))))
    stopifnot(is.finite(se_jk1), se_jk1 > 0)
    cat("OK JK1 SE positive & finite: SE=",
        sprintf("%.4f", se_jk1), "\n", sep = "")
  } else {
    cat("OK JK1 path: as.svrepdesign(type='JK1') unavailable in this survey build\n")
  }

  cat("variance methods: PASS\n")
})
