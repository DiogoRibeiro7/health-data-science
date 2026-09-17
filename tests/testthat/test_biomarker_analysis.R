test_that("fit_biomarker_analysis validates and fits an explicit pairwise contrast", {
  skip_if_not_installed("limma")

  set.seed(1)
  expression <- matrix(rnorm(12 * 8), nrow = 12)
  rownames(expression) <- paste0("gene_", seq_len(nrow(expression)))
  group <- factor(rep(c("control", "treated"), each = 4))

  expression[1, group == "treated"] <- expression[1, group == "treated"] + 3

  fit <- fit_biomarker_analysis(
    expression,
    group,
    reference = "control",
    comparison = "treated",
    alpha = 0.10,
    log2_fc_threshold = 0.5
  )

  expect_s3_class(fit, "hds_biomarker_analysis")
  expect_equal(fit$metadata$reference, "control")
  expect_equal(fit$metadata$comparison, "treated")
  expect_equal(fit$metadata$n_samples, 8)
  expect_equal(fit$metadata$n_features, 12)
  expect_true(all(c("feature", "significant", "adj.P.Val", "logFC") %in% names(fit$results)))
})

test_that("multi-group biomarker analysis requires an explicit pairwise comparison", {
  skip_if_not_installed("limma")

  expression <- matrix(rnorm(30), nrow = 5)
  rownames(expression) <- paste0("g", 1:5)
  group <- factor(rep(c("a", "b", "c"), each = 2))

  expect_error(
    fit_biomarker_analysis(expression, group),
    "specify both"
  )

  fit <- fit_biomarker_analysis(
    expression,
    group,
    reference = "a",
    comparison = "c"
  )
  expect_equal(fit$metadata$excluded_samples, 2)
  expect_equal(fit$metadata$reference_n, 2)
  expect_equal(fit$metadata$comparison_n, 2)
})

test_that("biomarker inputs are validated", {
  skip_if_not_installed("limma")

  expression <- matrix(rnorm(24), nrow = 6)
  group <- factor(rep(c("a", "b"), each = 2))

  expect_error(
    fit_biomarker_analysis(expression, group[-1]),
    "one value per expression sample"
  )

  bad <- expression
  bad[1, 1] <- Inf
  expect_error(
    fit_biomarker_analysis(bad, group),
    "finite values"
  )

  dup <- expression
  rownames(dup) <- c("x", "x", "a", "b", "c", "d")
  expect_error(
    fit_biomarker_analysis(dup, group),
    "unique"
  )

  too_small_group <- factor(c("a", "b", "b", "b"))
  rownames(expression) <- paste0("g", 1:6)
  expect_error(
    fit_biomarker_analysis(
      expression,
      too_small_group,
      reference = "a",
      comparison = "b"
    ),
    "at least two samples"
  )
})

test_that("tidy and summary biomarker helpers expose configured significance", {
  skip_if_not_installed("limma")

  set.seed(2)
  expression <- matrix(rnorm(20 * 10), nrow = 20)
  rownames(expression) <- paste0("gene_", seq_len(20))
  group <- factor(rep(c("control", "treated"), each = 5))
  expression[1:2, group == "treated"] <- expression[1:2, group == "treated"] + 4

  fit <- fit_biomarker_analysis(
    expression,
    group,
    reference = "control",
    comparison = "treated",
    alpha = 0.20,
    log2_fc_threshold = 1
  )

  tidy <- tidy_biomarker_analysis(
    fit,
    significant_only = TRUE,
    sort_by = "adjusted_p"
  )
  expect_true(all(tidy$significant))
  expect_true(all(tidy$reference == "control"))
  expect_true(all(tidy$comparison == "treated"))

  summary <- biomarker_analysis_summary(fit)
  expect_equal(summary$features_tested, 20)
  expect_equal(summary$significant, sum(fit$results$significant))
  expect_equal(
    summary$significant,
    summary$upregulated + summary$downregulated
  )
})

test_that("legacy biomarker_discovery keeps the historical topTable shape", {
  skip_if_not_installed("limma")

  set.seed(3)
  expression <- matrix(rnorm(15 * 6), nrow = 15)
  rownames(expression) <- paste0("gene_", seq_len(15))
  group <- factor(rep(c("a", "b"), each = 3))

  result <- biomarker_discovery(expression, group)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B") %in% names(result)))
  expect_lte(nrow(result), 10)
})
