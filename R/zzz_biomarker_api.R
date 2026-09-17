# ------------------------------------------------------------------------------
# File: zzz_biomarker_api.R
# Purpose: Validated pairwise biomarker analysis with limma.
# ------------------------------------------------------------------------------

.hds_validate_expression_matrix <- function(expression, group) {
  if (is.data.frame(expression)) {
    numeric_columns <- vapply(expression, is.numeric, logical(1))
    if (!all(numeric_columns)) {
      stop("All expression columns must be numeric", call. = FALSE)
    }
    expression <- as.matrix(expression)
  }
  if (!is.matrix(expression) || !is.numeric(expression)) {
    stop("`expression` must be a numeric matrix or data frame", call. = FALSE)
  }
  if (nrow(expression) == 0L || ncol(expression) == 0L) {
    stop("`expression` must contain at least one feature and one sample", call. = FALSE)
  }
  if (length(group) != ncol(expression)) {
    stop("`group` must contain one value per expression sample", call. = FALSE)
  }
  if (anyNA(group)) {
    stop("`group` cannot contain missing values", call. = FALSE)
  }
  if (any(!is.finite(expression))) {
    stop("`expression` must contain only finite values", call. = FALSE)
  }

  feature_names <- rownames(expression)
  if (is.null(feature_names)) {
    feature_names <- paste0("feature_", seq_len(nrow(expression)))
    rownames(expression) <- feature_names
  }
  if (anyNA(feature_names) || any(!nzchar(feature_names)) || anyDuplicated(feature_names)) {
    stop("Expression row names must be unique, non-empty feature identifiers", call. = FALSE)
  }

  group <- factor(group)
  if (nlevels(group) < 2L) {
    stop("`group` must contain at least two observed groups", call. = FALSE)
  }

  list(expression = expression, group = group)
}

#' Fit an explicit pairwise biomarker analysis
#'
#' Fits a moderated linear model with [limma::lmFit] and [limma::eBayes] for one
#' explicit comparison group versus one explicit reference group. If exactly two
#' groups are present, the first factor level is the default reference and the
#' second is the default comparison. With more than two groups, both
#' `reference` and `comparison` must be supplied and other samples are excluded
#' from this pairwise analysis.
#'
#' Statistical significance is defined separately from ranking using an adjusted
#' p-value threshold and an optional absolute log2-fold-change threshold.
#'
#' @param expression Numeric matrix with features in rows and samples in columns.
#' @param group Group label for each sample.
#' @param reference Reference group level.
#' @param comparison Comparison group level; log2 fold change is comparison minus
#'   reference.
#' @param adjust_method Multiple-testing method accepted by [stats::p.adjust].
#' @param alpha Adjusted p-value threshold.
#' @param log2_fc_threshold Minimum absolute log2 fold change for the
#'   `significant` flag.
#' @param trend Whether to use intensity-trend empirical Bayes moderation.
#' @param robust Whether to use robust empirical Bayes moderation.
#'
#' @return Object of class `hds_biomarker_analysis`.
#' @export
fit_biomarker_analysis <- function(
    expression,
    group,
    reference = NULL,
    comparison = NULL,
    adjust_method = "BH",
    alpha = 0.05,
    log2_fc_threshold = 0,
    trend = FALSE,
    robust = FALSE) {
  validated <- .hds_validate_expression_matrix(expression, group)
  expression <- validated$expression
  group <- validated$group

  if (!is.character(adjust_method) || length(adjust_method) != 1L ||
      !adjust_method %in% stats::p.adjust.methods) {
    stop(
      paste("`adjust_method` must be one of:", paste(stats::p.adjust.methods, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1", call. = FALSE)
  }
  if (!is.numeric(log2_fc_threshold) || length(log2_fc_threshold) != 1L ||
      !is.finite(log2_fc_threshold) || log2_fc_threshold < 0) {
    stop("`log2_fc_threshold` must be a non-negative finite number", call. = FALSE)
  }
  if (!is.logical(trend) || length(trend) != 1L || is.na(trend) ||
      !is.logical(robust) || length(robust) != 1L || is.na(robust)) {
    stop("`trend` and `robust` must be TRUE or FALSE", call. = FALSE)
  }

  levels_present <- levels(group)
  scalar_level <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }

  if (is.null(reference) && is.null(comparison)) {
    if (length(levels_present) != 2L) {
      stop(
        "For more than two groups, specify both `reference` and `comparison`",
        call. = FALSE
      )
    }
    reference <- levels_present[1L]
    comparison <- levels_present[2L]
  } else {
    if (!scalar_level(reference) || !scalar_level(comparison)) {
      stop("`reference` and `comparison` must both be single group labels", call. = FALSE)
    }
    if (reference == comparison) {
      stop("`reference` and `comparison` must be different", call. = FALSE)
    }
    if (!reference %in% levels_present || !comparison %in% levels_present) {
      stop("Reference and comparison levels must be observed in `group`", call. = FALSE)
    }
  }

  keep <- as.character(group) %in% c(reference, comparison)
  pair_group <- factor(
    as.character(group[keep]),
    levels = c(reference, comparison)
  )
  pair_expression <- expression[, keep, drop = FALSE]
  group_counts <- table(pair_group)
  if (any(group_counts < 2L)) {
    stop("Each comparison group must contain at least two samples", call. = FALSE)
  }

  ensure_packages("limma")
  design <- stats::model.matrix(~ pair_group)
  colnames(design)[2L] <- paste0(comparison, "_vs_", reference)
  fit <- limma::lmFit(pair_expression, design)
  fit <- limma::eBayes(fit, trend = trend, robust = robust)

  table <- limma::topTable(
    fit,
    coef = 2L,
    number = Inf,
    adjust.method = adjust_method,
    sort.by = "none"
  )
  table$feature <- rownames(table)
  table$significant <- (
    table$adj.P.Val <= alpha &
      abs(table$logFC) >= log2_fc_threshold
  )
  rownames(table) <- NULL

  structure(
    list(
      fit = fit,
      design = design,
      results = table,
      metadata = list(
        reference = reference,
        comparison = comparison,
        contrast = paste0(comparison, " - ", reference),
        coefficient = colnames(design)[2L],
        adjust_method = adjust_method,
        alpha = alpha,
        log2_fc_threshold = log2_fc_threshold,
        trend = trend,
        robust = robust,
        n_features = nrow(pair_expression),
        n_samples = ncol(pair_expression),
        reference_n = unname(group_counts[[reference]]),
        comparison_n = unname(group_counts[[comparison]]),
        excluded_samples = sum(!keep)
      )
    ),
    class = "hds_biomarker_analysis"
  )
}

#' Tidy biomarker-analysis results
#'
#' @param model Object returned by [fit_biomarker_analysis].
#' @param significant_only Return only biomarkers meeting both configured
#'   adjusted-p and fold-change thresholds.
#' @param sort_by Ranking criterion.
#' @param n Maximum number of rows to return, or `Inf`.
#'
#' @return Data frame with one row per biomarker.
#' @export
tidy_biomarker_analysis <- function(
    model,
    significant_only = FALSE,
    sort_by = c("adjusted_p", "p_value", "abs_log2_fc", "none"),
    n = Inf) {
  if (!inherits(model, "hds_biomarker_analysis")) {
    stop("`model` must be created by `fit_biomarker_analysis()`", call. = FALSE)
  }
  if (!is.logical(significant_only) || length(significant_only) != 1L ||
      is.na(significant_only)) {
    stop("`significant_only` must be TRUE or FALSE", call. = FALSE)
  }
  sort_by <- match.arg(sort_by)
  if (!(identical(n, Inf) ||
        (is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 1 && n == floor(n)))) {
    stop("`n` must be a positive integer or Inf", call. = FALSE)
  }

  result <- model$results
  result$reference <- model$metadata$reference
  result$comparison <- model$metadata$comparison

  if (significant_only) {
    result <- result[result$significant, , drop = FALSE]
  }
  if (sort_by == "adjusted_p") {
    result <- result[order(result$adj.P.Val, result$P.Value), , drop = FALSE]
  } else if (sort_by == "p_value") {
    result <- result[order(result$P.Value), , drop = FALSE]
  } else if (sort_by == "abs_log2_fc") {
    result <- result[order(-abs(result$logFC), result$adj.P.Val), , drop = FALSE]
  }

  if (!identical(n, Inf)) {
    result <- utils::head(result, n)
  }
  rownames(result) <- NULL
  result
}

#' Summarise a biomarker analysis
#'
#' @param model Object returned by [fit_biomarker_analysis].
#' @return One-row data frame describing the comparison and number of discoveries.
#' @export
biomarker_analysis_summary <- function(model) {
  if (!inherits(model, "hds_biomarker_analysis")) {
    stop("`model` must be created by `fit_biomarker_analysis()`", call. = FALSE)
  }
  result <- model$results
  significant <- result$significant

  data.frame(
    reference = model$metadata$reference,
    comparison = model$metadata$comparison,
    reference_n = model$metadata$reference_n,
    comparison_n = model$metadata$comparison_n,
    excluded_samples = model$metadata$excluded_samples,
    features_tested = nrow(result),
    significant = sum(significant),
    upregulated = sum(significant & result$logFC > 0),
    downregulated = sum(significant & result$logFC < 0),
    adjust_method = model$metadata$adjust_method,
    alpha = model$metadata$alpha,
    log2_fc_threshold = model$metadata$log2_fc_threshold,
    row.names = NULL,
    check.names = FALSE
  )
}

#' Biomarker discovery using limma
#'
#' Backward-compatible helper returning the default top-ten [limma::topTable]
#' for coefficient 2 of a `~ group` design. New analyses should prefer
#' [fit_biomarker_analysis] so the pairwise comparison and significance rules are
#' explicit.
#'
#' @param expression Matrix of expression values (features x samples).
#' @param group Group labels for samples.
#' @return Data frame of top biomarkers in the historical `topTable()` format.
#' @export
biomarker_discovery <- function(expression, group) {
  validated <- .hds_validate_expression_matrix(expression, group)
  ensure_packages("limma")
  design <- stats::model.matrix(~ validated$group)
  fit <- limma::lmFit(validated$expression, design)
  fit <- limma::eBayes(fit)
  limma::topTable(fit, coef = 2L)
}
