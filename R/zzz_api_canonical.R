# ------------------------------------------------------------------------------
# File: zzz_api_canonical.R
# Purpose: Canonical public definitions for APIs historically defined twice.
# ------------------------------------------------------------------------------

#' Incremental cost-effectiveness ratio
#'
#' Computes the incremental cost-effectiveness ratio (ICER) as the difference in
#' cost divided by the difference in effect. Historical package versions exposed
#' this function from two files with two different argument-name conventions.
#' The canonical API keeps the `cost1`/`effect1`/`cost2`/`effect2` names while
#' accepting the former `cost_a`/`effect_a`/`cost_b`/`effect_b` names through
#' `...` for backward compatibility.
#'
#' @param cost1 Cost for intervention 1.
#' @param effect1 Effect for intervention 1.
#' @param cost2 Cost for intervention 2.
#' @param effect2 Effect for intervention 2.
#' @param ... Historical aliases `cost_a`, `effect_a`, `cost_b`, and `effect_b`.
#'
#' @return Numeric ICER value or vector.
#' @export
cost_effectiveness <- function(
    cost1 = NULL,
    effect1 = NULL,
    cost2 = NULL,
    effect2 = NULL,
    ...) {
  dots <- list(...)
  if (length(dots) > 0L && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Additional arguments must use historical alias names", call. = FALSE)
  }

  allowed_aliases <- c("cost_a", "effect_a", "cost_b", "effect_b")
  unknown <- setdiff(names(dots), allowed_aliases)
  if (length(unknown) > 0L) {
    stop(
      paste("Unknown argument(s):", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }

  values <- list(
    cost1 = cost1,
    effect1 = effect1,
    cost2 = cost2,
    effect2 = effect2
  )
  aliases <- c(
    cost_a = "cost1",
    effect_a = "effect1",
    cost_b = "cost2",
    effect_b = "effect2"
  )

  for (alias in names(aliases)) {
    if (!is.null(dots[[alias]])) {
      target <- aliases[[alias]]
      if (!is.null(values[[target]])) {
        stop(
          sprintf("Specify either `%s` or `%s`, not both", target, alias),
          call. = FALSE
        )
      }
      values[[target]] <- dots[[alias]]
    }
  }

  missing <- names(values)[vapply(values, is.null, logical(1))]
  if (length(missing) > 0L) {
    stop(
      paste("Missing required argument(s):", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  if (any(!vapply(values, is.numeric, logical(1)))) {
    stop("Costs and effects must be numeric", call. = FALSE)
  }

  (values$cost1 - values$cost2) / (values$effect1 - values$effect2)
}
