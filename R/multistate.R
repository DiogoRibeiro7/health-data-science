# ------------------------------------------------------------------------------
# File: multistate.R
# Purpose: Multi-state survival models and Aalen-Johansen state occupation.
# ------------------------------------------------------------------------------

#' Validate and normalise a multi-state transition structure
#'
#' @param transitions Data frame with `from` and `to` columns and, optionally,
#'   a `label` column.
#' @return A normalised transition data frame.
#' @export
validate_transition_structure <- function(transitions) {
  if (!is.data.frame(transitions)) stop("`transitions` must be a data frame", call. = FALSE)
  if (!all(c("from", "to") %in% names(transitions))) {
    stop("`transitions` must contain `from` and `to` columns", call. = FALSE)
  }
  if (nrow(transitions) == 0L) stop("`transitions` must contain at least one allowed transition", call. = FALSE)
  from <- as.character(transitions$from)
  to <- as.character(transitions$to)
  if (anyNA(from) || anyNA(to) || any(!nzchar(from)) || any(!nzchar(to))) {
    stop("Transition states cannot be missing or empty", call. = FALSE)
  }
  if (any(from == to)) stop("Self-transitions are not allowed", call. = FALSE)
  key <- paste(from, to, sep = "\r")
  if (anyDuplicated(key)) stop("Duplicate transitions are not allowed", call. = FALSE)
  label <- if ("label" %in% names(transitions)) as.character(transitions$label) else paste(from, to, sep = " -> ")
  if (anyNA(label) || any(!nzchar(label))) stop("Transition labels cannot be missing or empty", call. = FALSE)
  out <- data.frame(
    transition_id = seq_len(nrow(transitions)), from = from, to = to, label = label,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  class(out) <- c("hds_transition_structure", "data.frame")
  out
}

.prepare_multistate_data <- function(data, start, stop, from, to, event, id,
                                     transitions, covariates = character(),
                                     na_action = c("fail", "omit"),
                                     require_event = FALSE) {
  na_action <- match.arg(na_action)
  transition_table <- validate_transition_structure(transitions)
  if (!is.data.frame(data)) stop("`data` must be a data frame", call. = FALSE)

  scalar_name <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  core <- list(start = start, stop = stop, from = from, to = to, event = event, id = id)
  if (!all(vapply(core, scalar_name, logical(1)))) {
    stop("Core arguments must be single column names", call. = FALSE)
  }
  if (!is.character(covariates) || anyNA(covariates) || any(!nzchar(covariates)) || anyDuplicated(covariates)) {
    stop("`covariates` must be unique column names", call. = FALSE)
  }

  required <- unique(c(start, stop, from, to, event, id, covariates))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }

  if (!is.numeric(data[[start]]) || !is.numeric(data[[stop]])) stop("Start and stop times must be numeric", call. = FALSE)
  if (any(!is.finite(data[[start]]), na.rm = TRUE) || any(!is.finite(data[[stop]]), na.rm = TRUE)) {
    stop("Start and stop times must contain only finite values or NA", call. = FALSE)
  }
  if (any(data[[start]] < 0, na.rm = TRUE)) stop("Start times cannot be negative", call. = FALSE)
  bad_interval <- !is.na(data[[start]]) & !is.na(data[[stop]]) & data[[stop]] <= data[[start]]
  if (any(bad_interval)) stop("Every interval must satisfy `stop > start`", call. = FALSE)

  if (!is.numeric(data[[event]]) && !is.logical(data[[event]])) stop("The event indicator must be numeric or logical", call. = FALSE)
  ev <- unique(data[[event]][!is.na(data[[event]])])
  if (!all(ev %in% c(0, 1, FALSE, TRUE))) stop("The event indicator must contain only 0 and 1", call. = FALSE)
  if (anyNA(data[[id]])) stop("Subject identifiers cannot be missing", call. = FALSE)

  if (length(covariates)) {
    unsupported <- vapply(data[covariates], function(x) !(is.numeric(x) || is.logical(x) || is.factor(x)), logical(1))
    if (any(unsupported)) stop("Covariates must be numeric, logical, or factor variables", call. = FALSE)
  }

  event_rows <- !is.na(data[[event]]) & as.integer(data[[event]]) == 1L
  missing_rows <- is.na(data[[start]]) | is.na(data[[stop]]) | is.na(data[[from]]) |
    is.na(data[[event]]) | (event_rows & is.na(data[[to]]))
  if (length(covariates)) missing_rows <- missing_rows | !stats::complete.cases(data[covariates])
  omitted_rows <- sum(missing_rows)
  if (omitted_rows) {
    if (na_action == "fail") stop("Missing values detected; use `na_action = \"omit\"`", call. = FALSE)
    data <- data[!missing_rows, , drop = FALSE]
  }
  if (!nrow(data)) stop("No complete observations remain", call. = FALSE)

  data[[from]] <- as.character(data[[from]])
  data[[to]] <- as.character(data[[to]])
  states <- unique(c(transition_table$from, transition_table$to))
  if (any(!data[[from]] %in% states)) stop("Observed origin states must belong to the transition structure", call. = FALSE)

  event_flag <- as.integer(data[[event]])
  observed <- event_flag == 1L
  if (any(observed & !data[[to]] %in% states)) stop("Observed destination states must belong to the transition structure", call. = FALSE)
  if (any(observed & data[[from]] == data[[to]])) stop("Observed events must change state", call. = FALSE)
  allowed <- paste(transition_table$from, transition_table$to, sep = "\r")
  observed_keys <- paste(data[[from]][observed], data[[to]][observed], sep = "\r")
  if (any(!observed_keys %in% allowed)) stop("Observed event transition is not allowed by `transitions`", call. = FALSE)

  non_event_change <- event_flag == 0L & !is.na(data[[to]]) & nzchar(data[[to]]) & data[[to]] != data[[from]]
  if (any(non_event_change)) stop("Rows with `event = 0` cannot change state", call. = FALSE)

  subject_rows <- split(seq_len(nrow(data)), data[[id]])
  for (rows in subject_rows) {
    rows <- rows[order(data[[start]][rows], data[[stop]][rows])]
    if (length(rows) > 1L) {
      if (any(data[[start]][rows[-1L]] < data[[stop]][rows[-length(rows)]])) {
        stop("Intervals cannot overlap within a subject", call. = FALSE)
      }
      for (j in seq_len(length(rows) - 1L)) {
        current <- rows[j]
        next_row <- rows[j + 1L]
        if (data[[start]][next_row] == data[[stop]][current]) {
          expected <- if (event_flag[current] == 1L) data[[to]][current] else data[[from]][current]
          if (data[[from]][next_row] != expected) {
            stop("Contiguous intervals must preserve state history", call. = FALSE)
          }
        }
      }
    }
  }

  event_count <- sum(event_flag)
  if (require_event && event_count == 0L) stop("At least one state transition event is required", call. = FALSE)
  list(data = data, transitions = transition_table, omitted_rows = omitted_rows,
       event_count = event_count, states = states)
}

#' Fit transition-specific cause-specific Cox models
#'
#' @param data Multi-state interval data.
#' @param start,stop,from,to,event,id Column names.
#' @param transitions Allowed transition structure.
#' @param covariates Character vector of covariate names.
#' @param na_action Missing-value policy.
#' @return An `hds_multistate_cox` object.
#' @export
fit_multistate_cox <- function(data, start, stop, from, to, event, id,
                               transitions, covariates = character(),
                               na_action = c("fail", "omit")) {
  prepared <- .prepare_multistate_data(
    data, start, stop, from, to, event, id, transitions,
    covariates = covariates, na_action = na_action, require_event = TRUE
  )
  data <- prepared$data
  transition_table <- prepared$transitions
  ensure_packages("survival")

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  response <- paste0("survival::Surv(", quote_name(start), ", ", quote_name(stop), ", .hds_status)")
  rhs <- if (length(covariates)) paste(vapply(covariates, quote_name, character(1)), collapse = " + ") else "1"
  rhs <- paste0(rhs, " + cluster(", quote_name(id), ")")
  formula <- stats::as.formula(paste(response, "~", rhs))

  models <- vector("list", nrow(transition_table))
  names(models) <- as.character(transition_table$transition_id)
  transition_events <- integer(nrow(transition_table))
  risk_rows <- integer(nrow(transition_table))

  for (i in seq_len(nrow(transition_table))) {
    tr <- transition_table[i, , drop = FALSE]
    risk_data <- data[data[[from]] == tr$from, , drop = FALSE]
    risk_rows[i] <- nrow(risk_data)
    if (!nrow(risk_data)) {
      warning(paste0("No risk intervals for transition ", tr$label), call. = FALSE)
      next
    }
    risk_data$.hds_status <- as.integer(as.integer(risk_data[[event]]) == 1L & risk_data[[to]] == tr$to)
    transition_events[i] <- sum(risk_data$.hds_status)
    if (!transition_events[i]) {
      warning(paste0("No observed events for transition ", tr$label), call. = FALSE)
      next
    }
    fit <- survival::coxph(formula, data = risk_data, ties = "efron", model = TRUE)
    attr(fit, "healthdatascience_transition") <- tr
    models[[i]] <- fit
  }

  transition_table$risk_rows <- risk_rows
  transition_table$event_count <- transition_events
  estimable <- !vapply(models, is.null, logical(1))
  structure(
    list(
      models = models,
      transitions = transition_table,
      metadata = list(
        n_rows = nrow(data), n_subjects = length(unique(data[[id]])),
        event_count = prepared$event_count, omitted_rows = prepared$omitted_rows,
        states = prepared$states,
        estimable_transitions = transition_table$transition_id[estimable],
        start = start, stop = stop, from = from, to = to, event = event,
        id = id, covariates = covariates
      )
    ),
    class = "hds_multistate_cox"
  )
}

#' Summarise transition-specific multi-state Cox models
#'
#' @param model Object returned by [fit_multistate_cox].
#' @param conf_level Confidence level.
#' @return Data frame of cause-specific hazard-ratio summaries.
#' @export
tidy_multistate_cox <- function(model, conf_level = 0.95) {
  if (!inherits(model, "hds_multistate_cox")) stop("`model` must be created by `fit_multistate_cox()`", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1", call. = FALSE)
  }
  zcrit <- stats::qnorm(1 - (1 - conf_level) / 2)
  tables <- lapply(seq_along(model$models), function(i) {
    fit <- model$models[[i]]
    if (is.null(fit)) return(NULL)
    b <- stats::coef(fit)
    if (!length(b)) return(NULL)
    se <- sqrt(diag(stats::vcov(fit)))
    z <- b / se
    tr <- model$transitions[i, , drop = FALSE]
    data.frame(
      transition_id = tr$transition_id, from = tr$from, to = tr$to,
      transition = tr$label, term = names(b), estimate = unname(b),
      std_error = unname(se), statistic = unname(z),
      p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      cause_specific_hazard_ratio = exp(unname(b)),
      conf_low = exp(unname(b) - zcrit * unname(se)),
      conf_high = exp(unname(b) + zcrit * unname(se)),
      row.names = NULL, check.names = FALSE
    )
  })
  tables <- Filter(Negate(is.null), tables)
  if (!length(tables)) return(data.frame())
  do.call(rbind, tables)
}

#' Estimate state-occupation probabilities with Aalen-Johansen
#'
#' @param data Multi-state interval data.
#' @param start,stop,from,to,event,id Column names.
#' @param transitions Allowed transition structure.
#' @param times Optional times for returned probabilities.
#' @param origin Initial time.
#' @param na_action Missing-value policy.
#' @return Long data frame with time, state, and probability.
#' @export
estimate_state_occupation <- function(data, start, stop, from, to, event, id,
                                      transitions, times = NULL, origin = 0,
                                      na_action = c("fail", "omit")) {
  if (!is.numeric(origin) || length(origin) != 1L || !is.finite(origin) || origin < 0) {
    stop("`origin` must be a finite non-negative number", call. = FALSE)
  }
  prepared <- .prepare_multistate_data(
    data, start, stop, from, to, event, id, transitions,
    covariates = character(), na_action = na_action, require_event = FALSE
  )
  data <- prepared$data
  transition_table <- prepared$transitions
  states <- prepared$states
  event_flag <- as.integer(data[[event]])

  if (is.null(times)) {
    event_times <- data[[stop]][event_flag == 1L & data[[stop]] >= origin]
    times <- sort(unique(c(origin, event_times, max(data[[stop]]))))
  } else {
    if (!is.numeric(times) || anyNA(times) || any(!is.finite(times))) stop("`times` must contain finite numeric values", call. = FALSE)
    if (any(times < origin)) stop("All `times` must be >= `origin`", call. = FALSE)
    times <- sort(unique(times))
  }

  ids <- unique(data[[id]])
  initial_rows <- which(data[[start]] <= origin & data[[stop]] > origin)
  initial_by_id <- split(initial_rows, data[[id]][initial_rows])
  if (length(initial_rows) == 0L || any(vapply(initial_by_id, length, integer(1)) != 1L) ||
      !setequal(names(initial_by_id), as.character(ids))) {
    stop("Every subject must have exactly one observed state at `origin`", call. = FALSE)
  }

  initial_states <- data[[from]][initial_rows]
  initial_probability <- table(factor(initial_states, levels = states)) / length(ids)
  transition_matrix <- diag(length(states))
  dimnames(transition_matrix) <- list(states, states)
  occupation <- as.numeric(initial_probability)
  names(occupation) <- states

  result <- vector("list", length(times))
  idx <- 1L
  record <- function(time, probability) data.frame(
    time = rep(time, length(states)), state = states,
    probability = as.numeric(probability), stringsAsFactors = FALSE
  )

  event_times <- sort(unique(data[[stop]][event_flag == 1L & data[[stop]] > origin & data[[stop]] <= max(times)]))
  for (event_time in event_times) {
    while (idx <= length(times) && times[idx] < event_time) {
      result[[idx]] <- record(times[idx], occupation)
      idx <- idx + 1L
    }
    increment <- matrix(0, nrow = length(states), ncol = length(states), dimnames = list(states, states))
    for (state in states) {
      at_risk <- data[[from]] == state & data[[start]] < event_time & data[[stop]] >= event_time
      risk_count <- length(unique(data[[id]][at_risk]))
      if (!risk_count) next
      outgoing <- transition_table[transition_table$from == state, , drop = FALSE]
      total_events <- 0L
      for (j in seq_len(nrow(outgoing))) {
        destination <- outgoing$to[j]
        d <- sum(event_flag == 1L & data[[stop]] == event_time & data[[from]] == state & data[[to]] == destination)
        increment[state, destination] <- d / risk_count
        total_events <- total_events + d
      }
      increment[state, state] <- -total_events / risk_count
    }
    transition_matrix <- transition_matrix %*% (diag(length(states)) + increment)
    occupation <- as.numeric(initial_probability) %*% transition_matrix
    occupation <- as.numeric(occupation)
    names(occupation) <- states
    while (idx <= length(times) && times[idx] == event_time) {
      result[[idx]] <- record(times[idx], occupation)
      idx <- idx + 1L
    }
  }
  while (idx <= length(times)) {
    result[[idx]] <- record(times[idx], occupation)
    idx <- idx + 1L
  }
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  attr(out, "healthdatascience") <- list(
    estimator = "aalen-johansen", origin = origin,
    n_subjects = length(ids), states = states,
    transitions = transition_table, omitted_rows = prepared$omitted_rows
  )
  out
}
