failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

read_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("`", "", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 _-]", "", x)
  x <- gsub("[[:space:]]+", "-", trimws(x))
  gsub("-+", "-", x)
}

markdown_anchors <- function(path) {
  lines <- read_lines(path)
  headings <- sub("^#{1,6}[[:space:]]+", "", lines[grepl("^#{1,6}[[:space:]]+", lines)])
  vapply(headings, slugify, character(1))
}

check_markdown_links <- function(path) {
  lines <- read_lines(path)
  matches <- regmatches(
    lines,
    gregexpr("\\[[^]]+\\]\\([^)]+\\)", lines, perl = TRUE)
  )
  links <- unlist(matches, use.names = FALSE)
  if (length(links) == 0L) {
    return(invisible(NULL))
  }

  targets <- sub("^.*\\(([^)]+)\\)$", "\\1", links)
  targets <- targets[
    !grepl("^(https?://|mailto:|#)", targets)
  ]

  for (target in targets) {
    parts <- strsplit(target, "#", fixed = TRUE)[[1]]
    relative <- parts[[1L]]
    anchor <- if (length(parts) > 1L) parts[[2L]] else ""

    resolved <- if (nzchar(relative)) {
      file.path(dirname(path), relative)
    } else {
      path
    }

    exists <- file.exists(resolved) || dir.exists(resolved)
    check(
      exists,
      sprintf("%s contains missing local link target: %s", path, target)
    )

    if (exists && nzchar(anchor) && file.exists(resolved) &&
        grepl("\\.md$", resolved, ignore.case = TRUE)) {
      anchors <- markdown_anchors(resolved)
      check(
        anchor %in% anchors,
        sprintf("%s contains missing Markdown anchor: %s", path, target)
      )
    }
  }

  invisible(NULL)
}

description <- read.dcf("DESCRIPTION")
version <- unname(description[1L, "Version"])
check(identical(version, "0.3.0"), "DESCRIPTION version must be 0.3.0")

namespace <- read_lines("NAMESPACE")
export_lines <- namespace[grepl("^export\\([^)]+\\)$", namespace)]
exports <- sub("^export\\(([^)]+)\\)$", "\\1", export_lines)
check(
  length(exports) == length(unique(exports)),
  "NAMESPACE contains duplicate export directives"
)

inventory <- read_lines("docs/api_inventory.md")
count_line <- inventory[grepl("^- Exported functions: \\*\\*[0-9]+\\*\\*$", inventory)]
check(length(count_line) == 1L, "API inventory export count line is missing or ambiguous")

if (length(count_line) == 1L) {
  inventory_count <- as.integer(sub(
    "^- Exported functions: \\*\\*([0-9]+)\\*\\*$",
    "\\1",
    count_line
  ))
  check(
    identical(inventory_count, length(exports)),
    sprintf(
      "API inventory count (%d) does not match NAMESPACE exports (%d)",
      inventory_count,
      length(exports)
    )
  )
}

heading <- match("## Complete export inventory", inventory)
check(!is.na(heading), "API inventory is missing the complete export inventory section")

if (!is.na(heading)) {
  after_heading <- seq.int(heading + 1L, length(inventory))
  fence_start_candidates <- after_heading[inventory[after_heading] == "```text"]
  if (length(fence_start_candidates) == 0L) {
    failures <- c(failures, "API inventory complete export code block is missing")
  } else {
    fence_start <- fence_start_candidates[[1L]]
    closing_candidates <- seq.int(fence_start + 1L, length(inventory))
    closing_candidates <- closing_candidates[inventory[closing_candidates] == "```"]

    if (length(closing_candidates) == 0L) {
      failures <- c(failures, "API inventory complete export code block is unterminated")
    } else {
      fence_end <- closing_candidates[[1L]]
      inventory_exports <- inventory[seq.int(fence_start + 1L, fence_end - 1L)]
      inventory_exports <- inventory_exports[nzchar(inventory_exports)]

      check(
        identical(sort(exports), sort(inventory_exports)),
        "API inventory export list does not match NAMESPACE"
      )
    }
  }
}

deprecated <- c(
  "difference_in_differences",
  "estimate_causal_effect",
  "instrumental_variable",
  "match_cohort",
  "propensity_stratification",
  "regression_discontinuity"
)
missing_deprecated <- setdiff(deprecated, exports)
check(
  length(missing_deprecated) == 0L,
  paste(
    "Deprecated migration wrappers missing from NAMESPACE:",
    paste(missing_deprecated, collapse = ", ")
  )
)

changelog <- paste(read_lines("CHANGELOG.md"), collapse = "\n")
check(
  grepl("0\\.3\\.0", changelog),
  "CHANGELOG does not mention the 0.3.0 release line"
)

check_markdown_links("README.md")
check_markdown_links("docs/README.md")

if (length(failures) > 0L) {
  cat("Release-surface validation failed:\n")
  cat(paste0("- ", failures, "\n"))
  quit(status = 1L)
}

cat(sprintf(
  "Release surface OK: version %s, %d unique exports, docs links valid.\n",
  version,
  length(exports)
))
