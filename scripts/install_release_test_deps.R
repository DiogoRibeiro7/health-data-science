args <- commandArgs(trailingOnly = TRUE)

profile_path <- if (length(args) > 0L) args[[1L]] else "scripts/release_test_packages.txt"
repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos[["CRAN"]]), "@CRAN@")) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

if (!file.exists(profile_path)) {
  stop("Release test profile not found: ", profile_path, call. = FALSE)
}

lines <- trimws(readLines(profile_path, warn = FALSE))
packages <- lines[nzchar(lines) & !startsWith(lines, "#")]
packages <- unique(packages)

desc <- read.dcf("DESCRIPTION")

parse_field <- function(field) {
  value <- desc[1L, field]
  if (is.na(value) || !nzchar(value)) return(character())
  values <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  sub("[[:space:]]*\\(.*\\)$", "", values)
}

declared <- unique(c(
  parse_field("Imports"),
  parse_field("Suggests"),
  parse_field("Depends")
))
declared <- setdiff(declared, c("R", ""))

undeclared <- setdiff(packages, declared)
if (length(undeclared) > 0L) {
  stop(
    "Release test profile contains undeclared packages: ",
    paste(undeclared, collapse = ", "),
    call. = FALSE
  )
}

bioconductor <- intersect(packages, c("limma"))
cran <- setdiff(packages, bioconductor)

missing_cran <- cran[
  !vapply(cran, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran) > 0L) {
  install.packages(missing_cran, repos = repos)
}

if (length(bioconductor) > 0L) {
  missing_bioc <- bioconductor[
    !vapply(bioconductor, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_bioc) > 0L) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager", repos = repos)
    }
    BiocManager::install(
      missing_bioc,
      ask = FALSE,
      update = FALSE
    )
  }
}

still_missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing) > 0L) {
  stop(
    "Release test dependencies remain unavailable: ",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}

message(
  "Release test profile ready: ",
  length(packages),
  " packages available."
)
