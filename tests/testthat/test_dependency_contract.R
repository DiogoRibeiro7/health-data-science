test_that("ensure_packages accepts available namespaces without attaching them", {
  was_attached <- "package:stats" %in% search()

  expect_true(ensure_packages("stats"))

  expect_equal("package:stats" %in% search(), was_attached)
})

test_that("ensure_packages reports missing optional packages without installing", {
  missing <- "healthdatascience_package_that_does_not_exist"

  expect_error(
    ensure_packages(missing),
    paste0(
      "Missing optional package: ",
      missing,
      ". Install with: install.packages"
    ),
    fixed = TRUE
  )

  expect_false(requireNamespace(missing, quietly = TRUE))
})

test_that("DESCRIPTION keeps the hard dependency surface small", {
  desc <- read.dcf("DESCRIPTION")
  imports <- trimws(strsplit(desc[1, "Imports"], ",", fixed = TRUE)[[1]])
  imports <- imports[nzchar(imports)]

  expect_setequal(imports, c("logger", "yaml", "uuid", "jsonlite"))
})


test_that("script CLI dependency is declared", {
  desc <- read.dcf("DESCRIPTION")
  suggests <- trimws(strsplit(desc[1, "Suggests"], ",", fixed = TRUE)[[1]])
  suggests <- sub("[[:space:]]*\\(.*\\)$", "", suggests)

  expect_true("optparse" %in% suggests)
})
