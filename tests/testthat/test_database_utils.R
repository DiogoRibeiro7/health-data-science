source(file.path("..", "..", "R", "database.R"))

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")
skip_if_not_installed("pool")


test_that("db_connect establishes a pool and db_query retrieves data", {
  cfg <- list(dbms = "sqlite", dbname = tempfile())
  pool <- db_connect(cfg)
  on.exit(db_disconnect(pool))
  DBI::dbWriteTable(pool, "patients", data.frame(id = 1:3, val = letters[1:3]))
  res <- db_query(pool, "select * from patients")
  expect_equal(nrow(res), 3)
})

