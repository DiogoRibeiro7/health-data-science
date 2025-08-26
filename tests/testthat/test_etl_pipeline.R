source(file.path("..", "..", "R", "database.R"))
source(file.path("..", "..", "R", "etl.R"))

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")
skip_if_not_installed("pool")


test_that("run_etl moves data and logs lineage", {
  src_cfg <- list(dbms = "sqlite", dbname = tempfile())
  dest_cfg <- list(dbms = "sqlite", dbname = tempfile())
  src <- db_connect(src_cfg)
  dest <- db_connect(dest_cfg)
  on.exit({db_disconnect(src); db_disconnect(dest)})
  DBI::dbWriteTable(src, "x", data.frame(a = 1:5))
  lf <- tempfile(fileext = ".csv")
  run_etl(src, dest, "select * from x", "y", chunk_size = 2, lineage_file = lf)
  res <- DBI::dbReadTable(dest, "y")
  expect_equal(nrow(res), 5)
  expect_true(file.exists(lf))
})

