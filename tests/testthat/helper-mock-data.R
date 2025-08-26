mock_ncdb_data <- function(n = 100) {
  data.frame(
    PUF_CASE_ID = seq_len(n),
    optcare = sample(0:1, n, TRUE),
    SEX = sample(1:2, n, TRUE),
    FACILITY_TYPE_CD = sample(c(1:4, 9), n, TRUE),
    DX_RX_STARTED_DAYS = rpois(n, 1),
    CROWFLY = rpois(n, 5),
    CDCC_TOTAL_BEST = sample(0:3, n, TRUE),
    race3 = sample(1:3, n, TRUE),
    hispanic3 = sample(1:3, n, TRUE),
    urbandwell = sample(1:3, n, TRUE),
    age4 = sample(1:4, n, TRUE),
    SES = sample(1:3, n, TRUE),
    insurancetype = sample(1:6, n, TRUE),
    stringsAsFactors = FALSE
  )
}
