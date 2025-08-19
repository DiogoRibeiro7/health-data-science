# Helper and modeling functions for regression analyses

load_lca_results <- function(data_path) {
  message("Loading LCA results from: ", data_path)
  require_data_file(data_path)
  load(data_path)
  lca.pufdata
}

fit_min_treatment_model <- function(df) {
  glm(mintreat ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
        CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
      data = df, family = "binomial")
}

fit_optimal_care_model <- function(df) {
  glm(optcare ~ LCAprofile + ANALYTIC_STAGE_GROUP + facility +
        CDCC_TOTAL_BEST + FACILITY_LOCATION_CD + YEAR_OF_DIAGNOSIS,
      data = df, family = "binomial")
}

run_regression <- function(data_path) {
  df <- load_lca_results(data_path)
  list(
    mintreat = fit_min_treatment_model(df),
    optcare = fit_optimal_care_model(df)
  )
}
