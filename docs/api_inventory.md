# Public API Inventory

This inventory is generated from the current `NAMESPACE` for the 0.3.0 release candidate.

- Exported functions: **233**
- Duplicate export directives: **0**
- Deprecated compatibility wrappers: **6**

## Modern statistical API

- `aggregate_group_time_did()`
- `bayesian_model_averaging()`
- `bayesian_sampling_diagnostics()`
- `biomarker_analysis_summary()`
- `clinical_prediction_performance()`
- `cost_effectiveness()`
- `did_pretrend_diagnostic()`
- `estimate_doubly_robust()`
- `estimate_propensity_effect()`
- `estimate_state_occupation()`
- `find_delta_tipping_point()`
- `fit_bayesian_lca()`
- `fit_biomarker_analysis()`
- `fit_clinical_prediction_rule()`
- `fit_competing_risks()`
- `fit_frailty_cox()`
- `fit_group_time_did()`
- `fit_hierarchical_bayes()`
- `fit_instrumental_variable()`
- `fit_latent_transition()`
- `fit_longitudinal_mixed()`
- `fit_mixture_covariates()`
- `fit_multilevel_lca()`
- `fit_multistate_cox()`
- `fit_propensity_design()`
- `fit_recurrent_events()`
- `fit_regression_discontinuity()`
- `fit_time_varying_cox()`
- `instrumental_variable_diagnostics()`
- `landmark_cox()`
- `latent_model_metadata()`
- `longitudinal_variance_components()`
- `pharmacovigilance_signal()`
- `posterior_predictive_check()`
- `posterior_predictive_draws()`
- `propensity_balance()`
- `propensity_overlap()`
- `rd_bandwidth_sensitivity()`
- `rd_covariate_balance()`
- `rd_density_diagnostic()`
- `rd_placebo_cutoffs()`
- `run_delta_sensitivity()`
- `run_mcmc()`
- `statistical_learning_metadata()`
- `statistical_learning_performance()`
- `tidy_bayesian_model_average()`
- `tidy_bayesian_posterior()`
- `tidy_biomarker_analysis()`
- `tidy_clinical_prediction_rule()`
- `tidy_competing_risks()`
- `tidy_cox_model()`
- `tidy_group_time_did()`
- `tidy_instrumental_variable()`
- `tidy_longitudinal_mixed()`
- `tidy_multistate_cox()`
- `tidy_recurrent_events()`
- `tidy_regression_discontinuity()`
- `train_gbm()`
- `train_neural_net()`
- `train_rf_missing()`
- `validate_transition_structure()`

## Deprecated causal compatibility wrappers

These remain exported during the migration window but are not the preferred API:

- `difference_in_differences()`
- `estimate_causal_effect()`
- `instrumental_variable()`
- `match_cohort()`
- `propensity_stratification()`
- `regression_discontinuity()`

See [causal_api_migration.md](causal_api_migration.md) for replacements.

## Complete export inventory

```text
ab_test_models
active_comparator
add_dp_noise
add_interaction_terms
add_polynomial_features
aggregate_group_time_did
analyze_prom
analyze_residuals
analyze_trends
assess_data_quality
audit_log
auto_feature_engineering
auto_select_lca
batch_predict
bayesian_model_averaging
bayesian_sampling_diagnostics
bias_sensitivity
biomarker_analysis_summary
biomarker_discovery
bootstrap_ci
calculate_kpis
case_control_match
check_rate_limit
check_role
class_assignment_metrics
clinical_prediction_performance
cohort_time_to_event
collect_diagnostics
compare_models
compute_shap
confounding_adjust
containerize_model
cost_effectiveness
cross_sectional_survey
cross_validate
dashboard_status
data_completeness
data_validate
db_connect
db_disconnect
db_query
decrypt_data
deploy_model
detect_anomalies
detect_model_drift
detect_outliers
detect_outliers_iqr
detect_pharmacovigilance
detect_significance
develop_clinical_prediction_rule
did_pretrend_diagnostic
difference_in_differences
disease_map
drug_exposure_summary
ecological_spatial_analysis
ema_pico
emit_event
encrypt_data
end_trace
enforce_retention_policy
ensure_dir
ensure_packages
estimate_causal_effect
estimate_doubly_robust
estimate_propensity_effect
estimate_state_occupation
export_data
fda_rwe_ready
feature_importance
find_delta_tipping_point
fit_bayesian_lca
fit_biomarker_analysis
fit_clinical_prediction_rule
fit_competing_risks
fit_frailty_cox
fit_group_time_did
fit_hierarchical_bayes
fit_instrumental_variable
fit_latent_transition
fit_longitudinal_mixed
fit_mixture_covariates
fit_multilevel_lca
fit_multistate_cox
fit_propensity_design
fit_recurrent_events
fit_regression_discontinuity
fit_survival_model
fit_time_series
fit_time_varying_cox
forest_plot
format_table
generate_executive_summary
generate_key
generate_narrative
generate_report
get_job_status
gis_join
goodness_of_fit
grid_search
gvp_compliance
handle_outliers
hash_api_key
health_check
health_disparity_index
health_outcomes
hrqol_model
impute_missing
init_logging
init_monitoring
instrumental_variable
instrumental_variable_diagnostics
kfold_cv
landmark_cox
latent_model_metadata
launch_api
launch_app
lca_stability
load_config
local_explanation
log_audit
log_debug
log_error
log_info
log_model_metrics
log_performance
log_warn
longitudinal_variance_components
make_interactive
match_cohort
memory_usage
model_air_pollution
monitor_resources
multiple_treatment_compare
nlp_extract_concepts
omop_standardize
optimal_lca_classes
paginate
parameter_sweep
partial_dependence_data
patient_preference
pharmacovigilance_signal
phenotype_diabetes
plot_geo
plot_interactive
plot_network
plot_profiles
plot_timeseries
posterior_predictive_check
posterior_predictive_draws
posterior_probabilities
power_observational
prepare_lca_data
propensity_balance
propensity_overlap
propensity_stratification
pseudonymize_data
quality_of_life_index
rd_bandwidth_sensitivity
rd_covariate_balance
rd_density_diagnostic
rd_placebo_cutoffs
read_csv_chunked
read_csv_safely
read_data_source
read_encrypted_rds
read_parquet_data
record_consent
record_data_lineage
record_data_version
record_metric
register_model
register_webhook
regression_discontinuity
render_report
require_data_file
rotate_api_key
run_delta_sensitivity
run_etl
run_lca
run_mcmc
run_regression
run_secure_api
run_transform_pipeline
sample_size_nonrandom
sanitize_input
save_encrypted_rds
schedule_report
score_model_api
secure_tempfile
select_cohort
select_important_features
send_alert
set_log_level
set_log_user
share_report
simulate_sir
spatial_cluster_scan
start_trace
statistical_learning_metadata
statistical_learning_performance
strobe_compliant
submit_job
syndromic_surveillance
temporal_consistency
theme_publication
tidy_bayesian_model_average
tidy_bayesian_posterior
tidy_biomarker_analysis
tidy_clinical_prediction_rule
tidy_competing_risks
tidy_cox_model
tidy_group_time_did
tidy_instrumental_variable
tidy_longitudinal_mixed
tidy_multistate_cox
tidy_recurrent_events
tidy_regression_discontinuity
to_fhir_patient
to_sparse_matrix
track_lineage
train_ensemble
train_gbm
train_neural_net
train_rf_missing
validate_config
validate_lca_data
validate_model
validate_oidc_token
validate_schema
validate_transition_structure
variable_network
verify_api_key
write_parquet_data
```

## Stability note

Presence in this inventory means the function is currently exported in 0.3.0. It does not by itself guarantee inclusion in the eventual 1.0 stable API. Deprecated wrappers are explicitly marked above; further removals should follow semantic-versioning and documented migration rules.
