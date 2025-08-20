# ------------------------------------------------------------------------------
# File: config.R
# Purpose: Load and validate YAML configuration files for analysis workflows.
# Author: Diogo Ribeiro (ESMAD - Instituto Politécnico do Porto)
# Last Modified: 2025-03-??
# ------------------------------------------------------------------------------

#' Load configuration settings
#'
#' Reads a base configuration file and merges it with an environment-specific
#' override. Parameter types and ranges are validated before returning.
#'
#' @param env Environment name (e.g., "default", "development").
#' @param root Path to project root containing the `config` directory.
#'
#' @return A named list of configuration parameters.
#' @export
#'
#' @examples
#' cfg <- load_config("testing")
load_config <- function(env = "default", root = getwd()) {
  ensure_packages("yaml")
  cfg_dir <- file.path(root, "config")
  default_file <- file.path(cfg_dir, "default.yaml")
  if (!file.exists(default_file)) {
    stop("Default configuration file not found at ", default_file, call. = FALSE)
  }
  cfg <- yaml::read_yaml(default_file)
  env_file <- file.path(cfg_dir, paste0(env, ".yaml"))
  if (env != "default" && file.exists(env_file)) {
    env_cfg <- yaml::read_yaml(env_file)
    cfg <- utils::modifyList(cfg, env_cfg)
  }
  validate_config(cfg)
  cfg
}

#' Validate configuration parameter ranges and types
#'
#' Stops execution when configuration entries fall outside allowed ranges or
#' have incorrect types.
#'
#' @param cfg List returned by `load_config()`.
#'
#' @return Invisible `TRUE` when validation passes.
#' @export
#'
#' @examples
#' cfg <- load_config()
#' validate_config(cfg)
validate_config <- function(cfg) {
  if (!is.list(cfg)) {
    stop("Configuration must be a list", call. = FALSE)
  }
  # LCA checks
  if (!is.numeric(cfg$lca$nclass) || cfg$lca$nclass < 1) {
    stop("lca$nclass must be a positive number", call. = FALSE)
  }
  if (!is.numeric(cfg$lca$maxiter) || cfg$lca$maxiter < 1) {
    stop("lca$maxiter must be a positive number", call. = FALSE)
  }
  if (!is.numeric(cfg$lca$tol) || cfg$lca$tol <= 0) {
    stop("lca$tol must be a positive number", call. = FALSE)
  }
  if (!is.numeric(cfg$lca$nrep) || cfg$lca$nrep < 1) {
    stop("lca$nrep must be a positive number", call. = FALSE)
  }
  if (!is.logical(cfg$lca$auto_classes)) {
    stop("lca$auto_classes must be TRUE or FALSE", call. = FALSE)
  }
  # Regression checks
  if (!is.character(cfg$regression$family)) {
    stop("regression$family must be a character string", call. = FALSE)
  }
  # Survival checks
  if (!is.character(cfg$survival$method)) {
    stop("survival$method must be a character string", call. = FALSE)
  }
  invisible(TRUE)
}
