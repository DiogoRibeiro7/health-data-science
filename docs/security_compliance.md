# Security and Compliance

This package includes security helpers intended to support deployments in regulated healthcare environments (e.g., HIPAA).

## Encryption
- **Encryption at rest** via `save_encrypted_rds()` and `read_encrypted_rds()` using AES-256.
- **Encryption in transit** using `run_secure_api()` to serve the Plumber API over HTTPS.

## Authentication and Authorisation
- API key verification and rotation with role enforcement through `verify_api_key()` and `rotate_api_key()`.
- Role checks supported by `check_role()` for implementing RBAC patterns.

## Audit Logging
- `audit_log()` captures user actions with timestamps.
- `track_lineage()` records data provenance for compliance audits.

## Data Privacy
- `pseudonymize_data()` hashes direct identifiers.
- `add_dp_noise()` applies differential privacy noise to numeric outputs.
- `enforce_retention_policy()` and `record_consent()` manage lifecycle and consent records.

## Security Scanning
All user inputs should be sanitised with `sanitize_input()` prior to processing. Database queries in this toolkit use parameterised interfaces to mitigate SQL injection.

These utilities are intended as building blocks; production deployments should integrate with enterprise key management, identity providers, and monitoring systems.
