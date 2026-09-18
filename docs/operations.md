# Operational Workflows

This guide outlines deployment, monitoring, and performance tuning for the health-data-science package.

## Deployment

1. **Environment setup**: `Rscript scripts/install_r_deps.R` to install dependencies declared in `DESCRIPTION`.
2. **Container build**: `docker build -t hds .` using the multi-stage Dockerfile.
3. **Kubernetes rollout**: apply manifests in `k8s/` or install the Helm chart in `helm/`.

### Troubleshooting
- Image fails health check: ensure `config/feature_flags.yaml` enables required services.

## Monitoring

The package exposes Prometheus metrics and health probes:
- `/metrics` for performance counters
- `/health/live` and `/health/ready` for liveness and readiness

### Troubleshooting
- Missing metrics: confirm `init_monitoring()` is called during start-up.

## Performance

- Use `run_parallel_lca()` for multi-core LCA fitting.
- Enable caching with `cache_set()` to avoid recomputation.

### Troubleshooting
- Slow ETL jobs: verify database indices and adjust `chunk_size` in `run_etl()`.
