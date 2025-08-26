# Deployment and CI/CD

This package includes a production-ready pipeline and infrastructure templates.

## CI/CD
- GitHub Actions matrix tests across R 4.2 and 4.3
- Coverage gating and linters
- Container build with vulnerability scanning via Trivy
- Optional image publishing for releases

## Kubernetes and Helm
- `k8s/` contains raw manifests with resource limits, health probes, and rolling update strategy
- `helm/health-data-science` provides a Helm chart for repeatable deployments and enables
  blue-green or canary rollouts by adjusting replica counts or image tags

## Terraform
- `infra/terraform` defines cloud storage for encrypted backups and can be expanded
  to provision clusters or databases

## Deployment strategies
- Blue-green and canary deployments can be achieved by running parallel Helm releases
  and gradually shifting traffic
- Rollbacks are handled by retaining previous releases and image tags
- Feature flags may be managed by editing configuration in `config/` and reloading the service

## Container optimization
- Multi-stage Dockerfile results in smaller runtime images and runs tests during build
- `.dockerignore` reduces build context
- Trivy scanning in CI guards against vulnerabilities

## Developer workflow
- `scripts/setup_dev.sh` bootstraps a local environment with `renv` and pre-commit hooks
- All environments share the same configuration files for parity between development, staging,
  and production
