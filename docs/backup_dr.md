# Backup and Disaster Recovery

## Backup strategy
- Terraform `infra/terraform` provisions an encrypted S3 bucket for automated backups
- Schedule database and model exports to the bucket using cron or cloud-native schedulers
- Enable versioning and lifecycle rules to retain historical copies

## Cross-region replication
- Configure the S3 bucket or storage system with replication rules to a secondary region
  for resilience against regional outages

## Disaster recovery testing
- Periodically restore backups into a staging environment to verify integrity
- Document recovery procedures and keep runbooks in version control

## Recovery objectives
- Define recovery time objective (RTO) and recovery point objective (RPO) based on
  organizational requirements. Use automated monitoring to ensure objectives are met.
