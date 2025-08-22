# Database Connectivity and ETL Pipelines

This package includes utilities for integrating with production healthcare
systems that store data in relational databases. Supported backends include
PostgreSQL, MySQL, SQL Server, Oracle, and SQLite for testing. Connections are
managed through pooled sessions to safely handle concurrent access and to reduce
connection overhead.

## Secure Connections

Credentials are not stored in code. `db_connect()` will read missing usernames
and passwords from the `DB_USER` and `DB_PASSWORD` environment variables. This
allows teams to manage secrets via external key stores to meet HIPAA security
requirements.

## Streaming ETL

`run_etl()` performs incremental extract–transform–load operations using
chunked queries so large tables can be processed without exhausting memory.
Transformations can be supplied as functions and are applied to each chunk prior
to loading into the destination table. Each ETL job records data lineage to a
CSV log for auditing.

## Data Lineage and Auditing

`record_data_lineage()` appends time-stamped entries describing the source,
transformation, and destination of each pipeline. These logs support compliance
reviews and reproducibility.

## Scalable Storage

Use `write_parquet_data()` and `read_parquet_data()` for analytics workflows
where columnar storage and compression are beneficial. Parquet datasets can be
partitioned for efficient querying and stored locally or on cloud object stores
such as Amazon S3, Azure Blob Storage, or Google Cloud Storage when the
appropriate credentials are configured.

