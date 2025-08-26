# Performance Considerations

The package includes several features to help manage performance when working
with large health datasets:

* **Progress Bars** – enable `show_progress = TRUE` in workflow functions such
  as `run_lca()` to surface progress information using the `progress` package.
* **Caching** – functions like `read_lca_input()` can cache intermediate
  results to avoid redundant parsing of large CSV files.
* **Selective Logging** – set the logging level via `init_logging()` to reduce
  console overhead during large batch operations.
* **Resource Monitoring** – wrap expensive steps with `monitor_step()` to
  record elapsed time and memory usage. These metrics assist in identifying
  bottlenecks.
* **Parallel Computing** – many routines such as LCA fitting, cross-validation
  and bootstrap confidence intervals accept `parallel = TRUE` to utilise all
  available CPU cores via the `future` framework.
* **Chunked Reading** – `read_csv_chunked()` streams large CSV files so they
  can be processed in manageable chunks without exhausting memory.
* **Sparse Structures** – `to_sparse_matrix()` converts dense data frames into
  `Matrix` objects which are more efficient for sparse data.
* **Cache Invalidation** – `cache_result()` caches intermediate results on disk
  and automatically invalidates them when underlying source files change. Cache
  sizes can be limited with `max_size_mb` to avoid runaway storage growth.

For computationally intensive models, consider reducing the number of bootstrap
replicates or cross-validation folds during development and increasing them
only for final analyses.

