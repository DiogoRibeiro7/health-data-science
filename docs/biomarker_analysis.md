# Biomarker Analysis

`fit_biomarker_analysis()` provides an explicit pairwise differential-expression workflow built on `limma`.

The expression input is a numeric feature-by-sample matrix. Feature identifiers must be unique row names, and the grouping vector must provide one observed label per sample. When exactly two groups are present, the first factor level is used as the reference and the second as the comparison unless the user specifies them explicitly. With more than two groups, the comparison must be stated explicitly so the reported log2 fold change always has a clear direction.

The fitted object records the reference group, comparison group, sample counts, excluded samples, multiple-testing adjustment method, adjusted-p threshold, and optional absolute log2-fold-change threshold. `tidy_biomarker_analysis()` separates ranking from significance filtering; `biomarker_analysis_summary()` reports the number of tested, significant, upregulated, and downregulated features.

Adjusted p-values address multiplicity across the tested features but do not solve batch effects, hidden confounding, poor normalization, low replication, or inappropriate experimental design. Fold-change thresholds should be justified scientifically rather than tuned after looking at the result.

The legacy `biomarker_discovery()` helper remains available and keeps the historical `limma::topTable()` return shape. New work should prefer the explicit API because it records the comparison and significance rules rather than relying on coefficient 2 of an implicit design.
