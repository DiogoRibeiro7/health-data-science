# Data Requirements

This toolkit expects tabular health data with column names following the NCDB
Public Use File (PUF) conventions. At a minimum, the latent class analysis
workflows require the following fields:

* `PUF_CASE_ID` – unique case identifier
* Demographics: `SEX`, `race3`, `hispanic3`, `urbandwell`, `age4`, `SES`
* Insurance information: `insurancetype`
* Clinical fields: `optcare`, `DX_RX_STARTED_DAYS`, `CROWFLY`,
  `CDCC_TOTAL_BEST`

Data should be provided as a UTF-8 encoded CSV file. Missing values are
expected to be encoded as blank fields. Additional columns are ignored but
preserved when merging results back into the full dataset.

For data processing utilities, any rectangular data frame is accepted. Numeric
columns are required for outlier detection and polynomial feature generation.

Ensure all files reside in accessible directories; functions such as
`read_csv_safely()` and `require_data_file()` will emit informative errors if
paths are invalid.

