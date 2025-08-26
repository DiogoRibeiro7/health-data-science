# Example: database ETL pipeline using SQLite
library(healthdatascience)

src <- db_connect(list(dbms = "sqlite", dbname = tempfile()))
dest <- db_connect(list(dbms = "sqlite", dbname = tempfile()))
DBI::dbWriteTable(src, "patients", data.frame(id = 1:2, val = letters[1:2]))
run_etl(src, dest, "select * from patients", "patients_copy")
print(DBI::dbReadTable(dest, "patients_copy"))

db_disconnect(src)
db_disconnect(dest)

