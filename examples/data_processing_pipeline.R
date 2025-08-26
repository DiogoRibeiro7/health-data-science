# Demonstration of the data processing pipeline utilities
source("../R/data_processing.R")

# Create a small sample dataset with missing values and an outlier
set.seed(42)
df <- data.frame(
  mpg = c(mtcars$mpg, NA),
  hp = c(mtcars$hp, 500)
)

# Define transformation steps
steps <- list(
  impute_missing,
  function(d) handle_outliers(d, c("mpg", "hp"))
)

# Run the pipeline with a validator
validators <- list(function(d) all(!is.na(d$mpg)))
clean <- run_transform_pipeline(df, steps, validators)
print(head(clean))
