# Simple Test Runner for phenop Package
# Run this script to execute tests

cat("Running tests for phenop package...\n")

# Load required packages
library(testthat)
library(devtools)

# Load the package
devtools::load_all()

# Run all tests
cat("\nRunning all tests...\n")
test_results <- test_dir("tests/testthat")

# Summary
cat("\n====================================\n")
cat("Test execution completed\n")
cat("Total tests run:", sum(sapply(test_results, function(x) x$tests)), "\n")
cat("Tests passed:", sum(sapply(test_results, function(x) x$passed)), "\n")
cat("Tests failed:", sum(sapply(test_results, function(x) x$failed)), "\n")
