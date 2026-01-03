# Setup file: runs before tests
# Load any required packages or set up test environment

cat("Setting up test environment...\n")

# Load the package
library(phenop)

# Create test data
create_test_data <- function() {
  set.seed(123)
  data.frame(
    id = 1:10,
    value = rnorm(10),
    group = rep(c("A", "B"), each = 5)
  )
}

# Make test data available
test_data <- create_test_data()

cat("Test environment ready\n")
