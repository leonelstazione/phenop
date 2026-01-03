# Teardown file: runs after tests
# Clean up any test files or objects

cat("Cleaning up test environment...\n")

# Remove any temporary files
temp_files <- list.files(tempdir(), pattern = "^test_", full.names = TRUE)
if (length(temp_files) > 0) {
  file.remove(temp_files)
}

# Clear any test objects from global environment
rm(list = ls(pattern = "^test_", envir = .GlobalEnv), envir = .GlobalEnv)

cat("Test environment cleaned\n")
