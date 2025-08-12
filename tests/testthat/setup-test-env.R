# Test environment setup
# This file runs before all tests

# Set up test-specific options
options(
  torp.test_mode = TRUE,
  warn = 1  # Show warnings immediately during testing
)

# Set up temporary directory for test artifacts
test_temp_dir <- file.path(tempdir(), "torp_tests")
if (!dir.exists(test_temp_dir)) {
  dir.create(test_temp_dir, recursive = TRUE)
}

# Mock data repository location for testing
options(torp.data.repo = "torpdata")

# Suppress specific warnings that are expected during testing
suppressWarnings({
  # Load required packages for testing
  library(testthat)
  library(torp)
})

# Clean up test environment on exit
reg.finalizer(globalenv(), function(e) {
  if (dir.exists(test_temp_dir)) {
    unlink(test_temp_dir, recursive = TRUE)
  }
}, onexit = TRUE)
