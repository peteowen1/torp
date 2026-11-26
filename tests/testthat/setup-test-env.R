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
options(torp.data.repo = "peteowen1/torpdata")

# Disable local data auto-detection during tests
options(torp.local_data_dir = NULL)

# Fixed EP/WP model id, so vintage entries and check_vintage_alignment() never
# read torpmodels' manifest during tests (.lookup_pbp_model_id()).
options(torp.pbp_model_id = "testmodel001")

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
