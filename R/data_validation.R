# Data Validation Pipeline
# ========================
# Comprehensive data validation and quality assurance for TORP models


#' AFL Data Schema Definitions
#'
#' Defines expected schemas for different data types in the AFL analytics pipeline
#'
#' @return List of data schema definitions
#' @keywords internal
get_afl_data_schemas <- function() {
  list(
    chains_data = list(
      required_cols = c("match_id", "period", "period_seconds", "team_id", "x", "y", 
                       "description", "player_name", "utc_start_time"),
      col_types = list(
        match_id = "character",
        period = "integer", 
        period_seconds = "numeric",
        team_id = "integer",
        x = "numeric",
        y = "numeric",
        description = "character",
        player_name = "character"
      ),
      constraints = list(
        period = c(1, 4),
        period_seconds = c(0, 2000),
        x = c(-120, 120),
        y = c(-80, 80)
      )
    ),
    
    model_data_epv = list(
      required_cols = c("match_id", "period", "period_seconds", "goal_x", "y", 
                       "exp_pts", "team_id_mdl", "home"),
      col_types = list(
        goal_x = "numeric",
        exp_pts = "numeric",
        home = "integer"
      ),
      constraints = list(
        goal_x = c(0, 200),
        exp_pts = c(-10, 10),
        home = c(0, 1)
      )
    ),
    
    model_data_wp = list(
      required_cols = c("match_id", "period", "period_seconds", "points_diff", 
                       "xpoints_diff", "label_wp"),
      col_types = list(
        points_diff = "numeric",
        xpoints_diff = "numeric", 
        label_wp = "numeric"
      ),
      constraints = list(
        points_diff = c(-200, 200),
        label_wp = c(0, 1)
      )
    ),
    
    player_data = list(
      required_cols = c("player_id", "player_name", "team", "position", "season"),
      col_types = list(
        player_id = "integer",
        player_name = "character",
        team = "character",
        position = "character",
        season = "integer"
      ),
      constraints = list(
        season = c(2010, 2030)
      )
    )
  )
}

#' Validate Data Schema
#'
#' Validates that a dataframe conforms to the expected schema
#'
#' @param data Dataframe to validate
#' @param schema_name Name of the schema to validate against
#' @param strict Logical, whether to fail on schema violations (default: TRUE)
#' @return List containing validation results
#' @export
#' @importFrom dplyr mutate_all group_by_all summarise arrange desc group_by filter
validate_data_schema <- function(data, schema_name, strict = TRUE) {
  
  schemas <- get_afl_data_schemas()
  
  if (!schema_name %in% names(schemas)) {
    stop(paste("Unknown schema:", schema_name, ". Available schemas:", 
               paste(names(schemas), collapse = ", ")))
  }
  
  schema <- schemas[[schema_name]]
  issues <- character(0)
  
  # Check required columns
  missing_cols <- setdiff(schema$required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check column types
  for (col_name in names(schema$col_types)) {
    if (col_name %in% names(data)) {
      expected_type <- schema$col_types[[col_name]]
      actual_type <- class(data[[col_name]])[1]
      
      # Type compatibility check
      compatible <- switch(expected_type,
        "character" = is.character(data[[col_name]]),
        "integer" = is.integer(data[[col_name]]) || is.numeric(data[[col_name]]),
        "numeric" = is.numeric(data[[col_name]]),
        "logical" = is.logical(data[[col_name]]),
        FALSE
      )
      
      if (!compatible) {
        issues <- c(issues, paste0("Column '", col_name, "' expected ", expected_type, 
                                  " but got ", actual_type))
      }
    }
  }
  
  # Check value constraints
  for (col_name in names(schema$constraints)) {
    if (col_name %in% names(data)) {
      constraint <- schema$constraints[[col_name]]
      col_data <- data[[col_name]]
      
      if (length(constraint) == 2) {  # Range constraint
        min_val <- constraint[1]
        max_val <- constraint[2]
        
        out_of_range <- sum(col_data < min_val | col_data > max_val, na.rm = TRUE)
        if (out_of_range > 0) {
          issues <- c(issues, paste0("Column '", col_name, "' has ", out_of_range, 
                                    " values outside range [", min_val, ", ", max_val, "]"))
        }
      }
    }
  }
  
  # Log validation results
  if (length(issues) > 0) {
    severity <- if (strict) "HIGH" else "MEDIUM"
    log_data_quality(schema_name, issues, severity)
    
    if (strict) {
      stop(paste("Data validation failed for schema", schema_name, ":\n", 
                paste(issues, collapse = "\n")))
    }
  } else {
    message(paste("Data validation passed for schema", schema_name, "with", nrow(data), "rows"))
  }
  
  return(list(
    schema_name = schema_name,
    valid = length(issues) == 0,
    issues = issues,
    n_rows = nrow(data),
    n_cols = ncol(data)
  ))
}

#' Validate Data Quality
#'
#' Performs comprehensive data quality checks beyond schema validation
#'
#' @param data Dataframe to validate
#' @param data_type Type of data ("chains", "model", "player", etc.)
#' @return List containing quality assessment results
#' @export
validate_data_quality <- function(data, data_type = "unknown") {
  
  quality_issues <- list()
  
  # Basic data structure checks
  if (nrow(data) == 0) {
    quality_issues$empty_data <- "Dataset is empty"
  }
  
  if (ncol(data) == 0) {
    quality_issues$no_columns <- "Dataset has no columns"
  }
  
  # Missing data analysis
  missing_analysis <- analyze_missing_data(data)
  if (missing_analysis$high_missing_cols > 0) {
    quality_issues$high_missing <- paste("Columns with >50% missing:", 
                                        paste(missing_analysis$high_missing_col_names, collapse = ", "))
  }
  
  # Duplicate analysis
  duplicate_analysis <- analyze_duplicates(data)
  if (duplicate_analysis$duplicate_rate > 0.1) {
    quality_issues$high_duplicates <- paste("High duplicate rate:", 
                                           round(duplicate_analysis$duplicate_rate * 100, 2), "%")
  }
  
  # Data type specific validations
  if (data_type == "chains") {
    quality_issues <- c(quality_issues, validate_chains_quality(data))
  } else if (data_type == "model") {
    quality_issues <- c(quality_issues, validate_model_data_quality(data))
  } else {
    # Generic quality checks for other data types
    quality_issues <- c(quality_issues, validate_generic_data_quality(data))
  }
  
  # Outlier detection for numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    outlier_analysis <- detect_outliers(data[, numeric_cols, drop = FALSE])
    if (outlier_analysis$extreme_outliers > 0) {
      quality_issues$outliers <- paste("Extreme outliers detected in", 
                                      outlier_analysis$extreme_outliers, "columns")
    }
  }
  
  # Overall quality score
  quality_score <- calculate_quality_score(data, quality_issues)
  
  # Log quality assessment
  if (length(quality_issues) > 0) {
    severity <- if (quality_score < 0.7) "HIGH" else "MEDIUM"
    log_data_quality(data_type, unlist(quality_issues), severity)
  }
  
  return(list(
    data_type = data_type,
    quality_score = quality_score,
    issues = quality_issues,
    missing_analysis = missing_analysis,
    duplicate_analysis = duplicate_analysis,
    n_rows = nrow(data),
    n_cols = ncol(data)
  ))
}

#' Analyze Missing Data Patterns
#'
#' Analyzes missing data patterns in a dataframe
#'
#' @param data Dataframe to analyze
#' @return List with missing data analysis results
#' @keywords internal
#' @importFrom dplyr group_by_all summarise arrange desc n
analyze_missing_data <- function(data) {

  # Calculate missing percentages by column (vectorized, no full copy)
  missing_pct <- vapply(data, function(x) sum(is.na(x)) / length(x), FUN.VALUE = numeric(1))

  # Identify columns with high missing rates
  high_missing_threshold <- 0.5
  high_missing_cols <- missing_pct > high_missing_threshold

  # Pattern analysis for combinations of missing values
  if (ncol(data) <= 20) {
    # Use vapply to create missing indicator matrix (more efficient than mutate_all)
    missing_matrix <- as.data.frame(
      lapply(data, is.na)
    )
    missing_patterns <- missing_matrix %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(count))
  } else {
    missing_patterns <- NULL
  }

  list(
    overall_missing_rate = sum(is.na(data)) / (nrow(data) * ncol(data)),
    missing_by_column = missing_pct,
    high_missing_cols = sum(high_missing_cols),
    high_missing_col_names = names(missing_pct)[high_missing_cols],
    missing_patterns = missing_patterns
  )
}

#' Analyze Duplicate Records
#'
#' Analyzes duplicate records in a dataframe
#'
#' @param data Dataframe to analyze
#' @return List with duplicate analysis results
#' @keywords internal
analyze_duplicates <- function(data) {
  
  # Count exact duplicates
  n_duplicates <- sum(duplicated(data))
  duplicate_rate <- n_duplicates / nrow(data)
  
  # Identify potential key columns for more sophisticated duplicate detection
  potential_keys <- c("match_id", "player_id", "team_id", "period", "period_seconds")
  key_cols <- intersect(potential_keys, names(data))
  
  key_duplicates <- 0
  if (length(key_cols) > 0) {
    key_duplicates <- sum(duplicated(data[, key_cols, drop = FALSE]))
  }
  
  list(
    n_total_rows = nrow(data),
    n_exact_duplicates = n_duplicates,
    duplicate_rate = duplicate_rate,
    n_key_duplicates = key_duplicates,
    key_columns_used = key_cols
  )
}

#' Detect Statistical Outliers
#'
#' Detects outliers in numeric data using multiple methods
#'
#' @param data Dataframe with numeric columns
#' @return List with outlier detection results
#' @keywords internal
detect_outliers <- function(data) {
  
  outlier_results <- list()
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    col_data <- col_data[!is.na(col_data)]
    
    if (length(col_data) < 10) next  # Skip columns with too few observations
    
    # IQR method
    q1 <- quantile(col_data, 0.25)
    q3 <- quantile(col_data, 0.75)
    iqr <- q3 - q1
    
    mild_outliers <- sum(col_data < (q1 - 1.5 * iqr) | col_data > (q3 + 1.5 * iqr))
    extreme_outliers <- sum(col_data < (q1 - 3 * iqr) | col_data > (q3 + 3 * iqr))
    
    # Z-score method (for comparison)
    z_scores <- abs(scale(col_data))
    z_outliers <- sum(z_scores > 3, na.rm = TRUE)
    
    outlier_results[[col_name]] <- list(
      mild_outliers_iqr = as.numeric(mild_outliers),
      extreme_outliers_iqr = as.numeric(extreme_outliers),
      z_score_outliers = as.numeric(z_outliers),
      outlier_rate = as.numeric(mild_outliers / length(col_data))
    )
  }
  
  # Summary statistics
  total_extreme <- sum(vapply(outlier_results, function(x) as.numeric(x$extreme_outliers_iqr), FUN.VALUE = numeric(1)))
  
  list(
    column_results = outlier_results,
    extreme_outliers = total_extreme,
    n_columns_analyzed = length(outlier_results)
  )
}

#' Validate Chains Data Quality
#'
#' Specific quality validation for AFL chains data
#'
#' @param data Chains dataframe
#' @return List of chains-specific quality issues
#' @keywords internal
validate_chains_quality <- function(data) {
  
  issues <- list()
  
  # Check match_id format
  if ("match_id" %in% names(data)) {
    invalid_match_ids <- sum(!grepl("^CD_M\\d{8}\\d{2}$", data$match_id))
    if (invalid_match_ids > 0) {
      issues$invalid_match_ids <- paste("Invalid match_id format:", invalid_match_ids, "rows")
    }
  }
  
  # Check period consistency
  if ("period" %in% names(data)) {
    invalid_periods <- sum(data$period < 1 | data$period > 4, na.rm = TRUE)
    if (invalid_periods > 0) {
      issues$invalid_periods <- paste("Invalid period values:", invalid_periods, "rows")
    }
  }
  
  # Check temporal consistency within matches
  if (all(c("match_id", "period", "period_seconds") %in% names(data))) {
    temporal_issues <- data %>%
      group_by(match_id, period) %>%
      summarise(
        min_seconds = min(period_seconds, na.rm = TRUE),
        max_seconds = max(period_seconds, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(min_seconds < 0 | max_seconds > 2000)
    
    if (nrow(temporal_issues) > 0) {
      issues$temporal_inconsistency <- paste("Temporal inconsistencies in", 
                                           nrow(temporal_issues), "match-periods")
    }
  }
  
  # Check coordinate bounds
  if ("x" %in% names(data) && "y" %in% names(data)) {
    out_of_bounds <- sum(abs(data$x) > 120 | abs(data$y) > 80, na.rm = TRUE)
    if (out_of_bounds > 0) {
      issues$coordinates_out_of_bounds <- paste("Coordinates out of AFL field bounds:", 
                                              out_of_bounds, "rows")
    }
  }
  
  return(issues)
}

#' Validate Model Data Quality
#'
#' Specific quality validation for model training/prediction data
#'
#' @param data Model dataframe
#' @return List of model data quality issues
#' @keywords internal
validate_model_data_quality <- function(data) {
  
  issues <- list()
  
  # Check for extreme values in key variables
  if ("points_diff" %in% names(data)) {
    extreme_scores <- sum(abs(data$points_diff) > 150, na.rm = TRUE)
    if (extreme_scores > 0) {
      issues$extreme_score_differences <- paste("Extreme score differences:", 
                                               extreme_scores, "rows")
    }
  }
  
  # Check label validity
  if ("label_wp" %in% names(data)) {
    invalid_labels <- sum(data$label_wp < 0 | data$label_wp > 1, na.rm = TRUE)
    if (invalid_labels > 0) {
      issues$invalid_wp_labels <- paste("Invalid win probability labels:", 
                                       invalid_labels, "rows")
    }
  }
  
  # Check for constant columns (zero variance)
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    constant_cols <- sapply(data[, numeric_cols, drop = FALSE], function(x) {
      var(x, na.rm = TRUE) == 0
    })
    
    if (any(constant_cols)) {
      issues$constant_columns <- paste("Zero-variance columns:", 
                                      paste(names(constant_cols)[constant_cols], collapse = ", "))
    }
  }
  
  return(issues)
}

#' Validate Generic Data Quality
#'
#' Generic quality validation for any data type
#'
#' @param data Generic dataframe
#' @return List of generic data quality issues
#' @keywords internal
validate_generic_data_quality <- function(data) {
  
  issues <- list()
  
  # Check for constant columns (zero variance)
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    constant_cols <- sapply(data[, numeric_cols, drop = FALSE], function(x) {
      var(x, na.rm = TRUE) == 0 | all(is.na(x))
    })
    
    if (any(constant_cols)) {
      issues$constant_columns <- paste("Zero-variance columns:", 
                                      paste(names(constant_cols)[constant_cols], collapse = ", "))
    }
  }
  
  return(issues)
}

#' Calculate Overall Data Quality Score
#'
#' Calculates a composite quality score based on various quality metrics
#'
#' @param data Dataframe being assessed
#' @param issues List of quality issues found
#' @return Numeric quality score between 0 and 1
#' @keywords internal
calculate_quality_score <- function(data, issues) {
  
  # Start with perfect score
  score <- 1.0
  
  # Penalize based on types and severity of issues
  for (issue_type in names(issues)) {
    penalty <- switch(issue_type,
      "empty_data" = 1.0,           # Complete failure
      "no_columns" = 1.0,           # Complete failure
      "high_missing" = 0.3,         # Major issue
      "high_duplicates" = 0.2,      # Moderate issue
      "invalid_match_ids" = 0.2,    # Moderate issue
      "temporal_inconsistency" = 0.3, # Major issue
      "extreme_score_differences" = 0.1,  # Minor issue
      "outliers" = 0.1,             # Minor issue
      0.1                           # Default minor penalty
    )
    
    score <- score - penalty
  }
  
  # Ensure score is between 0 and 1
  return(max(0, min(1, score)))
}

#' Validate Data Freshness
#'
#' Checks if data is recent enough for reliable predictions
#'
#' @param data_timestamp Timestamp of the most recent data (can be dataframe with timestamp column or direct timestamp)
#' @param timestamp_col Name of timestamp column if data_timestamp is a dataframe
#' @param max_age_days Maximum acceptable age in days (default: 7, overrides hours if provided)
#' @param max_age_hours Maximum acceptable age in hours (default: 24, used if max_age_days not provided)
#' @return Logical indicating if data is fresh enough
#' @export
validate_data_freshness <- function(data_timestamp, timestamp_col = "utc_start_time", max_age_days = NULL, max_age_hours = 24) {
  
  # Determine the time threshold - days takes precedence over hours
  if (!is.null(max_age_days)) {
    max_age_hours <- max_age_days * 24
  }
  
  # Handle different input types
  if (is.data.frame(data_timestamp)) {
    # If data_timestamp is a dataframe, extract timestamp column
    if (!timestamp_col %in% names(data_timestamp)) {
      warning(paste("Timestamp column", timestamp_col, "not found"))
      return(FALSE)
    }
    
    timestamps <- as.POSIXct(data_timestamp[[timestamp_col]])
    latest_data <- max(timestamps, na.rm = TRUE)
  } else {
    # If data_timestamp is already a timestamp
    latest_data <- as.POSIXct(data_timestamp)
  }
  
  age_hours <- as.numeric(difftime(Sys.time(), latest_data, units = "hours"))
  is_fresh <- age_hours <= max_age_hours
  
  if (!is_fresh) {
    log_data_quality("data_freshness", 
                     paste("Data is", round(age_hours, 1), "hours old, exceeds maximum of", max_age_hours),
                     "HIGH")
  }
  
  return(is_fresh)
}