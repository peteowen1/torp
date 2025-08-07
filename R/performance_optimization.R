# Performance Optimization and Memory Management
# ==============================================
# Enterprise-grade performance optimization for AFL analytics at scale

library(parallel)
library(future)
library(data.table)

#' Performance Monitor Class
#' 
#' Monitors and optimizes system performance and memory usage
#' 
#' @export
PerformanceMonitor <- R6::R6Class(
  "PerformanceMonitor",
  
  private = list(
    .performance_history = list(),
    .memory_thresholds = list(
      warning_mb = 1024,    # 1GB
      critical_mb = 2048,   # 2GB
      max_mb = 4096        # 4GB
    ),
    .cache_config = list(
      enabled = TRUE,
      max_size_mb = 512,
      ttl_minutes = 30
    ),
    .memory_cache = list(),
    .cache_metadata = list(),
    
    # Memory management utilities
    get_memory_usage = function() {
      if (exists("memory.limit")) {
        # Windows
        list(
          used_mb = as.numeric(memory.size()),
          limit_mb = as.numeric(memory.limit()),
          available_mb = as.numeric(memory.limit()) - as.numeric(memory.size())
        )
      } else {
        # Unix-like systems
        gc_info <- gc()
        list(
          used_mb = sum(gc_info[, "used"]) * 0.000001,  # Convert bytes to MB
          limit_mb = NA,
          available_mb = NA
        )
      }
    },
    
    # Garbage collection with logging
    smart_gc = function(verbose = FALSE) {
      before_memory <- private$get_memory_usage()
      
      # Force garbage collection
      gc_result <- gc(verbose = verbose)
      
      after_memory <- private$get_memory_usage()
      
      memory_freed <- before_memory$used_mb - after_memory$used_mb
      
      if (memory_freed > 10) {  # Log if significant memory freed
        logger::log_info("Garbage collection completed",
                         memory_freed_mb = round(memory_freed, 2),
                         memory_after_mb = round(after_memory$used_mb, 2))
      }
      
      return(list(
        memory_freed_mb = memory_freed,
        current_usage_mb = after_memory$used_mb
      ))
    },
    
    # Cache management
    cache_key = function(func_name, args) {
      paste(func_name, digest::digest(args, algo = "md5"), sep = "_")
    },
    
    cache_get = function(key) {
      if (!private$.cache_config$enabled) return(NULL)
      
      cache_entry <- private$.memory_cache[[key]]
      if (is.null(cache_entry)) return(NULL)
      
      # Check TTL
      if (difftime(Sys.time(), cache_entry$cached_at, units = "mins") > private$.cache_config$ttl_minutes) {
        private$cache_remove(key)
        return(NULL)
      }
      
      cache_entry$data
    },
    
    cache_set = function(key, data) {
      if (!private$.cache_config$enabled) return(FALSE)
      
      # Check cache size limits
      data_size_mb <- as.numeric(object.size(data)) / 1024^2
      current_cache_size <- sum(sapply(private$.cache_metadata, function(x) x$size_mb))
      
      if (current_cache_size + data_size_mb > private$.cache_config$max_size_mb) {
        private$cache_evict_lru()  # Remove least recently used
      }
      
      # Store in cache
      private$.memory_cache[[key]] <- list(
        data = data,
        cached_at = Sys.time(),
        access_count = 1
      )
      
      private$.cache_metadata[[key]] <- list(
        size_mb = data_size_mb,
        created_at = Sys.time(),
        last_accessed = Sys.time()
      )
      
      return(TRUE)
    },
    
    cache_remove = function(key) {
      private$.memory_cache[[key]] <- NULL
      private$.cache_metadata[[key]] <- NULL
    },
    
    cache_evict_lru = function() {
      if (length(private$.cache_metadata) == 0) return()
      
      # Find least recently used
      access_times <- sapply(private$.cache_metadata, function(x) x$last_accessed)
      lru_key <- names(access_times)[which.min(access_times)]
      
      logger::log_debug("Evicting cache entry", key = lru_key)
      private$cache_remove(lru_key)
    }
  ),
  
  public = list(
    
    #' Initialize Performance Monitor
    initialize = function() {
      # Use conditional logging to avoid version issues
      if (requireNamespace("logger", quietly = TRUE)) {
        tryCatch({
          logger::log_info("Performance monitor initialized")
        }, error = function(e) {
          message("Performance monitor initialized")
        })
      } else {
        message("Performance monitor initialized")
      }
    },
    
    #' Monitor Function Performance
    #' 
    #' @param func Function to monitor
    #' @param func_name Name of the function for logging
    #' @param ... Arguments to pass to the function
    #' @return Function result with performance metrics
    monitor_performance = function(func, func_name, ...) {
      start_time <- Sys.time()
      start_memory <- private$get_memory_usage()
      
      # Execute function
      tryCatch({
        result <- func(...)
        
        # Calculate performance metrics
        end_time <- Sys.time()
        end_memory <- private$get_memory_usage()
        
        execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        memory_delta <- end_memory$used_mb - start_memory$used_mb
        
        # Store performance history
        perf_record <- list(
          function_name = func_name,
          execution_time_seconds = execution_time,
          memory_delta_mb = memory_delta,
          memory_peak_mb = end_memory$used_mb,
          timestamp = end_time,
          success = TRUE
        )
        
        private$.performance_history[[length(private$.performance_history) + 1]] <- perf_record
        
        # Log if significant performance impact
        if (execution_time > 5 || abs(memory_delta) > 50) {
          logger::log_info("Performance monitoring",
                           function_name = func_name,
                           execution_time = round(execution_time, 3),
                           memory_delta_mb = round(memory_delta, 2))
        }
        
        # Check memory thresholds
        self$check_memory_thresholds(end_memory$used_mb)
        
        return(result)
        
      }, error = function(e) {
        # Record failed execution
        perf_record <- list(
          function_name = func_name,
          execution_time_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
          memory_delta_mb = private$get_memory_usage()$used_mb - start_memory$used_mb,
          timestamp = Sys.time(),
          success = FALSE,
          error_message = e$message
        )
        
        private$.performance_history[[length(private$.performance_history) + 1]] <- perf_record
        
        logger::log_error("Function execution failed",
                         function_name = func_name,
                         error = e$message)
        
        stop(e)
      })
    },
    
    #' Check Memory Thresholds
    #' 
    #' @param current_usage_mb Current memory usage in MB
    check_memory_thresholds = function(current_usage_mb = NULL) {
      if (is.null(current_usage_mb)) {
        current_usage_mb <- private$get_memory_usage()$used_mb
      }
      
      if (current_usage_mb > private$.memory_thresholds$critical_mb) {
        logger::log_error("CRITICAL: Memory usage exceeded critical threshold",
                         current_mb = round(current_usage_mb, 2),
                         threshold_mb = private$.memory_thresholds$critical_mb)
        
        # Force aggressive cleanup
        self$aggressive_cleanup()
        
      } else if (current_usage_mb > private$.memory_thresholds$warning_mb) {
        logger::log_warn("WARNING: Memory usage exceeded warning threshold",
                        current_mb = round(current_usage_mb, 2),
                        threshold_mb = private$.memory_thresholds$warning_mb)
        
        # Trigger preventive cleanup
        self$preventive_cleanup()
      }
      
      # Hard limit check
      if (current_usage_mb > private$.memory_thresholds$max_mb) {
        stop(paste("Memory usage exceeded maximum limit:", 
                  round(current_usage_mb, 2), "MB >", private$.memory_thresholds$max_mb, "MB"))
      }
    },
    
    #' Preventive Cleanup
    preventive_cleanup = function() {
      logger::log_info("Executing preventive memory cleanup")
      
      # Clear cache of older entries
      if (length(private$.cache_metadata) > 0) {
        old_entries <- names(private$.cache_metadata)[
          sapply(private$.cache_metadata, function(x) {
            difftime(Sys.time(), x$created_at, units = "mins") > 15
          })
        ]
        
        for (key in old_entries) {
          private$cache_remove(key)
        }
      }
      
      # Garbage collection
      private$smart_gc()
    },
    
    #' Aggressive Cleanup
    aggressive_cleanup = function() {
      logger::log_warn("Executing aggressive memory cleanup")
      
      # Clear all cache
      private$.memory_cache <- list()
      private$.cache_metadata <- list()
      
      # Multiple garbage collections
      for (i in 1:3) {
        private$smart_gc()
        Sys.sleep(0.1)
      }
      
      # Remove old performance history
      if (length(private$.performance_history) > 1000) {
        private$.performance_history <- tail(private$.performance_history, 500)
      }
    },
    
    #' Get Performance Report
    #' 
    #' @param last_n_records Number of recent records to analyze
    #' @return Performance analysis report
    get_performance_report = function(last_n_records = 100) {
      if (length(private$.performance_history) == 0) {
        return(list(message = "No performance data available"))
      }
      
      # Get recent records
      recent_records <- tail(private$.performance_history, last_n_records)
      
      # Calculate statistics
      execution_times <- sapply(recent_records, function(x) x$execution_time_seconds)
      memory_deltas <- sapply(recent_records, function(x) x$memory_delta_mb)
      success_rate <- mean(sapply(recent_records, function(x) x$success))
      
      # Function-level analysis
      func_stats <- aggregate(
        execution_times, 
        by = list(function_name = sapply(recent_records, function(x) x$function_name)),
        FUN = function(x) c(mean = mean(x), median = median(x), max = max(x), count = length(x))
      )
      
      current_memory <- private$get_memory_usage()
      
      report <- list(
        summary = list(
          total_calls = length(recent_records),
          success_rate = success_rate,
          avg_execution_time = mean(execution_times),
          median_execution_time = median(execution_times),
          max_execution_time = max(execution_times),
          avg_memory_delta = mean(memory_deltas),
          total_memory_allocated = sum(pmax(0, memory_deltas))
        ),
        current_status = list(
          memory_usage_mb = current_memory$used_mb,
          cache_entries = length(private$.memory_cache),
          cache_size_mb = sum(sapply(private$.cache_metadata, function(x) x$size_mb)),
          performance_records = length(private$.performance_history)
        ),
        function_statistics = func_stats,
        recommendations = self$get_performance_recommendations(recent_records)
      )
      
      return(report)
    },
    
    #' Get Performance Recommendations
    #' 
    #' @param recent_records Recent performance records
    #' @return List of performance recommendations
    get_performance_recommendations = function(recent_records) {
      recommendations <- list()
      
      # Memory recommendations
      memory_deltas <- sapply(recent_records, function(x) x$memory_delta_mb)
      if (mean(memory_deltas) > 10) {
        recommendations$memory <- "Consider implementing more aggressive caching or data chunking for memory-intensive operations"
      }
      
      # Execution time recommendations
      execution_times <- sapply(recent_records, function(x) x$execution_time_seconds)
      slow_functions <- names(table(sapply(recent_records[execution_times > 2], function(x) x$function_name)))
      
      if (length(slow_functions) > 0) {
        recommendations$performance <- paste("Consider optimizing these slow functions:", paste(slow_functions, collapse = ", "))
      }
      
      # Cache recommendations
      if (length(private$.memory_cache) == 0) {
        recommendations$caching <- "Consider enabling caching for frequently called functions"
      }
      
      return(recommendations)
    },
    
    #' Enable Caching
    #' 
    #' @param max_size_mb Maximum cache size in MB
    #' @param ttl_minutes Time to live in minutes
    enable_caching = function(max_size_mb = 512, ttl_minutes = 30) {
      private$.cache_config$enabled <- TRUE
      private$.cache_config$max_size_mb <- max_size_mb
      private$.cache_config$ttl_minutes <- ttl_minutes
      
      logger::log_info("Caching enabled",
                       max_size_mb = max_size_mb,
                       ttl_minutes = ttl_minutes)
    },
    
    #' Disable Caching
    disable_caching = function() {
      private$.cache_config$enabled <- FALSE
      private$.memory_cache <- list()
      private$.cache_metadata <- list()
      
      logger::log_info("Caching disabled")
    },
    
    #' Clear Cache
    clear_cache = function() {
      private$.memory_cache <- list()
      private$.cache_metadata <- list()
      
      logger::log_info("Cache cleared")
    }
  )
)

#' Global Performance Monitor Instance
#' 
#' @export
torp_performance <- PerformanceMonitor$new()

#' Optimized Data Processing Functions
#' 
#' High-performance versions of common data operations
#' 

#' Process Large Play-by-Play Data Efficiently
#' 
#' @param pbp_data Play-by-play data (data.frame or data.table)
#' @param chunk_size Number of rows to process per chunk
#' @param parallel Logical, whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @return Processed data
#' @export
process_pbp_data_optimized <- function(pbp_data, chunk_size = 10000, parallel = TRUE, n_cores = NULL) {
  
  # Convert to data.table for performance
  if (!data.table::is.data.table(pbp_data)) {
    pbp_data <- data.table::as.data.table(pbp_data)
  }
  
  # Determine optimal chunk size based on data size
  n_rows <- nrow(pbp_data)
  if (n_rows < chunk_size) {
    chunk_size <- n_rows
  }
  
  # Setup parallel processing if requested
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- min(4, parallel::detectCores() - 1)  # Conservative default
    }
    
    # Check if future is available and set plan
    if (requireNamespace("future", quietly = TRUE)) {
      original_plan <- future::plan()
      future::plan(future::multisession, workers = n_cores)
      on.exit(future::plan(original_plan), add = TRUE)
    }
  }
  
  # Process in chunks
  chunk_indices <- seq(1, n_rows, by = chunk_size)
  n_chunks <- length(chunk_indices)
  
  logger::log_info("Processing PBP data",
                   n_rows = n_rows,
                   n_chunks = n_chunks,
                   chunk_size = chunk_size,
                   parallel = parallel)
  
  # Function to process a single chunk
  process_chunk <- function(start_idx) {
    end_idx <- min(start_idx + chunk_size - 1, n_rows)
    chunk_data <- pbp_data[start_idx:end_idx]
    
    # Apply efficient processing using data.table operations
    tryCatch({
      # Example optimized operations
      chunk_data[, `:=`(
        time_remaining = pmax(0, (4 - period) * 1800 + (1800 - period_seconds)),
        goal_distance = sqrt((goal_x - 0)^2 + y^2),
        points_diff_abs = abs(points_diff)
      )]
      
      # Group operations (more efficient than dplyr for large data)
      chunk_data[, `:=`(
        team_momentum = zoo::rollmean(points_diff, k = 3, fill = NA, align = "right")
      ), by = .(match_id, team_id_mdl)]
      
      return(chunk_data)
      
    }, error = function(e) {
      logger::log_error("Chunk processing failed", 
                       start_idx = start_idx, 
                       end_idx = end_idx,
                       error = e$message)
      return(NULL)
    })
  }
  
  # Process chunks (parallel or sequential)
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    processed_chunks <- future.apply::future_lapply(chunk_indices, process_chunk)
  } else {
    processed_chunks <- lapply(chunk_indices, process_chunk)
  }
  
  # Remove any failed chunks
  processed_chunks <- Filter(Negate(is.null), processed_chunks)
  
  # Combine results efficiently
  result <- data.table::rbindlist(processed_chunks)
  
  logger::log_info("PBP data processing completed", output_rows = nrow(result))
  
  return(result)
}

#' Cached Model Prediction Function
#' 
#' @param model_func Function that performs prediction
#' @param input_data Input data for prediction
#' @param cache_key_suffix Additional suffix for cache key
#' @return Prediction results (cached if available)
#' @export
cached_prediction <- function(model_func, input_data, cache_key_suffix = "") {
  
  # Generate cache key
  func_name <- deparse(substitute(model_func))
  cache_key <- paste0(func_name, "_", cache_key_suffix, "_", 
                     digest::digest(input_data, algo = "md5"))
  
  # Try to get from cache first
  cached_result <- torp_performance$.__enclos_env__$private$cache_get(cache_key)
  
  if (!is.null(cached_result)) {
    logger::log_debug("Cache hit for prediction", cache_key = substr(cache_key, 1, 20))
    return(cached_result)
  }
  
  # Cache miss - execute function and cache result
  logger::log_debug("Cache miss for prediction", cache_key = substr(cache_key, 1, 20))
  
  result <- torp_performance$monitor_performance(model_func, func_name, input_data)
  
  # Cache the result
  torp_performance$.__enclos_env__$private$cache_set(cache_key, result)
  
  return(result)
}

#' Memory-Efficient Batch Processing
#' 
#' @param data_source Data source (file path, database connection, etc.)
#' @param processing_func Function to apply to each batch
#' @param batch_size Number of records per batch
#' @param output_file Optional output file for results
#' @return Processing summary
#' @export
batch_process_memory_efficient <- function(data_source, processing_func, batch_size = 5000, output_file = NULL) {
  
  processing_summary <- list(
    batches_processed = 0,
    total_records = 0,
    errors = 0,
    start_time = Sys.time()
  )
  
  # Initialize output connection if specified
  output_conn <- if (!is.null(output_file)) {
    file(output_file, "w")
  } else {
    NULL
  }
  
  on.exit({
    if (!is.null(output_conn)) close(output_conn)
  }, add = TRUE)
  
  # Process in batches
  batch_num <- 1
  
  repeat {
    # Check memory before processing batch
    torp_performance$check_memory_thresholds()
    
    # Read batch (this would be customized based on data source)
    batch_data <- tryCatch({
      # This is a placeholder - actual implementation would depend on data source type
      if (is.character(data_source) && file.exists(data_source)) {
        # File-based processing
        skip_rows <- (batch_num - 1) * batch_size
        readr::read_csv(data_source, skip = skip_rows, n_max = batch_size, show_col_types = FALSE)
      } else {
        NULL  # No more data
      }
    }, error = function(e) {
      logger::log_error("Error reading batch", batch = batch_num, error = e$message)
      NULL
    })
    
    # Break if no more data
    if (is.null(batch_data) || nrow(batch_data) == 0) {
      break
    }
    
    # Process batch
    tryCatch({
      processed_batch <- torp_performance$monitor_performance(
        processing_func, 
        paste0("batch_processing_", batch_num),
        batch_data
      )
      
      # Write output if specified
      if (!is.null(output_conn) && !is.null(processed_batch)) {
        if (batch_num == 1) {
          # Write header for first batch
          writeLines(paste(names(processed_batch), collapse = ","), output_conn)
        }
        
        # Write data
        write.table(processed_batch, output_conn, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      }
      
      processing_summary$batches_processed <- processing_summary$batches_processed + 1
      processing_summary$total_records <- processing_summary$total_records + nrow(batch_data)
      
    }, error = function(e) {
      logger::log_error("Error processing batch", batch = batch_num, error = e$message)
      processing_summary$errors <- processing_summary$errors + 1
    })
    
    # Memory cleanup every 10 batches
    if (batch_num %% 10 == 0) {
      torp_performance$preventive_cleanup()
    }
    
    batch_num <- batch_num + 1
  }
  
  processing_summary$end_time <- Sys.time()
  processing_summary$total_time_minutes <- as.numeric(difftime(processing_summary$end_time, processing_summary$start_time, units = "mins"))
  
  logger::log_info("Batch processing completed",
                   batches = processing_summary$batches_processed,
                   records = processing_summary$total_records,
                   errors = processing_summary$errors,
                   duration_min = round(processing_summary$total_time_minutes, 2))
  
  return(processing_summary)
}

#' Initialize Performance Optimization System
#' 
#' @param cache_enabled Logical, whether to enable caching
#' @param memory_warning_mb Memory warning threshold in MB
#' @param memory_critical_mb Memory critical threshold in MB
#' @export
initialize_performance_system <- function(cache_enabled = TRUE, 
                                         memory_warning_mb = 1024, 
                                         memory_critical_mb = 2048) {
  
  # Configure memory thresholds
  torp_performance$.__enclos_env__$private$.memory_thresholds$warning_mb <- memory_warning_mb
  torp_performance$.__enclos_env__$private$.memory_thresholds$critical_mb <- memory_critical_mb
  
  # Enable/disable caching
  if (cache_enabled) {
    torp_performance$enable_caching()
  } else {
    torp_performance$disable_caching()
  }
  
  logger::log_info("Performance optimization system initialized",
                   caching = cache_enabled,
                   memory_warning_mb = memory_warning_mb,
                   memory_critical_mb = memory_critical_mb)
}