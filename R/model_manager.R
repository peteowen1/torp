# Model Manager - Decoupled Model Loading and Management
# ======================================================
# Centralized model management system to decouple model loading from prediction functions

library(digest)
library(R6)

#' Model Registry Class
#'
#' Centralized registry for managing model artifacts and metadata
#'
#' @export
ModelRegistry <- R6::R6Class(
  "ModelRegistry",
  
  private = list(
    .models = list(),
    .model_metadata = list(),
    .cache_dir = NULL,
    
    # Initialize cache directory
    initialize_cache = function() {
      if (is.null(private$.cache_dir)) {
        private$.cache_dir <- file.path(tempdir(), "torp_model_cache")
        if (!dir.exists(private$.cache_dir)) {
          dir.create(private$.cache_dir, recursive = TRUE)
        }
      }
    },
    
    # Validate model object
    validate_model = function(model, model_type) {
      if (is.null(model)) {
        return(FALSE)
      }
      
      # Type-specific validation
      valid <- switch(model_type,
        "xgboost" = inherits(model, "xgb.Booster"),
        "lightgbm" = inherits(model, "lgb.Booster"),
        "gam" = inherits(model, "gam"),
        "glm" = inherits(model, "glm"),
        "ensemble" = is.list(model) && "ensemble_weights" %in% names(model),
        TRUE  # Default: accept any non-null object
      )
      
      return(valid)
    }
  ),
  
  public = list(
    
    #' Initialize Model Registry
    #'
    #' @param cache_dir Optional cache directory path
    initialize = function(cache_dir = NULL) {
      if (!is.null(cache_dir)) {
        private$.cache_dir <- cache_dir
      }
      private$initialize_cache()
      logger::log_info("Model registry initialized", cache_dir = private$.cache_dir)
    },
    
    #' Register a Model
    #'
    #' @param model_name Unique identifier for the model
    #' @param model_object The actual model object
    #' @param model_type Type of model ("xgboost", "gam", "ensemble", etc.)
    #' @param version Model version string
    #' @param metadata Additional metadata list
    #' @return Logical indicating success
    register_model = function(model_name, model_object, model_type, version = "1.0", metadata = list()) {
      
      # Validate inputs
      if (is.null(model_name) || model_name == "") {
        stop("Model name cannot be empty")
      }
      
      if (!private$validate_model(model_object, model_type)) {
        stop(paste("Invalid model object for type:", model_type))
      }
      
      # Create model entry
      model_entry <- list(
        model = model_object,
        type = model_type,
        version = version,
        registered_at = Sys.time(),
        hash = digest::digest(model_object, algo = "md5"),
        metadata = metadata
      )
      
      # Store in registry
      private$.models[[model_name]] <- model_entry
      
      # Log registration
      logger::log_info("Model registered",
                       model_name = model_name,
                       type = model_type,
                       version = version,
                       hash = model_entry$hash)
      
      return(TRUE)
    },
    
    #' Get a Model
    #'
    #' @param model_name Name of the model to retrieve
    #' @param validate_hash Logical, whether to validate model hash
    #' @return Model object or NULL if not found
    get_model = function(model_name, validate_hash = FALSE) {
      
      if (!model_name %in% names(private$.models)) {
        logger::log_warn("Model not found in registry", model_name = model_name)
        return(NULL)
      }
      
      model_entry <- private$.models[[model_name]]
      
      # Optional hash validation
      if (validate_hash) {
        current_hash <- digest::digest(model_entry$model, algo = "md5")
        if (current_hash != model_entry$hash) {
          logger::log_error("Model hash validation failed", 
                           model_name = model_name,
                           expected_hash = model_entry$hash,
                           current_hash = current_hash)
          return(NULL)
        }
      }
      
      logger::log_debug("Model retrieved", model_name = model_name)
      return(model_entry$model)
    },
    
    #' Get Model Metadata
    #'
    #' @param model_name Name of the model
    #' @return List with model metadata
    get_model_info = function(model_name) {
      
      if (!model_name %in% names(private$.models)) {
        return(NULL)
      }
      
      model_entry <- private$.models[[model_name]]
      
      return(list(
        name = model_name,
        type = model_entry$type,
        version = model_entry$version,
        registered_at = model_entry$registered_at,
        hash = model_entry$hash,
        metadata = model_entry$metadata
      ))
    },
    
    #' List Available Models
    #'
    #' @return Character vector of available model names
    list_models = function() {
      names(private$.models)
    },
    
    #' Remove a Model
    #'
    #' @param model_name Name of the model to remove
    #' @return Logical indicating success
    remove_model = function(model_name) {
      
      if (model_name %in% names(private$.models)) {
        private$.models[[model_name]] <- NULL
        logger::log_info("Model removed from registry", model_name = model_name)
        return(TRUE)
      } else {
        logger::log_warn("Attempted to remove non-existent model", model_name = model_name)
        return(FALSE)
      }
    },
    
    #' Save Registry to Disk
    #'
    #' @param file_path Optional file path (defaults to cache directory)
    #' @return Logical indicating success
    save_registry = function(file_path = NULL) {
      
      if (is.null(file_path)) {
        file_path <- file.path(private$.cache_dir, "model_registry.rds")
      }
      
      tryCatch({
        registry_data <- list(
          models = private$.models,
          saved_at = Sys.time(),
          version = "1.0"
        )
        
        saveRDS(registry_data, file_path)
        logger::log_info("Model registry saved", file_path = file_path)
        return(TRUE)
        
      }, error = function(e) {
        logger::log_error("Failed to save model registry", error = e$message)
        return(FALSE)
      })
    },
    
    #' Load Registry from Disk
    #'
    #' @param file_path Path to saved registry file
    #' @return Logical indicating success
    load_registry = function(file_path = NULL) {
      
      if (is.null(file_path)) {
        file_path <- file.path(private$.cache_dir, "model_registry.rds")
      }
      
      if (!file.exists(file_path)) {
        logger::log_warn("Registry file not found", file_path = file_path)
        return(FALSE)
      }
      
      tryCatch({
        registry_data <- readRDS(file_path)
        
        # Validate loaded data
        if (!is.list(registry_data) || !"models" %in% names(registry_data)) {
          logger::log_error("Invalid registry file format")
          return(FALSE)
        }
        
        private$.models <- registry_data$models
        logger::log_info("Model registry loaded", 
                        file_path = file_path,
                        n_models = length(private$.models))
        return(TRUE)
        
      }, error = function(e) {
        logger::log_error("Failed to load model registry", error = e$message)
        return(FALSE)
      })
    },
    
    #' Clear All Models
    #'
    #' @return Logical indicating success
    clear_registry = function() {
      n_models <- length(private$.models)
      private$.models <- list()
      logger::log_info("Model registry cleared", n_models_removed = n_models)
      return(TRUE)
    },
    
    #' Get Registry Status
    #'
    #' @return List with registry status information
    get_status = function() {
      list(
        n_models = length(private$.models),
        model_names = names(private$.models),
        cache_dir = private$.cache_dir,
        memory_usage = object.size(private$.models)
      )
    }
  )
)

#' Global Model Registry Instance
#'
#' Package-level singleton for model management
#'
#' @export
torp_model_registry <- ModelRegistry$new()

#' Load TORP Models Safely
#'
#' Centralized function to load all TORP models with proper error handling
#'
#' @param models_to_load Character vector of model names to load
#' @param force_reload Logical, whether to force reload even if already cached
#' @return List indicating which models were successfully loaded
#' @export
load_torp_models <- function(models_to_load = c("ep_model", "wp_model", "shot_ocat_mdl"), 
                            force_reload = FALSE) {
  
  loading_results <- list()
  
  for (model_name in models_to_load) {
    
    # Check if already loaded and not forcing reload
    if (!force_reload && !is.null(torp_model_registry$get_model(model_name))) {
      loading_results[[model_name]] <- "already_loaded"
      next
    }
    
    # Attempt to load model
    tryCatch({
      
      # Load from package data
      model_env <- new.env()
      utils::data(list = model_name, package = "torp", envir = model_env)
      
      if (exists(model_name, envir = model_env)) {
        model_object <- get(model_name, envir = model_env)
        
        # Determine model type
        model_type <- determine_model_type(model_object)
        
        # Register in central registry
        success <- torp_model_registry$register_model(
          model_name = model_name,
          model_object = model_object,
          model_type = model_type,
          version = "1.0",
          metadata = list(
            source = "package_data",
            loaded_at = Sys.time()
          )
        )
        
        loading_results[[model_name]] <- if (success) "loaded" else "registration_failed"
        
      } else {
        loading_results[[model_name]] <- "not_found_in_data"
      }
      
    }, error = function(e) {
      logger::log_error("Failed to load model", model = model_name, error = e$message)
      loading_results[[model_name]] <- paste("error:", e$message)
    })
  }
  
  # Log summary
  successful_loads <- sum(loading_results == "loaded")
  logger::log_info("Model loading completed", 
                   successful = successful_loads,
                   total_attempted = length(models_to_load))
  
  return(loading_results)
}

#' Determine Model Type from Object
#'
#' Automatically determines the type of a model object
#'
#' @param model_object The model object to analyze
#' @return Character string indicating model type
#' @keywords internal
determine_model_type <- function(model_object) {
  
  if (inherits(model_object, "xgb.Booster")) {
    return("xgboost")
  } else if (inherits(model_object, "lgb.Booster")) {
    return("lightgbm")
  } else if (inherits(model_object, "gam")) {
    return("gam")
  } else if (inherits(model_object, "glm")) {
    return("glm")
  } else if (is.list(model_object) && "ensemble_weights" %in% names(model_object)) {
    return("ensemble")
  } else if (is.list(model_object) && any(c("xgb_model", "lgb_model", "gam_model") %in% names(model_object))) {
    return("ensemble")
  } else {
    return("unknown")
  }
}

#' Get Model with Fallback
#'
#' Retrieves a model with automatic fallback to package data if not in registry
#'
#' @param model_name Name of the model to retrieve
#' @param fallback_to_data Logical, whether to fallback to package data
#' @return Model object or NULL
#' @export
get_model_safe <- function(model_name, fallback_to_data = TRUE) {
  
  # Try registry first
  model <- torp_model_registry$get_model(model_name)
  
  if (!is.null(model)) {
    return(model)
  }
  
  # Fallback to package data if requested
  if (fallback_to_data) {
    logger::log_debug("Model not in registry, attempting fallback to package data", 
                     model = model_name)
    
    tryCatch({
      model_env <- new.env()
      utils::data(list = model_name, package = "torp", envir = model_env)
      
      if (exists(model_name, envir = model_env)) {
        model <- get(model_name, envir = model_env)
        
        # Register for future use
        model_type <- determine_model_type(model)
        torp_model_registry$register_model(
          model_name = model_name,
          model_object = model,
          model_type = model_type,
          version = "1.0",
          metadata = list(source = "fallback_load")
        )
        
        return(model)
      }
      
    }, error = function(e) {
      logger::log_error("Fallback model loading failed", model = model_name, error = e$message)
    })
  }
  
  logger::log_warn("Model not available", model = model_name)
  return(NULL)
}

#' Initialize TORP Model System
#'
#' Sets up the model management system and loads essential models
#'
#' @param essential_models Character vector of essential models to load
#' @param cache_dir Optional cache directory
#' @return Logical indicating successful initialization
#' @export
initialize_torp_models <- function(essential_models = c("ep_model", "wp_model"), 
                                  cache_dir = NULL) {
  
  logger::log_info("Initializing TORP model system")
  
  # Initialize registry if needed
  if (!is.null(cache_dir)) {
    torp_model_registry$initialize(cache_dir)
  }
  
  # Try to load existing registry
  registry_loaded <- torp_model_registry$load_registry()
  
  if (registry_loaded) {
    logger::log_info("Existing model registry loaded")
  } else {
    logger::log_info("Starting with empty model registry")
  }
  
  # Load essential models
  loading_results <- load_torp_models(essential_models, force_reload = FALSE)
  
  # Check if all essential models loaded successfully
  essential_loaded <- all(loading_results[essential_models] %in% c("loaded", "already_loaded"))
  
  if (essential_loaded) {
    logger::log_info("TORP model system initialized successfully")
    return(TRUE)
  } else {
    logger::log_warn("Some essential models failed to load", 
                     failed_models = names(loading_results)[!loading_results %in% c("loaded", "already_loaded")])
    return(FALSE)
  }
}

#' Model Health Check
#'
#' Performs health checks on all registered models
#'
#' @return List with health check results
#' @export
check_model_health <- function() {
  
  model_names <- torp_model_registry$list_models()
  
  if (length(model_names) == 0) {
    return(list(
      status = "no_models",
      message = "No models registered",
      models = list()
    ))
  }
  
  health_results <- list()
  
  for (model_name in model_names) {
    
    model_info <- torp_model_registry$get_model_info(model_name)
    model_obj <- torp_model_registry$get_model(model_name, validate_hash = TRUE)
    
    if (is.null(model_obj)) {
      health_results[[model_name]] <- list(
        status = "failed",
        issue = "model_object_null_or_corrupted"
      )
    } else if (is.null(model_info)) {
      health_results[[model_name]] <- list(
        status = "failed", 
        issue = "metadata_missing"
      )
    } else {
      health_results[[model_name]] <- list(
        status = "healthy",
        type = model_info$type,
        version = model_info$version,
        age_hours = as.numeric(difftime(Sys.time(), model_info$registered_at, units = "hours"))
      )
    }
  }
  
  # Overall health assessment
  healthy_models <- sum(sapply(health_results, function(x) x$status == "healthy"))
  total_models <- length(health_results)
  
  overall_status <- if (healthy_models == total_models) {
    "healthy"
  } else if (healthy_models > 0) {
    "degraded"
  } else {
    "unhealthy"
  }
  
  return(list(
    status = overall_status,
    healthy_models = healthy_models,
    total_models = total_models,
    models = health_results
  ))
}