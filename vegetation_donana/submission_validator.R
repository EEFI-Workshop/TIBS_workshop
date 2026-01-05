# DonanaDT 
# Testing dummy submission files  
# Nov 17th, 2025
# ____________________________________________________ #

library(tidyverse)
library(lubridate)

# ============================================================
# 1. CREATE DUMMY TEMPORAL FORECAST
# ============================================================

# create_temporal_forecast <- function() {
#   # Set reference datetime (forecast start)
#   ref_datetime <- ymd_hms("2025-11-17 00:00:00", tz = "UTC")
#   
#   # Create 30 days of forecasts for 2 species
#   temporal_forecast <- expand_grid(
#     project_id = "doñana_forecast",
#     model_id = "example_temporal_model",
#     forecast_type = "temporal",
#     site_id = "doñana_park",  
#     species = c("species_1", "species_2"),
#     datetime = seq(ref_datetime + days(1), by = "1 day", length.out = 30),
#     parameter = c("mu", "sigma")  # Using normal distribution
#   ) %>%
#     mutate(
#       reference_datetime = ref_datetime,
#       duration = "P1D",  # Daily forecast
#       family = "normal",
#       variable = "abundance",
#       prediction = case_when(
#         parameter == "mu" ~ round(rnorm(n(), mean = 50, sd = 10), 2),
#         parameter == "sigma" ~ round(abs(rnorm(n(), mean = 5, sd = 2)), 2)
#       )
#     ) %>%
#     select(project_id, model_id, forecast_type, datetime, reference_datetime, 
#            duration, site_id, species, family, parameter, variable, prediction)
#   
#   return(temporal_forecast)
# }

# ============================================================
# 2. CREATE DUMMY SPATIAL FORECAST
# ============================================================

# create_spatial_forecast <- function() {
#   # Set reference datetime
#   ref_datetime <- ymd_hms("2025-11-17 00:00:00", tz = "UTC")
#   
#   # Create spatial grid (5x5 = 25 cells)
#   spatial_grid <- expand_grid(
#     lon = seq(-6.5, -6.1, length.out = 5),
#     lat = seq(36.9, 37.3, length.out = 5)
#   ) %>%
#     mutate(cell_id = paste0("cell_", row_number()))
#   
#   # Create forecasts for 7 days, 25 cells, 2 species
#   spatial_forecast <- expand_grid(
#     project_id = "doñana_forecast",
#     model_id = "example_spatial_model",
#     forecast_type = "spatial",
#     site_id = "doñana_park",  
#     species = c("species_1", "species_2"),
#     datetime = seq(ref_datetime + days(1), by = "1 day", length.out = 7),
#     cell_id = spatial_grid$cell_id,
#     parameter = c("mu", "sigma")
#   ) %>%
#     left_join(spatial_grid, by = "cell_id") %>%
#     mutate(
#       reference_datetime = ref_datetime,
#       duration = "P1D",
#       family = "normal",
#       variable = "abundance",
#       cell_lat = round(lat, 4),
#       cell_lon = round(lon, 4),
#       prediction = case_when(
#         parameter == "mu" ~ round(rnorm(n(), mean = 30, sd = 8), 2),
#         parameter == "sigma" ~ round(abs(rnorm(n(), mean = 4, sd = 1.5)), 2)
#       )
#     ) %>%
#     select(project_id, model_id, forecast_type, datetime, reference_datetime, 
#            duration, site_id, species, cell_id, cell_lat, cell_lon,  
#            family, parameter, variable, prediction)
#   
#   return(spatial_forecast)
# }

# ============================================================
# 3. VALIDATION FUNCTION
# ============================================================

validate_forecast <- function(df, forecast_type = c("temporal", "spatial")) {

  errors <- character()
  warnings <- character()

  # Check required columns based on forecast type
  required_base <- c("project_id", "model_id", "forecast_type", "datetime",
                     "reference_datetime", "duration", "site_id", "species",
                     "family", "uncertainty_component", "variable", "prediction")

  if (forecast_type == "spatial") {
    required_cols <- c(required_base, "cell_id", "cell_lat", "cell_lon")
  } else {
    required_cols <- required_base
  }

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check if data exists
  if (nrow(df) == 0) {
    errors <- c(errors, "File contains no data rows")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Validate forecast_type column
  if ("forecast_type" %in% names(df)) {
    if (!all(df$forecast_type %in% c("temporal", "spatial"))) {
      errors <- c(errors, "forecast_type must be either 'temporal' or 'spatial'")
    }
    if (forecast_type == "spatial" && any(df$forecast_type != "spatial")) {
      errors <- c(errors, "Spatial forecast file contains non-spatial forecast_type values")
    }
    if (forecast_type == "temporal" && any(df$forecast_type != "temporal")) {
      errors <- c(errors, "Temporal forecast file contains non-temporal forecast_type values")
    }
  }

  # Validate datetime formats
  if ("datetime" %in% names(df)) {
    datetime_test <- try(ymd_hms(df$datetime[1]), silent = TRUE)
    if (inherits(datetime_test, "try-error")) {
      errors <- c(errors, "datetime column not in correct format (%Y-%m-%d %H:%M:%S)")
    }
  }

  if ("reference_datetime" %in% names(df)) {
    ref_test <- try(ymd_hms(df$reference_datetime[1]), silent = TRUE)
    if (inherits(ref_test, "try-error")) {
      errors <- c(errors, "reference_datetime column not in correct format (%Y-%m-%d %H:%M:%S)")
    }
  }

  # Validate duration format (ISO 8601)
  if ("duration" %in% names(df)) {
    valid_durations <- c("P1D", "P1W", "P1M", "P1Y", "PT1H", "PT6H", "PT12H")
    if (!all(df$duration %in% valid_durations)) {
      errors <- c(errors, "duration must be valid ISO 8601 format (e.g., P1D, P1W, PT1H)")
    }
  }

  # Define valid families and their required representation of uncertainty
  valid_family_params <- list(
    normal = c("mu", "sigma"),
    lognormal = c("mu", "sigma"),
    bernoulli = "prob",
    beta = c("shape1", "shape2"),
    uniform = c("min", "max"),
    gamma = c("shape", "rate"),
    logistic = c("location", "scale"),
    exponential = "rate",
    poisson = "lambda",
    sample = "sample_index"  # non-parametric forecasts
  )

  # Validate family column
  if ("family" %in% names(df)) {
    valid_families <- names(valid_family_params)
    invalid_families <- setdiff(unique(df$family), valid_families)
    if (length(invalid_families) > 0) {
      errors <- c(errors, paste("Invalid family value(s):", paste(invalid_families, collapse = ", "),
                                "\nMust be one of:", paste(valid_families, collapse = ", ")))
    }
  }

  # Validate uncertainty_component column matches family
  if ("uncertainty_component" %in% names(df) && "family" %in% names(df)) {
    # Check each family type
    for (fam in names(valid_family_params)) {
      family_data <- df %>% filter(family == fam)
      if (nrow(family_data) > 0) {
        actual_params <- unique(family_data$uncertainty_component)
        expected_params <- valid_family_params[[fam]]

        if (fam == "sample") {
          # For non-parametric forecasts, uncertainty_component should be numeric
          if (!all(grepl("^[0-9]+$|^sample$", actual_params))) {
            errors <- c(errors, paste("Sample family should have numeric values (e.g. ensemble member, posterior draw, or replicate index)"))
          }
        } else {
          # For parametric distributions, check all required parameters are present
          missing_params <- setdiff(expected_params, actual_params)
          if (length(missing_params) > 0) {
            errors <- c(errors, paste0("Family '", fam, "' is missing required parameter(s): ",
                                       paste(missing_params, collapse = ", ")))
          }

          # Check for unexpected parameters
          extra_params <- setdiff(actual_params, expected_params)
          if (length(extra_params) > 0) {
            warnings <- c(warnings, paste0("Family '", fam, "' has unexpected parameter(s): ",
                                           paste(extra_params, collapse = ", ")))
          }
        }
      }
    }
  }

  # Validate prediction column
  if ("prediction" %in% names(df)) {
    if (!is.numeric(df$prediction)) {
      errors <- c(errors, "prediction column must be numeric")
    }
    if (any(is.na(df$prediction))) {
      errors <- c(errors, "prediction column contains NA values")
    }
  }

  # Spatial-specific validations
  if (forecast_type == "spatial") {
    # Check cell_id
    if ("cell_id" %in% names(df)) {
      if (any(is.na(df$cell_id))) {
        errors <- c(errors, "cell_id column contains NA values")
      }
      if (any(df$cell_id == "" | trimws(df$cell_id) == "")) {
        errors <- c(errors, "cell_id column contains empty strings")
      }
      # Check that cell_id is consistent with coordinates
      if ("cell_lon" %in% names(df) && "cell_lat" %in% names(df)) {
        cell_coords <- df %>%
          select(cell_id, cell_lon, cell_lat) %>%
          distinct()

        # Check if any cell_id has multiple coordinate pairs
        dup_cells <- cell_coords %>%
          group_by(cell_id) %>%
          filter(n() > 1) %>%
          pull(cell_id) %>%
          unique()

        if (length(dup_cells) > 0) {
          errors <- c(errors, paste("cell_id values have inconsistent coordinates:",
                                    paste(head(dup_cells, 5), collapse = ", ")))
        }
      }
    } else {
      errors <- c(errors, "Spatial forecasts must include cell_id column")
    }

    # Check coordinates
    if ("cell_lon" %in% names(df) && "cell_lat" %in% names(df)) {
      if (!is.numeric(df$cell_lon) || !is.numeric(df$cell_lat)) {
        errors <- c(errors, "cell_lon and cell_lat must be numeric")
      }
      if (any(is.na(df$cell_lon)) || any(is.na(df$cell_lat))) {
        errors <- c(errors, "cell_lon and cell_lat columns contain NA values")
      }
      if (any(df$cell_lon < -180 | df$cell_lon > 180, na.rm = TRUE)) {
        errors <- c(errors, "cell_lon values must be between -180 and 180")
      }
      if (any(df$cell_lat < -90 | df$cell_lat > 90, na.rm = TRUE)) {
        errors <- c(errors, "cell_lat values must be between -90 and 90")
      }
    } else {
      errors <- c(errors, "Spatial forecasts must include cell_lon and cell_lat columns")
    }
  }

  # Check for duplicate rows
  key_cols <- if (forecast_type == "spatial") {
    c("datetime", "site_id", "species", "cell_id", "uncertainty_component", "variable")
  } else {
    c("datetime", "site_id", "species", "uncertainty_component", "variable")
  }

  if (all(key_cols %in% names(df))) {
    dup_count <- df %>%
      group_by(across(all_of(key_cols))) %>%
      filter(n() > 1) %>%
      nrow()

    if (dup_count > 0) {
      errors <- c(errors, paste("Found", dup_count, "duplicate forecast rows"))
    }
  }

  # Return validation results
  valid <- length(errors) == 0

  return(list(
    valid = valid,
    errors = errors,
    warnings = warnings,
    n_rows = nrow(df),
    n_sites = length(unique(df$site_id)),
    n_cells = if(forecast_type == "temporal") NA else length(unique(df$cell_id)),
    n_species = if("species" %in% names(df)) length(unique(df$species)) else NA,
    forecast_type = forecast_type
  ))
}

# ============================================================
# 4. PRINT VALIDATION RESULTS
# ============================================================

print_validation_results <- function(results) {
  cat("\n========== VALIDATION RESULTS ==========\n")
  cat("Forecast Type:", results$forecast_type, "\n")
  cat("Number of Rows:", results$n_rows, "\n")
  cat("Number of Sites:", results$n_sites, "\n")
  cat("Number of Cells:", results$n_cells, "\n")
  cat("Number of Species:", results$n_species, "\n")
  cat("\nValid:", ifelse(results$valid, "✓ YES", "✗ NO"), "\n")
  
  if (length(results$errors) > 0) {
    cat("\n--- ERRORS ---\n")
    for (i in seq_along(results$errors)) {
      cat(i, ". ", results$errors[i], "\n", sep = "")
    }
  }
  
  if (length(results$warnings) > 0) {
    cat("\n--- WARNINGS ---\n")
    for (i in seq_along(results$warnings)) {
      cat(i, ". ", results$warnings[i], "\n", sep = "")
    }
  }
  
  if (results$valid) {
    cat("\n✓ File is ready for submission!\n")
  } else {
    cat("\n✗ Please fix errors before submitting.\n")
  }
  cat("========================================\n\n")
}

# ============================================================
# 5. GENERATE AND SAVE EXAMPLE FILES
# ============================================================

# Create dummy datasets
#temporal_data <- create_temporal_forecast()
#spatial_data <- create_spatial_forecast()

# Save to CSV
# write_csv(temporal_data, "example_temporal_forecast.csv")
# write_csv(spatial_data, "example_spatial_forecast.csv")
# 
# # Read from CSV
# temporal_data <- read.csv("example_temporal_forecast.csv")
# spatial_data <- read.csv("example_spatial_forecast.csv")
# 
# # Validate the example files
# temporal_results <- validate_forecast(temporal_data, "temporal")
# print_validation_results(temporal_results)
# 
# spatial_results <- validate_forecast(spatial_data, "spatial")
# print_validation_results(spatial_results)

