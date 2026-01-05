# NDVI Raster Processing Script
library(terra)
library(tidyverse)
library(lubridate)
library(here)
library(signal)
library(zoo)

# Function to apply BISE correction and interpolation to a single cell
process_cell_ndvi <- function(ndvi_values, dates) {
  # Initial validation
  if(all(is.na(ndvi_values))) {
    return(data.frame(
      date = dates,
      int.NDVI = NA
    ))
  }
  
  # Ensure values are numeric and handle any special values
  ndvi_values <- as.numeric(ndvi_values)
  ndvi_values[ndvi_values == 255] <- NA  # Handle no-data values
  
  # Validate NDVI range
  if(any(!is.na(ndvi_values) & (ndvi_values < -1 | ndvi_values > 1))) {
    warning("Found NDVI values outside expected range [-1, 1]")
  }
  
  # Create initial dataframe
  raw_ndvi_df <- data.frame(
    date = dates,
    ndvi = ndvi_values
  ) %>%
    arrange(date)
  
  # Initialize vectors for processed data
  ndvi_raw <- raw_ndvi_df$ndvi
  ndvi_processed <- ndvi_raw
  
  # Validate data before BISE correction
  if(all(is.na(ndvi_raw)) || length(ndvi_raw) < 2) {
    return(data.frame(
      date = dates,
      int.NDVI = rep(NA, length(dates))
    ))
  }
  
  # 1. BISE correction
  tryCatch({
    diff_vals <- diff(ndvi_raw)
    decrease_idx <- which(diff_vals < 0) + 1  # points showing decrease
    
    if(length(decrease_idx) > 0) {
      # Using a slope threshold of 0.2 (20%)
      data_threshold <- c(NA, ndvi_raw[-1] - 0.2 * diff_vals)
      
      # Forward sliding period (3 values window)
      val_sliding_period <- running(ndvi_raw, fun=max, width=3)
      val_sliding_period <- c(val_sliding_period, NA, NA)
      
      # Identify points to reject
      law_check <- val_sliding_period[decrease_idx] - data_threshold[decrease_idx]
      reject_decrease <- decrease_idx[which(law_check > 0)]
      
      # Check for sudden increases
      increase_idx <- which(diff_vals > 0.2)
      reject_increase <- increase_idx[!increase_idx %in% decrease_idx]
      reject_points <- c(reject_increase, reject_decrease)
      
      # Apply corrections
      ndvi_processed[reject_points] <- NA
    }
    
    # 2. Fill start and end gaps
    if(length(ndvi_processed) >= 11) {
      start_window <- ndvi_processed[7:11]
      ndvi_processed[1:6] <- start_window[!is.na(start_window)][1]
      
      end_window <- na.omit(ndvi_processed[(length(ndvi_processed)-4):(length(ndvi_processed)-1)])
      if(length(end_window) > 0) {
        ndvi_processed[length(ndvi_processed)] <- min(end_window)
      } else {
        ndvi_processed[length(ndvi_processed)] <- 0
      }
    }
    
    # 3. Interpolate missing values and apply Savitzky-Golay filter
    ndvi_interpolated <- na.spline(ndvi_processed)
    ndvi_smoothed <- sgolayfilt(ndvi_interpolated, p=3, n=7, m=0)
    
    # Create daily interpolation
    dates_seq <- seq(min(raw_ndvi_df$date), max(raw_ndvi_df$date), by="days")
    daily_values <- spline(x=as.numeric(raw_ndvi_df$date), 
                           y=ndvi_smoothed, 
                           xout=as.numeric(dates_seq))$y
    
    # Return results
    result_df <- data.frame(
      date = dates_seq,
      int.NDVI = daily_values
    )
    
    return(result_df)
  }, error = function(e) {
    warning(paste("Error in BISE correction:", e$message))
    return(data.frame(
      date = dates,
      int.NDVI = rep(NA, length(dates))
    ))
  })
}

# Function to calculate metrics from processed NDVI
calculate_cell_metrics <- function(processed_df) {
  if(all(is.na(processed_df$int.NDVI))) {
    return(data.frame(
      int.NDVI = NA,
      annual_mean = NA,
      annual_median = NA,
      winter_spring_mean = NA,
      winter_spring_median = NA,
      winter_spring_max = NA,
      winter_spring_integrated = NA,
      summer_mean = NA,
      summer_median = NA,
      summer_max = NA,
      summer_integrated = NA
    ))
  }
  
  # Add month information
  processed_df$month <- month(processed_df$date)
  
  # Calculate metrics
  metrics <- processed_df %>%
    summarise(
      # Annual metrics
      integrated_ndvi = sum(int.NDVI, na.rm = TRUE),
      annual_mean = mean(int.NDVI, na.rm = TRUE),
      annual_median = median(int.NDVI, na.rm = TRUE),
      
      # Winter-Spring metrics (January-May)
      winter_spring_mean = mean(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_median = median(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_max = max(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_integrated = sum(int.NDVI[month %in% 1:5], na.rm = TRUE),
      
      # Summer metrics (July-September)
      summer_mean = mean(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_median = median(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_max = max(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_integrated = sum(int.NDVI[month %in% 7:9], na.rm = TRUE)
    )
  
  return(metrics)
}

# Process raster stack
process_raster_ndvi <- function(raster_files) {
  # Read all rasters into a stack
  ndvi_stack <- rast(raster_files)
  
  # Extract dates from filenames
  dates <- as.Date(str_extract(raster_files, "\\d{8}"), format = "%Y%m%d")
  
  # Create empty rasters for each metric
  metrics <- c("integrated_ndvi", "annual_mean", "annual_median",
               "winter_spring_mean", "winter_spring_median", "winter_spring_max", 
               "winter_spring_integrated", "summer_mean", "summer_median",
               "summer_max", "summer_integrated")
  
  result_rasters <- lapply(metrics, function(x) {
    rast(ndvi_stack[[1]])  # Use first raster as template
  })
  names(result_rasters) <- metrics
  
  # Optimized chunked processing
  total_cells <- ncell(ndvi_stack)
  chunk_size <- 10000  # Process 10k cells at once
  n_chunks <- ceiling(total_cells / chunk_size)

  cat("\nProcessing", total_cells, "cells in", n_chunks, "chunks...\n")
  start_time <- Sys.time()

  for(chunk in 1:n_chunks) {
    chunk_start <- (chunk - 1) * chunk_size + 1
    chunk_end <- min(chunk * chunk_size, total_cells)
    chunk_cells <- chunk_start:chunk_end

    cat(sprintf("Processing chunk %d/%d (cells %d-%d)...\n",
                chunk, n_chunks, chunk_start, chunk_end))

    # Extract values for entire chunk at once (more efficient)
    chunk_values <- tryCatch({
      values(ndvi_stack)[chunk_cells, , drop = FALSE]
    }, error = function(e) {
      warning(paste("Error reading chunk", chunk, ":", e$message))
      return(matrix(NA, nrow = length(chunk_cells), ncol = length(dates)))
    })

    # Process each cell in the chunk
    chunk_results <- lapply(metrics, function(x) rep(NA, length(chunk_cells)))
    names(chunk_results) <- metrics

    for(i in 1:nrow(chunk_values)) {
      cell_values <- chunk_values[i, ]

      # Skip if all NA
      if(all(is.na(cell_values))) {
        next
      }

      # Process cell values with BISE correction and interpolation
      processed_df <- process_cell_ndvi(cell_values, dates)

      # Calculate metrics from processed data
      cell_metrics <- calculate_cell_metrics(processed_df)

      # Store results
      for(metric in metrics) {
        chunk_results[[metric]][i] <- cell_metrics[[metric]]
      }
    }

    # Assign chunk results to output rasters
    for(metric in metrics) {
      values(result_rasters[[metric]])[chunk_cells] <- chunk_results[[metric]]
    }

    # Progress update with time estimate
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    progress <- chunk / n_chunks
    estimated_total <- elapsed / progress
    remaining <- estimated_total - elapsed

    cat(sprintf("Chunk complete. Elapsed: %.1f min, Est. remaining: %.1f min\n",
                elapsed, remaining))
  }
  
  # Stack all result rasters
  result_stack <- rast(result_rasters)
  
  return(result_stack)
}

# Main execution
main <- function(years = NULL, save_intermediates = FALSE, debug = TRUE) {
  if(debug) {
    cat("\nSystem information:")
    cat("\nWorking directory:", getwd())
    cat("\nR version:", R.version.string)
    cat("\nPackage versions:")
    cat("\n  terra:", as.character(packageVersion("terra")))
    cat("\n  tidyverse:", as.character(packageVersion("tidyverse")))
    cat("\n  here:", as.character(packageVersion("here")))
    cat("\n")
  }
  
  # Get all raster files
  raster_files <- list.files(here("vegetation_donana", "NDVI_rasters"),
                             pattern = "\\.tif$",
                             full.names = TRUE)

  if(length(raster_files) == 0) {
    stop("No NDVI raster files found in NDVI_rasters folder")
  }

  # Extract years from filenames and group files by year
  dates <- as.Date(str_extract(basename(raster_files), "\\d{8}"), format = "%Y%m%d")
  file_years <- year(dates)

  # Create a dataframe with files and years
  file_year_df <- data.frame(
    file = raster_files,
    year = file_years,
    stringsAsFactors = FALSE
  )

  # Get unique years
  all_years <- sort(unique(file_years))

  # Filter years if specified
  if(!is.null(years)) {
    unique_years <- all_years[all_years %in% years]
    if(length(unique_years) == 0) {
      stop("None of the specified years (", paste(years, collapse = ", "),
           ") found in available data (", min(all_years), "-", max(all_years), ")")
    }
    cat("Processing specified years:", paste(unique_years, collapse = ", "), "\n")
  } else {
    unique_years <- all_years
    cat("Processing all available years\n")
  }

  # Print the files found for debugging
  cat("Found", length(raster_files), "raster files:\n")
  cat("Years available:", min(all_years), "to", max(all_years), "\n")
  cat("Years to process:", length(unique_years), "years\n")

  # Create main output directory
  main_output_dir <- here("vegetation_donana", "ndvi_metrics_rasters")
  if(!dir.exists(main_output_dir)) {
    dir.create(main_output_dir)
  }

  # Process rasters for metrics year by year
  for(current_year in unique_years) {
    cat("\nProcessing year", current_year, "...\n")

    # Get files for current year
    year_files <- file_year_df$file[file_year_df$year == current_year]

    if(length(year_files) == 0) {
      cat("No files found for year", current_year, "- skipping\n")
      next
    }

    cat("Found", length(year_files), "files for year", current_year, "\n")

    # Create year-specific output directory
    year_output_dir <- file.path(main_output_dir, as.character(current_year))
    if(!dir.exists(year_output_dir)) {
      dir.create(year_output_dir)
    }

    # Process rasters for this year
    result_stack <- process_raster_ndvi(year_files)

    # Save metric results
    for(i in 1:nlyr(result_stack)) {
      metric_name <- names(result_stack)[i]
      writeRaster(result_stack[[i]],
                  filename = file.path(year_output_dir, paste0(metric_name, "_", current_year, ".tif")),
                  overwrite = TRUE)
    }

    cat("Completed processing for year", current_year, "\n")
  }
  
  cat("Processing complete. Results saved in:", main_output_dir, "\n")
}

# Post-processing function to apply logical NDVI constraints
# There are a couple of outliers and a lot of negative values.
## I take out outliers based on the theoretical range, and take out completely the negatives, simply because we do not need them. 
apply_ndvi_constraints <- function(output_dir = NULL) {
  if(is.null(output_dir)) {
    output_dir <- here("vegetation_donana", "ndvi_metrics_rasters")
  }

  cat("\n=== Applying NDVI logical constraints ===\n")

  # Find all raster files
  raster_files <- list.files(output_dir, pattern = "\\.tif$",
                             recursive = TRUE, full.names = TRUE)

  if(length(raster_files) == 0) {
    cat("No raster files found in", output_dir, "\n")
    return(FALSE)
  }

  # Define constraints based on metric type
  constraints <- list(
    # Integrated metrics (days-based)
    "integrated_ndvi" = c(0, 365),           # Annual: 365 days max
    "winter_spring_integrated" = c(0, 151),  # Jan-May: ~151 days max
    "summer_integrated" = c(0, 92),          # Jul-Sep: ~92 days max

    # Mean and median metrics
    "annual_mean" = c(0, 1),
    "annual_median" = c(0, 1),
    "winter_spring_mean" = c(0, 1),
    "winter_spring_median" = c(0, 1),
    "summer_mean" = c(0, 1),
    "summer_median" = c(0, 1),

    # Max metrics (NDVI cannot exceed 1.0 by definition)
    "winter_spring_max" = c(0, 1.0),
    "summer_max" = c(0, 1.0)
  )

  # Process each file
  for(file in raster_files) {
    filename <- basename(file)

    # Extract metric name (everything before year)
    metric <- str_replace(filename, "_\\d{4}\\.tif$", "")

    if(metric %in% names(constraints)) {
      cat("Applying constraints to:", filename, "\n")

      # Load raster
      r <- rast(file)

      # Get constraints
      min_val <- constraints[[metric]][1]
      max_val <- constraints[[metric]][2]

      # Count violations before correction
      vals <- values(r, na.rm = TRUE)
      n_below <- sum(vals < min_val, na.rm = TRUE)
      n_above <- sum(vals > max_val, na.rm = TRUE)

      if(n_below > 0 || n_above > 0) {
        cat(sprintf("  Found %d values < %.1f and %d values > %.1f\n",
                    n_below, min_val, n_above, max_val))

        # Apply constraints
        r[r < min_val] <- NA  # Values below minimum -> NA (non-vegetation)
        r[r > max_val] <- NA  # Values above maximum -> clamp to maximum

        # Save corrected raster (overwrite original)
        writeRaster(r, filename = file, overwrite = TRUE)

        cat(sprintf("  Corrected and saved: %s\n", filename))
      } else {
        cat(sprintf("  No violations found in %s\n", filename))
      }
    } else {
      cat("  No constraints defined for metric:", metric, "\n")
    }
  }

  cat("\n=== Constraint application complete ===\n")
  return(TRUE)
}

# Run the script
# Test with a few years first:
main(years = 2009, debug = TRUE)

# Or run all years (will take a long time!):
# main(save_intermediates = FALSE, debug = TRUE)

# Apply post-processing constraints:
apply_ndvi_constraints()

# =============================================================================
# EXTRACT METRICS AT COORDINATE POINTS
# =============================================================================

# Function to extract NDVI metrics at coordinate points
extract_metrics_at_points <- function(output_dir = NULL) {
  if(is.null(output_dir)) {
    output_dir <- here("vegetation_donana", "ndvi_metrics_rasters")
  }

  cat("\n=== Extracting metrics at coordinate points ===\n")

  # Load coordinate files
  coords_2023 <- read.csv(here("vegetation_donana", "coordinates_2023_02.csv"))
  coords_2007 <- read.csv(here("vegetation_donana", "coords_plot_since2007.csv"))

  cat("Loaded", nrow(coords_2023), "points from coordinates_2023_02.csv\n")
  cat("Loaded", nrow(coords_2007), "points from coords_plot_since2007.csv\n")

  # Find all metric raster files
  raster_files <- list.files(output_dir, pattern = "\\.tif$",
                             recursive = TRUE, full.names = TRUE)

  if(length(raster_files) == 0) {
    cat("No raster files found for extraction\n")
    return(FALSE)
  }

  # Function to extract from one coordinate set
  extract_for_coords <- function(coords_df, coord_names, output_filename) {

    cat("\nProcessing", nrow(coords_df), "coordinates for", output_filename, "\n")

    # Create proper spatial object with CRS
    if("lat" %in% names(coords_df) && "lon" %in% names(coords_df)) {
      # coordinates_2023_02.csv format
      coords_sf <- st_as_sf(coords_df, coords = c("lon", "lat"), crs = 4326)
    } else if("Long" %in% names(coords_df) && "Lat" %in% names(coords_df)) {
      # coords_plot_since2007.csv format
      coords_sf <- st_as_sf(coords_df, coords = c("Long", "Lat"), crs = 4326)
    } else {
      stop("Could not find longitude/latitude columns in coordinates")
    }

    cat("  Created spatial object with", nrow(coords_sf), "points in CRS:", st_crs(coords_sf)$input, "\n")

    # Initialize results dataframe with original coordinate info (preserve ALL original columns)
    results_df <- coords_df

    # Add back the coordinate columns explicitly since st_as_sf removes them
    if("lat" %in% names(coords_df) && "lon" %in% names(coords_df)) {
      # Already have lat/lon in original dataframe
    } else if("Long" %in% names(coords_df) && "Lat" %in% names(coords_df)) {
      # Already have Long/Lat in original dataframe
    }

    cat("  Original columns preserved:", ncol(results_df), "columns\n")
    cat("  Column names:", paste(names(results_df), collapse = ", "), "\n")

    # Create a list to store all extraction results
    all_extractions <- list()

    # Extract from each raster file
    for(raster_file in raster_files) {
      filename <- basename(raster_file)

      # Extract metric name and year
      metric <- str_replace(filename, "_\\d{4}\\.tif$", "")
      year <- as.numeric(str_extract(filename, "\\d{4}"))

      cat("  Extracting", metric, "for year", year, "\n")

      # Load raster and extract values
      tryCatch({
        r <- rast(raster_file)

        # Check and match CRS
        raster_crs <- crs(r)

        # Transform coordinates to match raster CRS if needed
        coords_transformed <- st_transform(coords_sf, crs = raster_crs)

        # Extract values using proper spatial object
        extracted_values <- terra::extract(r, coords_transformed)[,2]  # [,2] to get values, not IDs

        # Store extraction result
        extraction_df <- data.frame(
          point_id = 1:nrow(coords_df),
          year = year,
          metric = metric,
          value = extracted_values
        )

        all_extractions[[length(all_extractions) + 1]] <- extraction_df

      }, error = function(e) {
        cat("    Error extracting from", filename, ":", e$message, "\n")
      })
    }

    # Combine all extractions and reshape to long format
    if(length(all_extractions) > 0) {
      combined_extractions <- bind_rows(all_extractions)

      # Pivot to wide format with one column per metric
      metrics_wide <- combined_extractions %>%
        pivot_wider(names_from = metric, values_from = value)

      # Create final results by expanding original data for each year
      available_years <- sort(unique(metrics_wide$year))

      # Replicate original data for each year
      final_results <- results_df[rep(1:nrow(results_df), each = length(available_years)), ]
      final_results$year <- rep(available_years, nrow(results_df))
      final_results$point_id <- rep(1:nrow(results_df), each = length(available_years))

      # Join with extracted metrics
      final_results <- final_results %>%
        left_join(metrics_wide, by = c("point_id", "year")) %>%
        select(-point_id)  # Remove helper column

      results_df <- final_results
    }

    # Save results
    output_path <- here("vegetation_donana", output_filename)
    write.csv(results_df, file = output_path, row.names = FALSE)
    cat("  Saved results to:", output_filename, "\n")

    return(results_df)
  }

  # Extract for both coordinate sets
  results_2023 <- extract_for_coords(coords_2023, c("lon", "lat"), "ndvi_metrics_land_updated.csv")
  results_2007 <- extract_for_coords(coords_2007, c("Long", "Lat"), "ndvi_metrics_updated.csv")

  cat("\n=== Extraction complete ===\n")
  cat("Results saved as:\n")
  cat("  - df.ndvi_land_2023.csv (", nrow(results_2023), "rows )\n")
  cat("  - df.ndvi_land_plots.csv (", nrow(results_2007), "rows )\n")

  return(list(land = results_2023, plots = results_2007))
}

# Extract metrics at coordinate points:
extract_metrics_at_points()

# =============================================================================
# COMPARISON FIGURE: Raster metrics vs CSV metrics ranges
# =============================================================================

# Function to create comparison figure
create_comparison_figure <- function() {
  cat("\nCreating comparison figure between raster and CSV metrics...\n")

  # Load CSV metrics data
  csv_metrics_file <- here("vegetation_donana", "ndvi_metrics.csv")
  if(!file.exists(csv_metrics_file)) {
    cat("Warning: ndvi_metrics.csv not found. Skipping comparison.\n")
    return(NULL)
  }

  csv_metrics <- read.csv(csv_metrics_file)

  # Calculate ranges for CSV metrics - only keep metrics that match raster output
  raster_metric_names <- c("integrated_ndvi", "annual_mean", "annual_median",
                           "winter_spring_mean", "winter_spring_median", "winter_spring_max",
                           "winter_spring_integrated", "summer_mean", "summer_median",
                           "summer_max", "summer_integrated")

  # Select only matching columns that exist in both datasets
  available_metrics <- base::intersect(raster_metric_names, names(csv_metrics))

  csv_metrics_clean <- csv_metrics %>%
    select(all_of(available_metrics))

  # Calculate ranges for CSV metrics
  csv_ranges <- data.frame()
  for(metric in available_metrics) {
    metric_values <- csv_metrics_clean[[metric]]
    csv_ranges <- rbind(csv_ranges,
                        data.frame(
                          metric = metric,
                          min = min(metric_values, na.rm = TRUE),
                          max = max(metric_values, na.rm = TRUE),
                          source = "CSV_plots",
                          range = max(metric_values, na.rm = TRUE) - min(metric_values, na.rm = TRUE)
                        ))
  }

  # Get raster metrics ranges
  raster_output_dir <- here("vegetation_donana", "ndvi_metrics_rasters")

  if(!dir.exists(raster_output_dir)) {
    cat("Warning: Raster metrics directory not found. Skipping comparison.\n")
    return(NULL)
  }

  # Find all raster files
  raster_files <- list.files(raster_output_dir,
                             pattern = "\\.tif$",
                             recursive = TRUE,
                             full.names = TRUE)

  if(length(raster_files) == 0) {
    cat("Warning: No raster metric files found. Skipping comparison.\n")
    return(NULL)
  }

  # Extract ranges from rasters
  raster_ranges <- data.frame()

  # Extract metric names from raster files (everything before the year)
  # Pattern: metric_name_YEAR.tif -> extract metric_name
  metric_names <- unique(str_replace(basename(raster_files), "_\\d{4}\\.tif$", ""))

  for(metric in metric_names) {
    metric_files <- raster_files[grepl(paste0("^", metric, "_\\d{4}\\.tif$"), basename(raster_files))]

    if(length(metric_files) > 0) {
      # Calculate overall min and max across all years for this metric
      all_values <- c()

      for(file in metric_files) {
        tryCatch({
          rast_data <- rast(file)
          rast_data[rast_data<0]<-NA
          values_vec <- values(rast_data, na.rm = TRUE)
          all_values <- c(all_values, values_vec)
        }, error = function(e) {
          cat("Warning: Could not read", basename(file), "\n")
        })
      }

      if(length(all_values) > 0) {
        raster_ranges <- rbind(raster_ranges,
                               data.frame(
                                 metric = metric,
                                 min = min(all_values, na.rm = TRUE),
                                 max = max(all_values, na.rm = TRUE),
                                 source = "Raster_spatial",
                                 range = max(all_values, na.rm = TRUE) - min(all_values, na.rm = TRUE)
                               ))
      }
    }
  }

  # Combine data for comparison
  if(nrow(raster_ranges) > 0) {
    comparison_data <- rbind(csv_ranges, raster_ranges)

    # Create comparison plot
    p1 <- ggplot(comparison_data, aes(x = metric, y = range, fill = source)) +
      geom_col(position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "NDVI Metrics Range Comparison",
           subtitle = "Comparing ranges between plot-based CSV data and spatial raster data",
           x = "NDVI Metric",
           y = "Range (Max - Min)",
           fill = "Data Source") +
      scale_fill_manual(values = c("CSV_plots" = "#2E86AB", "Raster_spatial" = "#A23B72"))

    p2 <- ggplot(comparison_data, aes(x = min, y = max, color = source)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      facet_wrap(~metric, scales = "free") +
      theme_bw() +
      labs(title = "Min vs Max Values by Metric",
           x = "Minimum Value",
           y = "Maximum Value",
           color = "Data Source") +
      scale_color_manual(values = c("CSV_plots" = "#2E86AB", "Raster_spatial" = "#A23B72"))

    # Save plots
    ggsave(filename = here("vegetation_donana", "ndvi_metrics_comparison.pdf"),
           plot = ggarrange(p1, p2, ncol = 1),
           width = 12, height = 10)

    cat("Comparison figure saved as: ndvi_metrics_comparison.pdf\n")

    # Print summary statistics
    cat("\nSummary of metric ranges:\n")
    print(comparison_data %>%
          select(metric, source, range) %>%
          pivot_wider(names_from = source, values_from = range))

    return(comparison_data)
  } else {
    cat("Warning: No valid raster data found for comparison.\n")
    return(NULL)
  }
}

# Uncomment the line below to run the comparison (for testing)
create_comparison_figure()
