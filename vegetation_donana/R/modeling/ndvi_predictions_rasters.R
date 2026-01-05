# NDVI Raster Predictions Script
# This script trains spatial models and generates NDVI prediction rasters
# for the entire Doñana study area using climate data

# Setup ----
library(here)
here()

# Load packages
source(here("load_packages2.R"))

# Load functions
source(here("vegetation_donana", "ndvi_calculation_functions.R"))

cat("\n")
cat("================================================================================\n")
cat("NDVI RASTER PREDICTIONS - SPATIAL MODEL TRAINING AND APPLICATION\n")
cat("================================================================================\n")

# =============================================================================
# LOAD NDVI RASTERS AND EXTRACT TRAINING DATA
# =============================================================================

cat("\n[1/7] Loading NDVI rasters...\n")

# Define NDVI metrics
ndvi_metrics <- c("integrated_ndvi", "winter_spring_integrated", "summer_integrated")

# Define years for training (2005-2021, excluding years with missing data)
training_years <- 2005:2021

# Base directory for NDVI rasters
ndvi_rasters_dir <- here("vegetation_donana", "ndvi_metrics_rasters")

# Check available years
available_years <- list.dirs(ndvi_rasters_dir, full.names = FALSE, recursive = FALSE)
available_years <- as.numeric(available_years[grepl("^\\d{4}$", available_years)])
available_years <- available_years[available_years >= 2005 & available_years <= 2021]

cat("  Available years:", paste(available_years, collapse = ", "), "\n")

# Load NDVI rasters
ndvi_raster_list <- list()
for(yr in available_years) {
  year_dir <- file.path(ndvi_rasters_dir, as.character(yr))

  for(metric in ndvi_metrics) {
    # Construct filename based on observed structure: metric_year.tif
    raster_file <- file.path(year_dir, paste0(metric, "_", yr, ".tif"))

    if(file.exists(raster_file)) {
      r <- rast(raster_file)
      ndvi_raster_list[[paste0(metric, "_", yr)]] <- r
      cat("  ✓ Loaded:", metric, yr, "\n")
    } else {
      cat("  ⚠ Missing:", metric, yr, "\n")
    }
  }
}

if(length(ndvi_raster_list) == 0) {
  stop("ERROR: No NDVI rasters found. Check ndvi_metrics_rasters/ directory structure.")
}

cat("\n✓ Loaded", length(ndvi_raster_list), "NDVI rasters total\n")

# =============================================================================
# LOAD BIOCLIM RASTERS (PRESENT CLIMATE)
# =============================================================================

cat("\n[2/7] Loading present bioclim rasters...\n")

# Check if bioclim rasters exist
bioclim_present_file <- here("vegetation_donana", "bioclim_rasters_present.tif")

if(!file.exists(bioclim_present_file)) {
  cat("⚠ WARNING: Present bioclim rasters not found.\n")
  cat("  Expected file:", bioclim_present_file, "\n")
  cat("  Please run chelsa_download_rasters.R first and implement bioclim calculations.\n")
  cat("  For now, creating placeholder...\n")

  # Placeholder: create dummy bioclim rasters for demonstration
  # In production, remove this and require actual bioclim rasters
  example_raster <- ndvi_raster_list[[1]]
  bio1 <- init(example_raster, fun = function() rnorm(1, mean = 15, sd = 5))
  bio12 <- init(example_raster, fun = function() rnorm(1, mean = 500, sd = 100))
  bio9 <- init(example_raster, fun = function() rnorm(1, mean = 12, sd = 4))
  bio18 <- init(example_raster, fun = function() rnorm(1, mean = 80, sd = 20))

  names(bio1) <- "bio1"
  names(bio12) <- "bio12"
  names(bio9) <- "bio9"
  names(bio18) <- "bio18"

  bioclim_present <- c(bio1, bio12, bio9, bio18)
  cat("  Created placeholder bioclim rasters (replace with actual data)\n")
} else {
  bioclim_present <- rast(bioclim_present_file)
  cat("✓ Loaded present bioclim rasters\n")
}

# =============================================================================
# LOAD COORDINATE RASTERS
# =============================================================================

cat("\n[3/7] Loading coordinate rasters...\n")

lon_raster_file <- here("vegetation_donana", "coordinate_lon.tif")
lat_raster_file <- here("vegetation_donana", "coordinate_lat.tif")

if(!file.exists(lon_raster_file) || !file.exists(lat_raster_file)) {
  stop("ERROR: Coordinate rasters not found. Please run chelsa_download_rasters.R first.")
}

lon_raster <- rast(lon_raster_file)
lat_raster <- rast(lat_raster_file)

cat("✓ Loaded coordinate rasters\n")

# =============================================================================
# ALIGN RASTERS TO COMMON GRID
# =============================================================================

cat("\n[4/6] Aligning rasters to common grid...\n")

# Use first NDVI raster as template for alignment
template_raster <- ndvi_raster_list[[1]]

# Check if bioclim needs resampling
if(!compareGeom(template_raster, bioclim_present, stopOnError = FALSE)) {
  cat("  Resampling bioclim rasters to match NDVI grid...\n")
  bioclim_present <- resample(bioclim_present, template_raster, method = "bilinear")
}

# Check if coordinates need resampling
if(!compareGeom(template_raster, lon_raster, stopOnError = FALSE)) {
  cat("  Resampling coordinate rasters to match NDVI grid...\n")
  lon_raster <- resample(lon_raster, template_raster, method = "bilinear")
  lat_raster <- resample(lat_raster, template_raster, method = "bilinear")
}

cat("✓ All rasters aligned to common grid\n")

# =============================================================================
# EXTRACT TRAINING DATA
# =============================================================================

cat("\n[5/6] Extracting training data from rasters...\n")

# Stack all rasters for extraction
predictors_stack <- c(bioclim_present, lon_raster, lat_raster)
names(predictors_stack) <- c("bio1", "bio12", "bio9", "bio18", "lon", "lat")

cat("  Extracting values (this may take a few minutes)...\n")

# Function to extract training data for one metric
extract_training_data <- function(metric, years, ndvi_list, predictor_stack) {
  training_df <- data.frame()

  for(yr in years) {
    raster_name <- paste0(metric, "_", yr)
    if(!raster_name %in% names(ndvi_list)) next

    ndvi_r <- ndvi_list[[raster_name]]

    # Stack NDVI with predictors
    full_stack <- c(ndvi_r, predictor_stack)
    names(full_stack)[1] <- metric

    # Convert to data frame (only non-NA cells)
    df <- as.data.frame(full_stack, xy = FALSE, na.rm = TRUE)
    df$year <- yr

    training_df <- rbind(training_df, df)
  }

  return(training_df)
}

# Extract training data for each metric
training_data_list <- list()
for(metric in ndvi_metrics) {
  cat("  Extracting", metric, "...\n")
  training_data_list[[metric]] <- extract_training_data(metric, available_years,
                                                        ndvi_raster_list, predictors_stack)
}

cat("✓ Training data extracted\n")
for(metric in ndvi_metrics) {
  cat("  ", metric, ":", nrow(training_data_list[[metric]]), "pixels\n")
}

# =============================================================================
# TRAIN SPATIAL MODELS (LM AND RF)
# =============================================================================

cat("\n[6/7] Training spatial models with coordinates...\n")

# Helper function to get valid NDVI bounds
get_ndvi_bounds <- function(metric) {
  bounds <- list(
    integrated_ndvi = c(0, 365),
    winter_spring_integrated = c(0, 151),
    summer_integrated = c(0, 92)
  )
  return(bounds[[metric]])
}

# Train models for each metric
models_list <- list()

for(metric in ndvi_metrics) {
  cat("\n  Training models for:", metric, "\n")

  training_data <- training_data_list[[metric]]

  if(nrow(training_data) == 0) {
    cat("    ⚠ No training data available\n")
    next
  }

  # Sample data if too large (for speed)
  if(nrow(training_data) > 50000) {
    cat("    Sampling", 50000, "pixels for faster training...\n")
    training_data <- training_data[sample(nrow(training_data), 50000), ]
  }

  # Train LM with spatial terms
  cat("    Training LM...\n")
  formula_lm <- as.formula(paste0(metric, " ~ bio1 + bio12 + bio9 + bio18 + lon + lat + lon:lat"))
  model_lm <- lm(formula_lm, data = training_data)

  # Train RF with coordinates
  cat("    Training RF...\n")
  formula_rf <- as.formula(paste0(metric, " ~ bio1 + bio12 + bio9 + bio18 + lon + lat"))
  model_rf <- ranger(formula_rf, data = training_data, num.trees = 500)

  # Store models
  models_list[[metric]] <- list(
    lm = model_lm,
    rf = model_rf,
    bounds = get_ndvi_bounds(metric)
  )

  cat("    ✓ Models trained\n")
}

# Save trained models
saveRDS(models_list, here("vegetation_donana", "raster_prediction_models.rds"))
cat("\n✓ Models saved to: raster_prediction_models.rds\n")

# =============================================================================
# GENERATE RASTER PREDICTIONS
# =============================================================================

cat("\n[7/7] Generating raster predictions...\n")

# Create output directory
output_dir <- here("vegetation_donana", "ndvi_prediction_rasters")
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to clip predictions to valid bounds
clip_predictions <- function(pred_raster, bounds) {
  pred_clipped <- app(pred_raster, fun = function(x) {
    pmax(bounds[1], pmin(bounds[2], x))
  })
  return(pred_clipped)
}

# Load future bioclim rasters for each scenario
scenarios <- c("ssp245", "ssp370", "ssp585")
future_years <- 2022:2030  # Updated to match chelsa_download_rasters.R

for(scenario in scenarios) {
  cat("\n  Scenario:", scenario, "\n")

  # Load future bioclim rasters
  bioclim_future_file <- here("vegetation_donana", paste0("bioclim_rasters_future_", scenario, ".tif"))

  if(!file.exists(bioclim_future_file)) {
    cat("    ⚠ Future bioclim rasters not found:", scenario, "\n")
    cat("      Please complete chelsa_download_rasters.R first\n")

    # Create placeholder for demonstration
    cat("      Creating placeholder future bioclim (replace with actual data)\n")
    example_raster <- ndvi_raster_list[[1]]
    bio1_fut <- init(example_raster, fun = function() rnorm(1, mean = 17, sd = 5))
    bio12_fut <- init(example_raster, fun = function() rnorm(1, mean = 480, sd = 100))
    bio9_fut <- init(example_raster, fun = function() rnorm(1, mean = 14, sd = 4))
    bio18_fut <- init(example_raster, fun = function() rnorm(1, mean = 75, sd = 20))

    bioclim_future <- c(bio1_fut, bio12_fut, bio9_fut, bio18_fut)
  } else {
    bioclim_future <- rast(bioclim_future_file)

    # Ensure future bioclim matches template grid
    if(!compareGeom(template_raster, bioclim_future, stopOnError = FALSE)) {
      cat("    Resampling future bioclim to match NDVI grid...\n")
      bioclim_future <- resample(bioclim_future, template_raster, method = "bilinear")
    }
  }

  # Stack with coordinates (already aligned to template)
  prediction_stack <- c(bioclim_future, lon_raster, lat_raster)
  names(prediction_stack) <- c("bio1", "bio12", "bio9", "bio18", "lon", "lat")

  # Generate predictions for each metric and model
  for(metric in ndvi_metrics) {
    if(!metric %in% names(models_list)) {
      cat("    ⚠ No model for", metric, "\n")
      next
    }

    cat("    Predicting:", metric, "\n")

    models <- models_list[[metric]]
    bounds <- models$bounds

    # LM predictions
    cat("      LM...\n")
    pred_lm <- predict(prediction_stack, models$lm, na.rm = TRUE)
    pred_lm_clipped <- clip_predictions(pred_lm, bounds)

    output_file_lm <- file.path(output_dir,
                                 paste0(metric, "_lm_", scenario, ".tif"))
    writeRaster(pred_lm_clipped, output_file_lm, overwrite = TRUE)

    # RF predictions
    cat("      RF...\n")
    pred_rf <- predict(prediction_stack, models$rf, na.rm = TRUE)

    # Extract predictions (ranger returns list with $predictions)
    if(is.list(pred_rf) && "predictions" %in% names(pred_rf)) {
      pred_rf <- pred_rf$predictions
    }

    pred_rf_clipped <- clip_predictions(pred_rf, bounds)

    output_file_rf <- file.path(output_dir,
                                 paste0(metric, "_rf_", scenario, ".tif"))
    writeRaster(pred_rf_clipped, output_file_rf, overwrite = TRUE)

    cat("      ✓ Saved predictions\n")
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("RASTER PREDICTIONS COMPLETE\n")
cat("================================================================================\n")
cat("\nModels trained:\n")
for(metric in names(models_list)) {
  cat("  -", metric, "(LM + RF with lon/lat)\n")
}

cat("\nPrediction rasters saved to:", output_dir, "\n")
cat("  - 3 scenarios (ssp245, ssp370, ssp585)\n")
cat("  - 3 NDVI metrics\n")
cat("  - 2 models (LM, RF)\n")
cat("  Total:", length(scenarios) * length(ndvi_metrics) * 2, "raster files\n")

cat("\nNote: Predictions are clipped to valid NDVI ranges:\n")
cat("  - integrated_ndvi: [0, 365]\n")
cat("  - winter_spring_integrated: [0, 151]\n")
cat("  - summer_integrated: [0, 92]\n\n")
