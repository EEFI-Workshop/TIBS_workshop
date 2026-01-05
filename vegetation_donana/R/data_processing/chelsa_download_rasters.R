# CHELSA Climate Data Download and Processing for Raster Predictions
# This script downloads CHELSA climate data for the entire Doñana study area
# and calculates bioclim variables as raster stacks

# Setup ----
library(here)
here()

# Load packages
source(here("load_packages2.R"))

# Load functions
source(here("vegetation_donana", "ndvi_calculation_functions.R"))

cat("\n")
cat("================================================================================\n")
cat("CHELSA CLIMATE DATA DOWNLOAD FOR RASTER PREDICTIONS\n")
cat("================================================================================\n")

# =============================================================================
# GET SPATIAL EXTENT FROM NDVI RASTERS
# =============================================================================

cat("\n[1/5] Getting spatial extent from NDVI rasters...\n")

# NDVI rasters are organized in year subdirectories
ndvi_rasters_dir <- here("vegetation_donana", "ndvi_metrics_rasters")

# Find first available year directory with rasters (starting from 2005)
available_years <- list.dirs(ndvi_rasters_dir, full.names = FALSE, recursive = FALSE)
available_years <- as.numeric(available_years[grepl("^\\d{4}$", available_years)])
available_years <- sort(available_years[available_years >= 2005])

if(length(available_years) == 0) {
  stop("ERROR: No year directories found in ndvi_metrics_rasters/")
}

# Get first raster file from first available year
first_year_dir <- file.path(ndvi_rasters_dir, as.character(available_years[1]))
ndvi_raster_files <- list.files(first_year_dir, pattern = "\\.tif$", full.names = TRUE)

if(length(ndvi_raster_files) == 0) {
  stop("ERROR: No NDVI rasters found in ", first_year_dir)
}

# Load first raster to get extent and resolution
example_raster <- rast(ndvi_raster_files[1])
study_extent <- ext(example_raster)
study_crs <- crs(example_raster)

cat("✓ Using example raster from year:", available_years[1], "\n")
cat("✓ Study extent:\n")
cat("  xmin:", study_extent$xmin, "  xmax:", study_extent$xmax, "\n")
cat("  ymin:", study_extent$ymin, "  ymax:", study_extent$ymax, "\n")
cat("  CRS:", study_crs, "\n")

# =============================================================================
# DOWNLOAD PRESENT CLIMATE DATA (2005-2021)
# =============================================================================

cat("\n[2/5] Downloading CHELSA present climate data (2005-2021)...\n")

# Convert UTM extent to lat/lon for CHELSA download
example_raster_latlon <- project(example_raster, "EPSG:4326")
extent_latlon <- ext(example_raster_latlon)

cat("  Geographic extent (lat/lon):\n")
cat("    xmin:", extent_latlon$xmin, "  xmax:", extent_latlon$xmax, "\n")
cat("    ymin:", extent_latlon$ymin, "  ymax:", extent_latlon$ymax, "\n")

present_years <- 2005:2021
chelsa_output_present <- here("vegetation_donana", "chelsa_rasters_present")

if(!dir.exists(chelsa_output_present)) {
  dir.create(chelsa_output_present, recursive = TRUE)
}

# Check which years are already downloaded
existing_years <- c()
for(yr in present_years) {
  tas_file <- file.path(chelsa_output_present, paste0("tas_", yr, ".tif"))
  pr_file <- file.path(chelsa_output_present, paste0("pr_", yr, ".tif"))
  if(file.exists(tas_file) && file.exists(pr_file)) {
    existing_years <- c(existing_years, yr)
  }
}

years_to_download <- setdiff(present_years, existing_years)

if(length(years_to_download) > 0) {
  cat("  Downloading years:", paste(years_to_download, collapse = ", "), "\n\n")

  for(yr in years_to_download) {
    cat("  Processing year", yr, "...\n")

    # Download temperature (tas)
    cat("    Downloading temperature (tas)...\n")
    tas_raster <- getChelsa(
      var = "tas",
      extent = c(extent_latlon$xmin, extent_latlon$xmax,
                 extent_latlon$ymin, extent_latlon$ymax),
      startdate = as.Date(paste0(yr, "-01-01")),
      enddate = as.Date(paste0(yr, "-12-31")),
      version = "CHELSA",
      freq = "daily",
      protocol = "vsicurl",
      verbose = FALSE
    )

    # Download precipitation (pr)
    cat("    Downloading precipitation (pr)...\n")
    pr_raster <- getChelsa(
      var = "pr",
      extent = c(extent_latlon$xmin, extent_latlon$xmax,
                 extent_latlon$ymin, extent_latlon$ymax),
      startdate = as.Date(paste0(yr, "-01-01")),
      enddate = as.Date(paste0(yr, "-12-31")),
      version = "CHELSA",
      freq = "daily",
      protocol = "vsicurl",
      verbose = FALSE
    )

    # Reproject to match NDVI rasters (UTM)
    cat("    Reprojecting to UTM...\n")
    tas_utm <- project(tas_raster, study_crs)
    pr_utm <- project(pr_raster, study_crs)

    # Save rasters
    writeRaster(tas_utm,
                file.path(chelsa_output_present, paste0("tas_", yr, ".tif")),
                overwrite = TRUE)
    writeRaster(pr_utm,
                file.path(chelsa_output_present, paste0("pr_", yr, ".tif")),
                overwrite = TRUE)

    cat("    ✓ Saved temperature and precipitation rasters\n\n")
  }

  cat("✓ Present climate download complete\n")

} else {
  cat("✓ All present climate years already downloaded\n")
}

# =============================================================================
# DOWNLOAD FUTURE CLIMATE DATA (2022-2030)
# =============================================================================

cat("\n[3/5] Downloading CHELSA future climate data (2022-2030)...\n")

future_years <- 2022:2030
scenarios <- c("ssp245", "ssp370", "ssp585")

for(scenario in scenarios) {
  cat("\n  Scenario:", scenario, "\n")

  chelsa_output_future <- here("vegetation_donana", paste0("chelsa_rasters_future_", scenario))

  if(!dir.exists(chelsa_output_future)) {
    dir.create(chelsa_output_future, recursive = TRUE)
  }

  # Check which years are already downloaded
  existing_years <- c()
  for(yr in future_years) {
    tas_file <- file.path(chelsa_output_future, paste0("tas_", yr, ".tif"))
    pr_file <- file.path(chelsa_output_future, paste0("pr_", yr, ".tif"))
    if(file.exists(tas_file) && file.exists(pr_file)) {
      existing_years <- c(existing_years, yr)
    }
  }

  years_to_download <- setdiff(future_years, existing_years)

  if(length(years_to_download) > 0) {
    cat("    Downloading years:", paste(years_to_download, collapse = ", "), "\n\n")

    for(yr in years_to_download) {
      cat("    Processing year", yr, "...\n")

      # Download temperature (tas) - future projections
      cat("      Downloading temperature (tas)...\n")
      tas_raster <- getChelsa(
        var = "tas",
        extent = c(extent_latlon$xmin, extent_latlon$xmax,
                   extent_latlon$ymin, extent_latlon$ymax),
        startdate = as.Date(paste0(yr, "-01-01")),
        enddate = as.Date(paste0(yr, "-12-31")),
        version = paste0("CMIP6_", scenario),  # Use CMIP6 scenario version
        freq = "daily",
        protocol = "vsicurl",
        verbose = FALSE
      )

      # Download precipitation (pr) - future projections
      cat("      Downloading precipitation (pr)...\n")
      pr_raster <- getChelsa(
        var = "pr",
        extent = c(extent_latlon$xmin, extent_latlon$xmax,
                   extent_latlon$ymin, extent_latlon$ymax),
        startdate = as.Date(paste0(yr, "-01-01")),
        enddate = as.Date(paste0(yr, "-12-31")),
        version = paste0("CMIP6_", scenario),  # Use CMIP6 scenario version
        freq = "daily",
        protocol = "vsicurl",
        verbose = FALSE
      )

      # Reproject to match NDVI rasters (UTM)
      cat("      Reprojecting to UTM...\n")
      tas_utm <- project(tas_raster, study_crs)
      pr_utm <- project(pr_raster, study_crs)

      # Save rasters
      writeRaster(tas_utm,
                  file.path(chelsa_output_future, paste0("tas_", yr, ".tif")),
                  overwrite = TRUE)
      writeRaster(pr_utm,
                  file.path(chelsa_output_future, paste0("pr_", yr, ".tif")),
                  overwrite = TRUE)

      cat("      ✓ Saved temperature and precipitation rasters\n\n")
    }

    cat("    ✓", scenario, "download complete\n")

  } else {
    cat("    ✓ All years already downloaded\n")
  }
}

# =============================================================================
# CALCULATE BIOCLIM VARIABLES AS RASTERS
# =============================================================================

cat("\n[4/5] Calculating bioclim variables as rasters...\n")

# Function to calculate bioclim from daily temperature and precipitation rasters
calculate_bioclim_rasters <- function(tas_raster, pr_raster) {
  # tas_raster: Daily temperature raster stack (365/366 layers)
  # pr_raster: Daily precipitation raster stack (365/366 layers)

  # Get dates from layer names
  dates <- time(tas_raster)
  if(is.null(dates)) {
    # If no dates, assume one year
    dates <- seq.Date(as.Date("2000-01-01"), by = "day", length.out = nlyr(tas_raster))
  }

  # Add month information
  months <- as.numeric(format(dates, "%m"))

  # Calculate monthly means/totals
  monthly_tas_mean <- tapp(tas_raster, months, fun = mean, na.rm = TRUE)
  monthly_tas_min <- tapp(tas_raster, months, fun = min, na.rm = TRUE)
  monthly_tas_max <- tapp(tas_raster, months, fun = max, na.rm = TRUE)
  monthly_pr_sum <- tapp(pr_raster, months, fun = sum, na.rm = TRUE)

  # BIO1: Annual Mean Temperature
  bio1 <- mean(monthly_tas_mean)

  # BIO12: Annual Precipitation
  bio12 <- sum(monthly_pr_sum)

  # BIO9: Mean Temperature of Driest Quarter
  # Find driest quarter (3 consecutive months with minimum precipitation)
  # Simple approach: use minimum quarterly precipitation month as proxy
  quarterly_pr <- c()
  quarterly_tas <- c()
  for(i in 1:12) {
    # 3-month window (handle wrap-around)
    idx <- ((i-2):(i)) %% 12 + 1
    idx[idx == 0] <- 12
    quarterly_pr <- c(quarterly_pr, i)
    quarterly_tas <- c(quarterly_tas, i)
  }
  # Use approximate: temperature of month with minimum precipitation
  min_pr_month <- which.min(app(monthly_pr_sum, fun = sum))
  bio9 <- monthly_tas_mean[[min_pr_month]]

  # BIO18: Precipitation of Warmest Quarter
  # Use approximate: precipitation of month with maximum temperature
  max_tas_month <- which.max(app(monthly_tas_mean, fun = sum))
  bio18 <- monthly_pr_sum[[max_tas_month]]

  # Stack bioclim variables
  bioclim_stack <- c(bio1, bio12, bio9, bio18)
  names(bioclim_stack) <- c("bio1", "bio12", "bio9", "bio18")

  return(bioclim_stack)
}

# For present climate (average across all years)
cat("  Present climate (2005-2021):\n")
present_bioclim_file <- here("vegetation_donana", "bioclim_rasters_present.tif")

if(!file.exists(present_bioclim_file)) {
  cat("    Loading present climate rasters...\n")

  # Load all present climate years
  all_tas <- list()
  all_pr <- list()

  for(yr in present_years) {
    tas_file <- file.path(chelsa_output_present, paste0("tas_", yr, ".tif"))
    pr_file <- file.path(chelsa_output_present, paste0("pr_", yr, ".tif"))

    if(file.exists(tas_file) && file.exists(pr_file)) {
      all_tas[[as.character(yr)]] <- rast(tas_file)
      all_pr[[as.character(yr)]] <- rast(pr_file)
    }
  }

  if(length(all_tas) > 0) {
    cat("    Calculating bioclim variables for", length(all_tas), "years...\n")

    # Calculate bioclim for each year and average
    bioclim_list <- list()
    for(i in seq_along(all_tas)) {
      cat("      Processing year", names(all_tas)[i], "...\n")
      bioclim_list[[i]] <- calculate_bioclim_rasters(all_tas[[i]], all_pr[[i]])
    }

    # Average bioclim across years
    cat("    Averaging bioclim variables across years...\n")
    bioclim_present <- mean(rast(bioclim_list))
    names(bioclim_present) <- c("bio1", "bio12", "bio9", "bio18")

    # Save
    writeRaster(bioclim_present, present_bioclim_file, overwrite = TRUE)
    cat("    ✓ Saved:", present_bioclim_file, "\n")
  } else {
    cat("    ⚠ No present climate data found to calculate bioclim\n")
  }
} else {
  cat("    ✓ Present bioclim already exists\n")
}

# For future scenarios (average across years)
for(scenario in scenarios) {
  cat("\n  Future climate -", scenario, "(2022-2030):\n")
  future_bioclim_file <- here("vegetation_donana", paste0("bioclim_rasters_future_", scenario, ".tif"))

  if(!file.exists(future_bioclim_file)) {
    chelsa_output_future <- here("vegetation_donana", paste0("chelsa_rasters_future_", scenario))
    cat("    Loading future climate rasters...\n")

    # Load all future climate years for this scenario
    all_tas <- list()
    all_pr <- list()

    for(yr in future_years) {
      tas_file <- file.path(chelsa_output_future, paste0("tas_", yr, ".tif"))
      pr_file <- file.path(chelsa_output_future, paste0("pr_", yr, ".tif"))

      if(file.exists(tas_file) && file.exists(pr_file)) {
        all_tas[[as.character(yr)]] <- rast(tas_file)
        all_pr[[as.character(yr)]] <- rast(pr_file)
      }
    }

    if(length(all_tas) > 0) {
      cat("    Calculating bioclim variables for", length(all_tas), "years...\n")

      # Calculate bioclim for each year and average
      bioclim_list <- list()
      for(i in seq_along(all_tas)) {
        cat("      Processing year", names(all_tas)[i], "...\n")
        bioclim_list[[i]] <- calculate_bioclim_rasters(all_tas[[i]], all_pr[[i]])
      }

      # Average bioclim across years
      cat("    Averaging bioclim variables across years...\n")
      bioclim_future <- mean(rast(bioclim_list))
      names(bioclim_future) <- c("bio1", "bio12", "bio9", "bio18")

      # Save
      writeRaster(bioclim_future, future_bioclim_file, overwrite = TRUE)
      cat("    ✓ Saved:", future_bioclim_file, "\n")
    } else {
      cat("    ⚠ No future climate data found to calculate bioclim\n")
    }
  } else {
    cat("    ✓ Future bioclim already exists\n")
  }
}

# =============================================================================
# CREATE COORDINATE RASTERS
# =============================================================================

cat("\n[5/5] Creating coordinate rasters (lon, lat)...\n")

# Create longitude and latitude rasters matching the study extent
lon_raster <- init(example_raster, "x")
lat_raster <- init(example_raster, "y")

# Save coordinate rasters
writeRaster(lon_raster,
            here("vegetation_donana", "coordinate_lon.tif"),
            overwrite = TRUE)
writeRaster(lat_raster,
            here("vegetation_donana", "coordinate_lat.tif"),
            overwrite = TRUE)

cat("✓ Coordinate rasters saved\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CHELSA RASTER DOWNLOAD COMPLETE\n")
cat("================================================================================\n")
cat("\nSummary:\n")
cat("✓ Climate rasters downloaded using getChelsa() with extent parameter\n")
cat("✓ Bioclim variables calculated (bio1, bio12, bio9, bio18)\n")
cat("✓ Coordinate rasters created (lon, lat)\n\n")

cat("Files created:\n")
cat("  - coordinate_lon.tif\n")
cat("  - coordinate_lat.tif\n")
cat("  - bioclim_rasters_present.tif (4 layers)\n")
cat("  - bioclim_rasters_future_ssp245.tif (4 layers)\n")
cat("  - bioclim_rasters_future_ssp370.tif (4 layers)\n")
cat("  - bioclim_rasters_future_ssp585.tif (4 layers)\n\n")

cat("Intermediate files (can be large):\n")
cat("  - chelsa_rasters_present/ (daily tas/pr for 2005-2021)\n")
cat("  - chelsa_rasters_future_*/  (daily tas/pr for 2022-2030)\n\n")

cat("Next step: Run ndvi_predictions_rasters.R to generate NDVI predictions\n\n")
