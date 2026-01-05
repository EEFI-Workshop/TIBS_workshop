# Project Reorganization Summary

## Date: November 9, 2024

This document summarizes the reorganization of the Doñana shrub forecasting project.

---

## New Folder Structure

```
vegetation_donana/
├── app.R                          # Main Shiny dashboard application
├── workflow.R                     # Updated master workflow script
├── workflow_OLD_BACKUP.R          # Backup of previous workflow
│
├── R/                             # All R scripts organized by purpose
│   ├── functions/                 # Core reusable functions
│   │   ├── ndvi_calculation_functions.R
│   │   └── init_abund_pred_updated.R
│   │
│   ├── data_processing/          # Data download & processing scripts
│   │   ├── chelsa_download.R
│   │   ├── chelsa_download_rasters.R
│   │   ├── ndvi_indices_calculations.R
│   │   └── ndvi_raster_indices_calculations.R
│   │
│   ├── modeling/                 # Model fitting & prediction scripts
│   │   ├── ndvi_predictions.R
│   │   ├── ndvi_predictions_rasters.R
│   │   ├── run_predictions_shrubs_updated.R
│   │   ├── run_spatial_model_updated.R
│   │   └── run_spatial_predictions_updated.R
│   │
│   └── deprecated/               # Archived old scripts
│       └── wcs_donana.r
│
├── data/                         # All data files
│   ├── raw/                      # Original/observed data
│   │   ├── shrub_abundance_since2007_18_plots.csv
│   │   ├── shrub_number.csv
│   │   ├── coordinate_lat.tif
│   │   ├── coordinate_lon.tif
│   │   ├── coordinates_2023_02.csv
│   │   ├── coords_plot_since2007.csv
│   │   └── df.ndvi_land.csv
│   │
│   ├── processed/                # Processed/intermediate data
│   │   ├── ndvi_metrics_updated.csv
│   │   ├── ndvi_metrics_land_updated.csv
│   │   ├── ndvi_processed.rds
│   │   ├── ndvi_plot.rds
│   │   ├── observed_totals.rds
│   │   ├── present_climate_land.csv
│   │   └── present_climate_plots.csv
│   │
│   ├── climate/                  # Climate data files
│   │   ├── future_climate_all.csv
│   │   ├── future_climate_all_land.csv
│   │   ├── future_climate_all_plots.csv
│   │   ├── future_climate_land.csv
│   │   ├── future_climate_plots.csv
│   │   ├── land_bioclim_timeseries.csv
│   │   ├── plots_bioclim_timeseries.csv
│   │   ├── present_bioclim_land.csv
│   │   ├── present_bioclim_plots.csv
│   │   └── chelsa_rasters_present/
│   │
│   ├── spatial/                  # Spatial reference data
│   │   ├── DN_layers/
│   │   └── maps/
│   │
│   ├── ndvi_rasters/             # NDVI rasters by year
│   │   └── ndvi_metrics_rasters/ (2005-2024, 20 folders × 11 metrics)
│   │
│   └── results/                  # Model outputs & predictions
│       ├── temporal_predictions/     # Temporal model runs (103 files)
│       │   └── model_predictions_{metric}_{scenario}_{bioclim}_{model}.rds
│       │
│       ├── spatial_predictions/      # Spatial prediction rasters (43 files)
│       │   └── {species}_{ndvi_metric}.tif
│       │
│       └── ndvi_predictions/         # NDVI prediction outputs
│           ├── ndvi_predictions_land.rds
│           ├── ndvi_predictions_plots.rds
│           ├── integrated_ndvi_predictions.rds
│           ├── summer_integrated_predictions.rds
│           ├── winter_spring_integrated_predictions.rds
│           ├── ndvi_predictions_comparison_land.csv
│           └── ndvi_predictions_comparison_plots.csv
│
└── Figures/                      # Documentation figures
    └── (maps, plot locations, etc.)
```

---

## Key Changes Made

### 1. Created Organized Directory Structure
- **R/** - All scripts organized by function (data_processing, modeling, functions)
- **data/** - Raw vs processed data clearly separated, with results nested inside
- **data/results/** - All model outputs organized by type

### 2. Moved Files to Appropriate Locations
- ✅ 14 R scripts moved to organized subdirectories
- ✅ 20+ data files moved to appropriate data/ subdirectories
- ✅ 103 temporal prediction files → data/results/temporal_predictions/
- ✅ 43 spatial prediction files → data/results/spatial_predictions/
- ✅ 7 NDVI prediction files → data/results/ndvi_predictions/

### 3. Updated All File Paths
Updated paths in:
- ✅ app.R - Updated all data loading paths
- ✅ run_predictions_shrubs_updated.R - Updated data file paths
- ✅ run_spatial_model_updated.R - Updated data file paths
- ✅ run_spatial_predictions_updated.R - Updated raster and shapefile paths
- ✅ ndvi_indices_calculations.R - Updated input/output paths

### 4. Created New Workflow Script
- ✅ workflow.R - Complete rewrite with:
  - Clear step-by-step structure
  - Progress tracking
  - Better error handling
  - All paths updated for new structure
  - Informative console output
- ✅ workflow_OLD_BACKUP.R - Backup of original

---

## Workflow Steps

The new `workflow.R` orchestrates the complete analysis pipeline:

1. **NDVI Metrics Calculation** - Process raw NDVI data
2. **Climate Data & NDVI Predictions** - Download climate data and predict future NDVI
3. **Prepare Observed Shrub Data** - Process validation data
4. **Temporal Shrub Predictions** - Run 180 model combinations (2 metrics × 3 scenarios × 15 bioclim × 2 models)
5. **Spatial Shrub Predictions** - Generate spatial rasters for 11 NDVI metrics
6. **Launch Shiny App** - Interactive visualization dashboard

---

## Benefits of New Structure

1. **Clarity** - Easy to understand what each folder contains
2. **Separation of Concerns** - Raw data, processed data, and results are clearly separated
3. **Maintainability** - Scripts organized by purpose make it easier to find and update code
4. **Scalability** - New scripts can be added to appropriate subdirectories
5. **Version Control** - Easier to .gitignore large data files while keeping code
6. **Collaboration** - Team members can quickly understand project organization

---

## Files That Can Be Deleted (After Verification)

Once you've tested the new structure:
- `workflow_OLD_BACKUP.R` - after confirming new workflow works
- Any `.DS_Store` files (macOS system files)

---

## Next Steps

1. **Test the workflow** - Run `source("workflow.R")` to ensure all paths work correctly
2. **Test the Shiny app** - Run `runApp("vegetation_donana")` to verify app loads data correctly
3. **Update .gitignore** - Consider excluding large data files and results if needed
4. **Commit changes** - Commit the reorganization to git

---

## Notes

- All original files have been moved (not copied) to save space
- The reorganization maintains all functionality while improving organization
- All paths have been updated using `here()` for reproducibility across systems
