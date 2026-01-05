# Doñana Shrub Vegetation Forecasting - Complete Workflow Documentation

**Project**: Forecasting vegetation dynamics in Doñana National Park
**Last Updated**: November 9, 2024
**Contributors**: Billur Bektaş, Patrícia Singh, Mary E Lofton, Freya Olsson, Maria Paniw, María Teresa Mejia Sánchez, Matthew Clements, Francisco Lloret

---

## Table of Contents
1. [Project Overview](#project-overview)
2. [Quick Start](#quick-start)
3. [Project Structure](#project-structure)
4. [Complete Workflow](#complete-workflow)
5. [Script Reference](#script-reference)
6. [Data Files Reference](#data-files-reference)
7. [Troubleshooting](#troubleshooting)

---

## Project Overview

This project analyzes long-term shrub monitoring data from Doñana National Park (Spain) and forecasts future shrub abundance using two different shrub abundance datasets, satellite-derived NDVI (Normalized Difference Vegetation Index) and climate projections.

### Research Questions
- What are the spatialtemporal patterns of shrub distribution across the landscape?
- How fast are shrubs invading new habitats?
- How will shrub abundance change under different climate scenarios?

### Key Features
- Processes 20 years of NDVI satellite data (2005-2024)
- Integrates 18-plot logterm individual shrub monitoring data (2007-2024)
- Integrates 117-plot landscape-level shrub abundance data (2007-2024)
- Models shrub abundance for 4 species using Bayesian approaches
- Generates temporal and spatiotemporal predictions under 3 climate scenarios
- Interactive Shiny dashboard for visualization

### Four Focal Species
1. **Halimium halimifolium** (Sun rose)
2. **Lavandula stoechas** (Spanish lavender)
3. **Rosmarinus officinalis** (Rosemary)
4. **Cistus libanotis** (Rock rose)

---

## Quick Start

### Prerequisites
- R version ≥ 4.0
- RStudio (recommended)
- ~10GB disk space for NDVI rasters
- Internet connection for climate data download

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/[your-repo]/forecasting-workshop-shinyapp.git
cd forecasting-workshop-shinyapp
```

2. **Open the R project**
```r
# Open workshops_EFFI.Rproj in RStudio
```

3. **Install dependencies**
```r
# All packages will be auto-installed when you run the workflow
source("load_packages2.R")
```

### Running the Complete Analysis

**Option 1: Use the Master Workflow (Recommended)**
```r
# Run the complete pipeline from scratch
source("vegetation_donana/workflow.R")
```

**Option 2: Launch the Shiny App Directly**
```r
# If data is already processed, just launch the dashboard
shiny::runApp("vegetation_donana")
```

**Option 3: Run Individual Steps**
See [Complete Workflow](#complete-workflow) section below.

---

## Project Structure

```
vegetation_donana/
│
├── app.R                          # Shiny dashboard application
├── workflow.R                     # Master workflow orchestrator
├── load_packages2.R              # Package management
│
├── R/                             # All R scripts
│   ├── functions/                 # Core reusable functions
│   │   ├── ndvi_calculation_functions.R
│   │   └── init_abund_pred_updated.R
│   │
│   ├── data_processing/          # Data preparation scripts
│   │   ├── chelsa_download.R
│   │   ├── chelsa_download_rasters.R
│   │   ├── ndvi_indices_calculations.R
│   │   └── ndvi_raster_indices_calculations.R
│   │
│   ├── modeling/                 # Prediction models
│   │   ├── ndvi_predictions.R
│   │   ├── ndvi_predictions_rasters.R
│   │   ├── run_predictions_shrubs_updated.R
│   │   ├── run_spatial_model_updated.R
│   │   └── run_spatial_predictions_updated.R
│   │
│   └── deprecated/               # Archived scripts
│
├── data/                         # All data files
│   ├── raw/                      # Original data (never modify!)
│   ├── processed/                # Intermediate processed data
│   ├── climate/                  # Climate data (CHELSA)
│   ├── spatial/                  # Shapefiles & spatial layers
│   ├── ndvi_rasters/             # NDVI satellite imagery
│   ├── validation/               # Citizen science data to validate spatiotemporal predictions (example only for now)
│   └── results/                  # Model outputs
│       ├── temporal_predictions/         # Time series forecasts as csv
│       ├── spatial_predictions/          # Spatial abundance maps
│       ├── spatial_predictions_csv/      # Spatial abundance maps saved as csv
│       └── ndvi_predictions/             # Future NDVI projections
│
├── Figures/                      # Plots and images
└── README_WORKFLOW.md            # This file
```

---

## Complete Workflow

The analysis follows 6 sequential steps. Each step can be run independently if prerequisites are met.

### Workflow Diagram

```
┌──────────────────────────────────────────────────────────────────┐
│                    STEP 1: NDVI Processing                        │
│  Raw NDVI Rasters → BISE Correction → Interpolation → Metrics   │
└────────────────────────┬─────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────────┐
│              STEP 2: Climate Data & NDVI Predictions              │
│  Download CHELSA → Calculate Bioclim → Predict Future NDVI      │
└────────────────────────┬─────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────────┐
│           STEP 3: Prepare Observed Shrub Data                     │
│  Load Monitoring Data → Aggregate by Species → Validation Set   │
└────────────────────────┬─────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────────┐
│        STEP 4: Temporal Shrub Predictions (180 models)            │
│  Bayesian Models → Parameter Sampling → Abundance Forecasts     │
└────────────────────────┬─────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────────┐
│         STEP 5: Spatiotemporal Shrub Predictions (11 metrics)     │
│  GAM Spatial Model → Raster Predictions → Landscape Maps       │
└────────────────────────┬─────────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────────┐
│                   STEP 6: Launch Shiny App                        │
│  Interactive Dashboard → Visualization → Export Results          │
└──────────────────────────────────────────────────────────────────┘
```

---

### STEP 1: NDVI Metrics Calculation

**Purpose**: Process raw NDVI satellite imagery into meaningful ecological metrics.

**Script**: `R/data_processing/ndvi_indices_calculations.R`

**Input Files**:
- `data/raw/df.ndvi.csv` - Raw NDVI values extracted from satellite imagery
- `data/raw/coords_plot_since2007.csv` - Plot coordinates

**Output Files**:
- `data/processed/ndvi_metrics_updated.csv` - NDVI metrics per plot per year
- `data/processed/ndvi_processed.rds` - Time series with BISE correction
- `data/processed/ndvi_trends_plots.pdf` - Visualization of trends

**Processing Steps**:
1. Load raw NDVI data
2. Apply BISE correction (removes atmospheric noise)
3. Interpolate missing values
4. Smooth time series
5. Calculate 11 metrics per plot per year:
   - `integrated_ndvi` - Total annual productivity
   - `annual_mean`, `annual_median` - Central tendencies
   - `summer_integrated`, `summer_max`, `summer_mean`, `summer_median`
   - `winter_spring_integrated`, `winter_spring_max`, `winter_spring_mean`, `winter_spring_median`

**How to Run**:
```r
source(here::here("vegetation_donana", "R", "data_processing", "ndvi_indices_calculations.R"))
```

**Expected Runtime**: ~5-10 minutes

---

### STEP 2: Climate Data & NDVI Predictions

**Purpose**: Download climate data and predict future NDVI under climate scenarios.

**Scripts**:
- `R/data_processing/chelsa_download.R` - Download climate data
- `R/modeling/ndvi_predictions.R` - Generate NDVI predictions

#### Step 2A: Download Climate Data

**Input Files**:
- `data/raw/coords_plot_since2007.csv` - Plot coordinates

**Output Files**:
- `data/climate/present_climate_plots.csv` - Historical climate (2005-2023)
- `data/climate/future_climate_all_plots.csv` - Future projections (2024-2100)
- `data/climate/chelsa_rasters_present/` - Climate rasters

**Climate Variables Downloaded**:
- **Temperature**: Daily mean, min, max (°C)
- **Precipitation**: Daily total (mm)
- **Scenarios**: SSP2-4.5, SSP3-7.0, SSP5-8.5

**How to Run**:
```r
source(here::here("vegetation_donana", "R", "data_processing", "chelsa_download.R"))
```

**Expected Runtime**: 1-3 hours (downloads ~50MB of data)

**Download Strategy**:
1. Try downloading full year → if successful, move to next year
2. If year fails → download month-by-month
3. If month fails → download day-by-day
4. Automatically resumes if interrupted

#### Step 2B: Generate NDVI Predictions

**Input Files**:
- `data/climate/present_climate_plots.csv`
- `data/climate/future_climate_all_plots.csv`
- `data/processed/ndvi_metrics_updated.csv`

**Output Files**:
- `data/results/ndvi_predictions/ndvi_predictions_plots.rds` - Future NDVI predictions
- `data/processed/ndvi_plot.rds` - Visualization-ready dataset

**Models Tested**:
- Linear Model (LM)
- Random Forest (RF)

**Bioclim Variables**:
- bio1: Annual Mean Temperature
- bio9: Mean Temperature of Driest Quarter
- bio12: Annual Precipitation
- bio18: Precipitation of Warmest Quarter
- Plus 11 combinations of these variables

**How to Run**:
```r
source(here::here("vegetation_donana", "R", "modeling", "ndvi_predictions.R"))
```

**Expected Runtime**: 15-30 minutes

---

### STEP 3: Prepare Observed Shrub Data

**Purpose**: Load and aggregate monitoring data for model validation.

**Handled by**: `workflow.R` (lines 53-98)

**Input Files**:
- `data/raw/shrub_abundance_since2007_18_plots.csv` - Shrub counts per plot

**Output Files**:
- `data/processed/observed_totals.rds` - Aggregated totals by species

**Processing**:
```r
# Aggregate by species across all 18 plots
observed_totals <- rbind(
  lavandula_totals,
  halimium_totals,
  rosmarinus_totals,
  cistus_totals
)
```

**Data Structure**:
```
year | species              | tot (total adults)
-----|----------------------|------------------
2024 | Lavandula stoechas  | 1234
2024 | Halimium halimifolium| 567
```

---

### STEP 4: Temporal Shrub Predictions

**Purpose**: Forecast shrub abundance for 2024-2027 under climate scenarios.

**Script**: `R/modeling/run_predictions_shrubs_updated.R`

**Dependencies**:
- Requires: `R/modeling/run_spatial_model_updated.R` (must be sourced first!)

**Input Files**:
- `data/processed/ndvi_metrics_updated.csv`
- `data/results/ndvi_predictions/ndvi_predictions_plots.rds`
- `data/raw/shrub_abundance_since2007_18_plots.csv`

**Output Files**:
- `data/results/temporal_predictions/model_predictions_{metric}_{scenario}_{bioclim}_{model}.rds`
- Total: 180 files (2 metrics × 3 scenarios × 15 bioclim × 2 models)

**Model Combinations**:
```
NDVI Metrics:
  - integrated_ndvi
  - winter_spring_integrated

Climate Scenarios:
  - ssp245 (moderate emissions)
  - ssp370 (high emissions)
  - ssp585 (very high emissions)

Bioclim Variables:
  - 4 single variables: bio1, bio9, bio12, bio18
  - 6 two-variable combos: bio1_bio12, bio1_bio9, etc.
  - 4 three-variable combos: bio1_bio12_bio9, etc.
  - 1 four-variable combo: bio1_bio12_bio9_bio18

Models:
  - lm (Linear Model)
  - rf (Random Forest)
```

**Function**: `run_model_combination()`

**Parameters**:
- `ndvi_metric` - Which NDVI metric to use as predictor
- `scenario` - Climate scenario (ssp245, ssp370, ssp585)
- `bioclim` - Which bioclim variables to use
- `model` - Model type (lm, rf)
- `year_before_pred` - Last observed year (default: 2024)
- `lag` - Time lag between NDVI and abundance (default: 0)
- `n.years.pred` - Years to forecast (default: 4)
- `n.years.obs` - Years with observations for validation (default: 2)

**Returns**:
```r
list(
  parameters = list(ndvi_metric, scenario, bioclim, model),
  predictions = list(
    halimium = data.frame(N, year, sim, mse, metric, scenario, bioclim, model, species),
    lavandula = ...,
    rosmarinus = ...,
    cistus = ...
  )
)
```

**Modeling Approach**:
1. Load NDVI observations and predictions
2. Scale NDVI metrics (mean = 0, sd = 1)
3. Run Bayesian spatial model with JAGS
4. Sample 1000 posterior parameter sets
5. Forecast abundance using:
   ```
   N[t+1] = exp(α₀ + α₁ × NDVI[t] + α₂ × log(N[t] + 0.001))
   ```
6. Calculate MSE for validation years
7. Generate prediction intervals

**How to Run**:
```r
# Load dependencies first!
source(here::here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
source(here::here("vegetation_donana", "R", "modeling", "run_predictions_shrubs_updated.R"))

# Run single model
result <- run_model_combination(
  ndvi_metric = "integrated_ndvi",
  scenario = "ssp370",
  bioclim = "bio1_bio12",
  model = "lm",
  year_before_pred = 2024,
  lag = 0,
  n.years.pred = 4,
  n.years.obs = 2
)
```

**Expected Runtime**:
- Single model: ~30 seconds
- All 180 combinations: ~1.5-2 hours

**Output Example**:
```
data/results/temporal_predictions/model_predictions_integrated_ndvi_ssp370_bio1_bio12_lm.rds

Contains:
  N     year  sim  mse      metric            scenario  bioclim    model  species
  234   2024  1    1234.5   integrated_ndvi   ssp370    bio1_bio12 lm     Halimium halimifolium
  245   2024  2    1456.2   integrated_ndvi   ssp370    bio1_bio12 lm     Halimium halimifolium
  ...
```

---

### STEP 5: Spatial Shrub Predictions

**Purpose**: Generate landscape-scale abundance maps.

**Scripts**:
- `R/modeling/run_spatial_model_updated.R` - Fit spatial Bayesian model of abundance change
- `R/functions/initial_abund_pred_updated.R` - Fit GAM models to predict initial abundance across the landscape
- `R/modeling/run_spatial_predictions_updated.R` - Generate raster predictions

**Dependencies**:
- MGCV package must be installed

#### Step 5A: Fit Spatial Model

**Function**: `run_spatial_model()`

**Parameters**:
- `ndvi_metric` - NDVI metric to use
- `year_before_pred` - Last observed year
- `lag` - Time lag

**Input Files**:
- `data/processed/ndvi_metrics_updated.csv`
- `data/raw/shrub_abundance_since2007_18_plots.csv`

**Returns**:
```r
list(
  mcmc = list(a0.h, a1.h, a2.h, ...),  # MCMC samples for parameters
  observed_fut = list(halimium, lavandula, rosmarinus, cistus)  # Initial conditions
)
```

**Model Structure** (NIMBLE):
```r
# Likelihood
for each plot i, year k:
  C[i,k] ~ Poisson(lambda[i,k])
  log(lambda[i,k]) = a0 + a1 × NDVI[i,k] + a2 × log(C[i,k-1] + 0.001)

# Priors
a0 ~ Normal(0, 0.01)
a1 ~ Normal(0, 0.01)
a2 ~ Normal(0, 0.01)
```

**MCMC Settings**:
- Burn-in: 5,000 iterations
- Samples: 10,000 iterations
- Chains: 3
- Thin: 5

#### Step 5B: Generate Spatial Predictions

**Function**: `run_spatial_predictions()`

**Parameters**:
- `ndvi_metric` - Which NDVI metric
- `year_before_pred` - Prediction year
- `lag` - Time lag

**Input Files**:
- `data/ndvi_rasters/ndvi_metrics_rasters/2024/{metric}_2024.tif`
- `data/spatial/maps/siose_DN.shp` - Habitat classification
- Model parameters from `run_spatial_model()`
- Model parameters from `initial_abund_pred_updated()`

**Output Files**:
- `data/results/spatial_predictions/halimium_{metric}.tif`
- `data/results/spatial_predictions/lavandula_{metric}.tif`
- `data/results/spatial_predictions/rosmarinus_{metric}.tif`
- `data/results/spatial_predictions/cistus_{metric}.tif`

**Processing Steps**:
1. Load NDVI raster for prediction year
2. Load habitat map and filter suitable habitats
3. Scale NDVI values using plot means/sds
4. Sample 100 parameter sets from MCMC
5. For each pixel:
   ```r
   N_predicted = exp(a0 + a1 × NDVI_scaled)
   ```
6. Calculate median prediction across samples
7. Mask unsuitable habitats
8. Save as GeoTIFF

**Suitable Habitats**:
- Code 330: Scrubland
- Code 340: Herbaceous vegetation

**How to Run**:
```r
source(here::here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
source(here::here("vegetation_donana", "R", "modeling", "run_spatial_predictions_updated.R"))

# Generate predictions for one metric
results <- run_spatial_predictions(
  ndvi_metric = "integrated_ndvi",
  year_before_pred = 2024,
  lag = 0
)
```

**Expected Runtime**:
- Single metric: ~2-5 minutes
- All 11 metrics: ~30-60 minutes

---

### STEP 6: Launch Shiny App

**Purpose**: Interactive visualization and exploration of results.

**Script**: `app.R`

**Input Files** (all loaded at startup):
- `data/processed/ndvi_plot.rds` - NDVI time series
- `data/processed/ndvi_processed.rds` - Raw NDVI data
- `data/raw/shrub_number.csv` - Monitoring data
- `data/processed/observed_totals.rds` - Validation data
- `data/results/temporal_predictions/*.rds` - All model predictions
- `data/results/spatial_predictions/*.tif` - Spatial maps

**Dashboard Features**:

1. **NDVI Data Tab**
   - Plot raw NDVI time series
   - Interactive plotly charts
   - Filter by plot, date range

2. **Shrub Data Tab**
   - Observed abundance over time
   - Filter by species
   - Shows adults, saplings, seedlings

3. **NDVI Predictions Tab**
   - Compare future NDVI scenarios
   - Select metric, scenario, bioclim, model
   - Predicted vs observed

4. **Shrub Predictions (Temporal) Tab**
   - Forecast abundance 2024-2027
   - Uncertainty bands (1000 simulations)
   - MSE by year for validation
   - Compare scenarios side-by-side

5. **Shrub Predictions (Spatial) Tab**
   - Landscape abundance maps
   - Select species and NDVI metric
   - Color gradients for density

**How to Launch**:
```r
# From R console
shiny::runApp("vegetation_donana")

# Or from command line
R -e "shiny::runApp('vegetation_donana')"
```

**Access**:
- Local: http://127.0.0.1:XXXX (port shown in console)
- Server: Can be deployed to shinyapps.io or RStudio Connect

---

## Script Reference

### Core Functions

#### R/functions/ndvi_calculation_functions.R

**Purpose**: Reusable functions for NDVI processing.

| Function | Input | Output | Description |
|----------|-------|--------|-------------|
| `process_ndvi_data()` | `df` (dataframe with plot, date, ndvi) | List of processed dataframes | BISE correction, interpolation, smoothing |
| `calculate_ndvi_metrics()` | `processed_data` (list) | Dataframe of metrics | Calculates 11 seasonal/annual metrics |
| `calculate_trends()` | `metrics` (dataframe) | Dataframe with slopes, p-values | Linear trend analysis |
| `get_slope_significance()` | `data` (time series) | List(slope, p-value) | Mann-Kendall trend test |

**Key Algorithms**:

1. **BISE Correction** - Removes low-frequency atmospheric noise:
```r
# Best Index Slope Extraction
for each year:
  fit linear model to NDVI vs day-of-year
  if slope > threshold:
    accept trend, remove low values
  else:
    reject, keep original
```

2. **Interpolation** - Fills gaps:
```r
# Piecewise Cubic Hermite Interpolating Polynomial
ndvi_interpolated <- pchip(dates, ndvi_values, all_dates)
```

3. **Smoothing** - Reduces noise:
```r
# LOESS smoothing with span = 0.1
ndvi_smoothed <- loess(ndvi ~ day, span = 0.1)
```

#### R/functions/init_abund_pred_updated.R

**Purpose**: Initialize abundance predictions using GAM models.

| Function | Input | Output | Description |
|----------|-------|--------|-------------|
| `init_abund_pred()` | `ndvi_metric`, `year_before_pred` | Initial abundance estimates | Fits GAM to predict starting values |

**Model**:
```r
# Generalized Additive Model
abundance ~ s(ndvi, k = 5) + s(year, k = 5) + s(plot, bs = "re")
```

---

### Data Processing Scripts

#### R/data_processing/chelsa_download.R

**Purpose**: Download CHELSA climate data from web API.

**No Functions** (procedural script)

**Downloads**:
1. **Present Climate** (2005-2023):
   - Daily temperature (mean, min, max)
   - Daily precipitation
   - For each plot coordinate

2. **Future Climate** (2024-2100):
   - Monthly bioclim variables
   - Three scenarios: SSP2-4.5, SSP3-7.0, SSP5-8.5
   - For each plot coordinate

**API Endpoints**:
- Temperature: `https://chelsa-climate.org/wp-admin/download-page/CHELSA_V2/GLOBAL/daily/temp/`
- Precipitation: `https://chelsa-climate.org/wp-admin/download-page/CHELSA_V2/GLOBAL/daily/pr/`
- Future: `https://chelsa-climate.org/wp-admin/download-page/CHELSA_cmip6/`

**Smart Download Logic**:
```
For each variable (temp, precip):
  For each coordinate:
    Check which dates have NA values
    Try to download full year
      If successful → next year
      If failed → try month-by-month
        If month fails → try day-by-day
    Save after each successful download
```

#### R/data_processing/ndvi_indices_calculations.R

**Purpose**: Calculate NDVI metrics for monitoring plots.

**Calls Functions From**: `R/functions/ndvi_calculation_functions.R`

**Workflow**:
```
1. Load df.ndvi.csv (raw NDVI values)
2. For each plot:
     process_ndvi_data() → BISE + interpolation + smoothing
3. Combine all plots
4. calculate_ndvi_metrics() → 11 metrics per plot per year
5. calculate_trends() → significance of trends
6. Save ndvi_metrics_updated.csv
7. Generate diagnostic plots
```

#### R/data_processing/ndvi_raster_indices_calculations.R

**Purpose**: Calculate NDVI metrics for entire landscape (raster-based).

**Parallelized Processing** using `foreach` and `doParallel`.

**Workflow**:
```
1. Load all NDVI rasters for one year
2. For each pixel in parallel:
     Extract NDVI time series
     Apply BISE correction
     Interpolate and smooth
     Calculate 11 metrics
3. Combine into raster stack
4. Save as separate GeoTIFFs
```

**Output Structure**:
```
data/ndvi_rasters/ndvi_metrics_rasters/
  2024/
    integrated_ndvi_2024.tif
    annual_mean_2024.tif
    annual_median_2024.tif
    summer_integrated_2024.tif
    summer_max_2024.tif
    summer_mean_2024.tif
    summer_median_2024.tif
    winter_spring_integrated_2024.tif
    winter_spring_max_2024.tif
    winter_spring_mean_2024.tif
    winter_spring_median_2024.tif
```

---

### Modeling Scripts

#### R/modeling/ndvi_predictions.R

**Purpose**: Predict future NDVI using climate projections.

**Key Steps**:

1. **Calculate Present Bioclim**:
```r
bio1 = mean(annual_temp)
bio9 = mean(temp_driest_quarter)
bio12 = sum(annual_precip)
bio18 = sum(precip_warmest_quarter)
```

2. **Load Future Climate**:
```r
future_bioclim <- get_future_clim(coords, scenario)
```

3. **Fit Models**:
```r
for each NDVI metric:
  for each bioclim combination:
    for each model type (lm, rf):
      model <- train(ndvi ~ bio1 + bio12 + ...)
      predictions <- predict(model, future_bioclim)
      save predictions
```

4. **Combine for Visualization**:
```r
ndvi_plot <- rbind(
  observed_ndvi,
  predicted_ndvi_ssp245,
  predicted_ndvi_ssp370,
  predicted_ndvi_ssp585
)
```

**Model Formulas**:

Linear Model:
```r
lm(ndvi ~ bio1 + bio9 + bio12 + bio18, data = training)
```

Random Forest:
```r
randomForest(ndvi ~ bio1 + bio9 + bio12 + bio18, ntree = 500, mtry = 2)
```

#### R/modeling/run_spatial_model_updated.R

**Purpose**: Fit Bayesian spatial model for shrub abundance.

**Function**: `run_spatial_model(ndvi_metric, year_before_pred, lag)`

**JAGS Model Code**:
```r
model {
  # Halimium
  for(i in 1:n.plots) {
    for(k in 2:n.years) {
      C.hal[i,k] ~ dpois(lambda.hal[i,k])
      log(lambda.hal[i,k]) <- a0.h + a1.h * cov.hal[i,k] + a2.h * log(C.hal[i,k-1] + 0.001)
    }
  }

  # Lavandula (similar structure)
  # Rosmarinus (similar structure)
  # Cistus (similar structure)

  # Priors
  a0.h ~ dnorm(0, 0.01)
  a1.h ~ dnorm(0, 0.01)
  a2.h ~ dnorm(0, 0.01)
  # ... same for other species
}
```

**Parameters Monitored**:
- `a0.h`, `a0.l`, `a0.r`, `a0.c` - Intercepts
- `a1.h`, `a1.l`, `a1.r`, `a1.c` - NDVI effects
- `a2.h`, `a2.l`, `a2.r`, `a2.c` - Autoregressive effects

**Returns**:
```r
list(
  mcmc = list(
    a0.h = numeric vector [10000],
    a1.h = numeric vector [10000],
    ...
  ),
  observed_fut = list(
    halimium = numeric vector [18],  # One value per plot
    lavandula = numeric vector [18],
    ...
  )
)
```

#### R/modeling/run_predictions_shrubs_updated.R

**Purpose**: Generate temporal abundance forecasts.

**Function**: `run_model_combination(ndvi_metric, scenario, bioclim, model, year_before_pred, lag, n.years.pred, n.years.obs)`

**Workflow**:
```
1. Load observed NDVI metrics (past)
2. Load predicted NDVI metrics (future)
3. Scale NDVI (mean=0, sd=1)
4. Call run_spatial_model() → get MCMC samples
5. Load initial abundances for 2024
6. For each of 1000 parameter sets:
     For each plot:
       For each prediction year:
         N[t+1] = exp(a0 + a1*NDVI[t] + a2*log(N[t]))
         N[t+1] ~ Poisson(N[t+1])  # Add stochasticity
   Sum across plots → landscape total
   Calculate MSE for validation years
7. Return predictions for all species
```

**Uncertainty Sources**:
1. **Parameter uncertainty** - 1000 MCMC samples
2. **Process uncertainty** - Poisson draws
3. **NDVI uncertainty** - Multiple climate scenarios

**Output Data Structure**:
```r
data.frame(
  N = predicted abundance,
  year = 2024, 2025, 2026, 2027,
  sim = simulation number (1-1000),
  mse = mean squared error (for validation years),
  metric = NDVI metric used,
  scenario = climate scenario,
  bioclim = bioclim variables used,
  model = model type,
  species = species name
)
```

#### R/modeling/run_spatial_predictions_updated.R

**Purpose**: Generate landscape-scale abundance maps.

**Function**: `run_spatial_predictions(ndvi_metric, year_before_pred, lag)`

**Workflow**:
```
1. Load NDVI raster for prediction year
2. Load habitat shapefile (siose_DN.shp)
3. Mask to suitable habitats (codes 330, 340)
4. Call run_spatial_model() → get parameters
5. Extract NDVI values for all pixels
6. Scale NDVI using plot statistics
7. For each pixel:
     Sample 1000 predictions:
       N_predicted = exp(a0 + a1 * NDVI_scaled)
     Calculate median across samples
8. Create raster with predictions
9. Mask unsuitable habitats (set to NA)
10. Save as GeoTIFF
```

**Raster Properties**:
- Resolution: 10m × 10m
- CRS: EPSG:25830 (ETRS89 / UTM zone 30N)
- Extent: Doñana National Park boundary
- NoData value: -9999

---

## Data Files Reference

### Raw Data (`data/raw/`)

| File | Size | Description | Source |
|------|------|-------------|--------|
| `shrub_abundance_since2007_18_plots.csv` | 81KB | Shrub counts by species, plot, year | Field monitoring |
| `shrub_number.csv` | 65KB | Detailed counts (adults, saplings, seedlings) | Field monitoring |
| `coords_plot_since2007.csv` | 2KB | Plot coordinates (lat/lon) | GPS measurements |
| `coordinate_lat.tif`, `coordinate_lon.tif` | 18MB | Landscape coordinate rasters | Generated |
| `df.ndvi.csv` | Unknown | Raw NDVI values per plot per date | Satellite imagery |

**Shrub Data Structure**:
```csv
plot,year,species,adults,saplings,seedlings
1,2007,Halimium halimifolium,45,12,8
1,2007,Lavandula stoechas,23,5,2
...
```

### Processed Data (`data/processed/`)

| File | Size | Description | Updated |
|------|------|-------------|---------|
| `ndvi_metrics_updated.csv` | 90KB | 11 NDVI metrics × 18 plots × 20 years | Oct 6 |
| `ndvi_processed.rds` | 2.5MB | BISE-corrected NDVI time series | Nov 5 |
| `ndvi_plot.rds` | 2.6MB | Observed + predicted NDVI for plots | Oct 20 |
| `observed_totals.rds` | 262B | Aggregated abundance for validation | Nov 9 |
| `present_climate_land.csv` | 23MB | Historical climate, landscape scale | - |
| `present_climate_plots.csv` | 6.1MB | Historical climate, 18 plots | - |

**NDVI Metrics CSV Structure**:
```csv
ID,year,integrated_ndvi,annual_mean,annual_median,summer_integrated,summer_max,summer_mean,summer_median,winter_spring_integrated,winter_spring_max,winter_spring_mean,winter_spring_median
1,2005,45.2,0.42,0.45,23.1,0.65,0.58,0.60,22.1,0.52,0.38,0.40
1,2006,48.7,0.46,0.48,25.3,0.68,0.61,0.63,23.4,0.54,0.40,0.42
...
```

### Climate Data (`data/climate/`)

| File | Size | Description |
|------|------|-------------|
| `future_climate_all.csv` | 12MB | All scenarios combined |
| `future_climate_all_plots.csv` | - | Plot-level future climate |
| `future_climate_all_land.csv` | - | Landscape-level future climate |
| `chelsa_rasters_present/` | 34MB | Historical climate rasters |

**Future Climate Structure**:
```csv
lon,lat,scenario,year,bio1,bio9,bio12,bio18
-6.45,37.05,ssp245,2024,16.5,12.3,567,89
-6.45,37.05,ssp245,2025,16.7,12.5,542,82
-6.45,37.05,ssp370,2024,16.8,12.6,534,78
...
```

### Results (`data/results/`)

#### Temporal Predictions (`data/results/temporal_predictions/`)

180 files following pattern:
```
model_predictions_{ndvi_metric}_{scenario}_{bioclim}_{model}.rds
```

Examples:
- `model_predictions_integrated_ndvi_ssp370_bio1_lm.rds`
- `model_predictions_winter_spring_integrated_ssp245_bio1_bio12_bio9_bio18_rf.rds`

Each file contains predictions for all 4 species.

#### Spatial Predictions (`data/results/spatial_predictions/`)

43 GeoTIFF files following pattern:
```
{species}_{ndvi_metric}.tif
```

Examples:
- `halimium_integrated_ndvi.tif`
- `lavandula_summer_max.tif`
- `rosmarinus_winter_spring_integrated.tif`

#### NDVI Predictions (`data/results/ndvi_predictions/`)

| File | Size | Description |
|------|------|-------------|
| `ndvi_predictions_plots.rds` | 363KB | Future NDVI for plots (all scenarios) |
| `ndvi_predictions_land.rds` | - | Future NDVI for landscape |
| `integrated_ndvi_predictions.rds` | - | Integrated NDVI only |

---

## Troubleshooting

### Common Issues

#### 1. Package Installation Errors

**Problem**: `Error: package 'X' is not available`

**Solutions**:
```r
# Install from CRAN
install.packages("X")

# Install from Bioconductor
BiocManager::install("X")

# Install from GitHub
devtools::install_github("user/package")
```

**Special Cases**:
- **JAGS**: Must install system dependency first
  - Mac: `brew install jags`
  - Linux: `sudo apt-get install jags`
  - Windows: Download from https://sourceforge.net/projects/mcmc-jags/

- **rJAVA**: Requires Java JDK
  ```bash
  # Mac
  brew install openjdk
  R CMD javareconf
  ```

#### 2. File Path Errors

**Problem**: `Error: cannot open file 'X': No such file or directory`

**Check**:
```r
# Verify here() points to project root
library(here)
here()  # Should show: /path/to/forecasting-workshop-shinyapp

# Check file exists
file.exists(here("vegetation_donana", "data", "raw", "shrub_abundance_since2007_18_plots.csv"))
```

**Common Causes**:
- Working directory not set to project root
- Files not moved to new structure
- Typo in filename

#### 3. Memory Errors

**Problem**: `Error: cannot allocate vector of size X Gb`

**Solutions**:
```r
# Increase memory limit (Windows)
memory.limit(size = 16000)  # 16GB

# Use less cores for parallel processing
options(mc.cores = 2)  # Instead of detectCores()

# Process in batches
# Instead of loading all rasters at once, process year-by-year
```

#### 4. JAGS Model Errors

**Problem**: `Error in jags.model: Cannot find model file`

**Check**:
```r
# Verify JAGS is installed
library(rjags)
testjags()

# Check model syntax
# Look for unmatched braces, missing commas
```

**Problem**: `Error: Compilation error on line X`

**Common Causes**:
- Missing dimension in array
- Using `=` instead of `<-` in model code
- Priors with zero precision

#### 5. Shiny App Won't Launch

**Problem**: `Error in shiny::runApp: Application directory does not exist`

**Solutions**:
```r
# Check current directory
getwd()  # Should be project root or vegetation_donana/

# Full path
shiny::runApp(here::here("vegetation_donana"))

# Or set working directory first
setwd(here::here("vegetation_donana"))
shiny::runApp()
```

**Problem**: App launches but shows errors loading data

**Check**:
```r
# Verify all input files exist
required_files <- c(
  "data/processed/ndvi_plot.rds",
  "data/processed/ndvi_processed.rds",
  "data/raw/shrub_number.csv",
  "data/processed/observed_totals.rds"
)

sapply(required_files, function(f) {
  exists <- file.exists(here::here("vegetation_donana", f))
  cat(f, ":", ifelse(exists, "OK", "MISSING"), "\n")
  exists
})
```

#### 6. Workflow Script Errors

**Problem**: `Error: could not find function "run_spatial_model"`

**Solution**: Scripts must be sourced in correct order
```r
# CORRECT order:
source(here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
source(here("vegetation_donana", "R", "modeling", "run_predictions_shrubs_updated.R"))

# Then run:
result <- run_model_combination(...)
```

**Problem**: `Error: object 'observed_totals' not found`

**Solution**: Run Step 3 first to generate observed_totals.rds

#### 7. Climate Download Failures

**Problem**: `Error: cannot connect to CHELSA server`

**Solutions**:
- Check internet connection
- Server may be temporarily down - try again later
- Use VPN if institutional firewall blocks access

**Problem**: Download is extremely slow

**Solution**: This is normal - downloading daily climate data for 20 years × 18 plots takes 1-3 hours. The script saves progress, so you can interrupt and resume.

#### 8. Raster Processing Errors

**Problem**: `Error: extents do not match`

**Solution**: Reproject rasters to same CRS
```r
library(terra)
rast1 <- rast("file1.tif")
rast2 <- rast("file2.tif")

# Check CRS
crs(rast1)
crs(rast2)

# Reproject if needed
rast2_reproj <- project(rast2, rast1)
```

#### 9. Parallel Processing Crashes

**Problem**: R session crashes during parallel processing

**Solutions**:
```r
# Reduce number of cores
library(doParallel)
cl <- makeCluster(2)  # Instead of detectCores()
registerDoParallel(cl)

# Add error handling
foreach(i = 1:n, .errorhandling = "pass") %dopar% {
  tryCatch({
    # your code
  }, error = function(e) {
    message("Error in iteration ", i, ": ", e$message)
    NULL
  })
}

# Always stop cluster when done
stopCluster(cl)
```

---

## Performance Tips

### Speed Up Analysis

1. **Skip completed steps**
   - The workflow checks for existing output files
   - Delete specific outputs to rerun only those steps

2. **Use fewer model combinations**
   ```r
   # Test with subset first
   ndvi_metrics <- c("integrated_ndvi")  # Just one
   scenarios <- c("ssp370")  # Just one
   bioclims <- c("bio1", "bio12")  # Just two
   # = 4 models instead of 180
   ```

3. **Reduce MCMC iterations** (for testing only!)
   ```r
   # In run_spatial_model_updated.R, change:
   n.iter <- 1000  # Instead of 10000
   n.burnin <- 500  # Instead of 5000
   ```

4. **Process fewer years of NDVI**
   ```r
   # In ndvi_indices_calculations.R
   df_ndvi <- df_ndvi[df_ndvi$year >= 2015, ]  # Last 10 years only
   ```

### Reduce Memory Usage

1. **Remove large objects after use**
   ```r
   result <- big_function()
   save(result, file = "output.rds")
   rm(result)
   gc()  # Garbage collection
   ```

2. **Process rasters in chunks**
   ```r
   # Instead of loading entire raster
   r <- rast("huge_file.tif")

   # Process by tiles
   tiles <- makeTiles(r, ncols = 4, nrows = 4)
   for(tile in tiles) {
     chunk <- crop(r, tile)
     # process chunk
   }
   ```

---

## Citation

If you use this workflow in your research, please cite:

```
Bektaş, B., Singh, P., Lofton, M.E., Olsson, F., Paniw, M., Mejia Sánchez, M.T.,
Clements, M., & Lloret, F. (2024). Forecasting vegetation dynamics in Doñana
National Park using NDVI and climate projections.
GitHub repository: https://github.com/[your-repo]
```

**NDVI Data Source**:
```
Díaz-Delgado, R., Afan, I., Aragones, D., Garcia, D., & Bustamante, J. (2019).
NDVIs Doñana 1984/2019 (v1.0) [Data set]. Zenodo.
https://doi.org/10.5281/zenodo.3518879
```

**Climate Data Source**:
```
Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W.,
Zimmermann, N.E., Linder, P., Kessler, M. (2017). Climatologies at high
resolution for the Earth's land surface areas. Scientific Data. 4 170122.
https://doi.org/10.1038/sdata.2017.122
```

---

## Contact & Support

**Issues**: Please report bugs or request features via GitHub Issues

**Questions**: Contact project maintainers
- Billur Bektaş: [email]
- Maria Paniw: [email]

**Workshop Materials**: Available at [workshop website]

---

## License

[Specify license - e.g., MIT, GPL-3, Creative Commons]

---

## Changelog

### Version 2.0 (November 9, 2024)
- ✅ Reorganized folder structure (R/, data/, results/)
- ✅ Updated workflow.R with better error handling
- ✅ Fixed path dependencies across all scripts
- ✅ Added progress tracking to model runs
- ✅ Updated documentation

### Version 1.0 (November 5, 2024)
- Initial release
- Basic workflow functional
- Shiny app operational

---

**Last Updated**: November 9, 2024
**Document Version**: 2.0
