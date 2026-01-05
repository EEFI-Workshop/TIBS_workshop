# ============================================================================
# MASTER WORKFLOW: Doñana Shrub Forecasting Analysis
# ============================================================================
# This script orchestrates the complete analysis pipeline:
# 1. NDVI data processing
# 2. Climate data acquisition
# 3. NDVI future predictions
# 4. Shrub abundance temporal predictions
# 5. Shrub abundance spatial predictions
# 6. Shiny app launch
# ============================================================================

# Setup ----
library(here)

# Load all packages for the analysis
source(here("load_packages2.R"))

# Set working directory to vegetation_donana
setwd(here("vegetation_donana"))

cat("=== Doñana Shrub Forecasting Workflow ===\n")
cat("Starting analysis pipeline...\n\n")

# ============================================================================
# STEP 1: NDVI Metrics Calculation ----
# ============================================================================
cat("STEP 1: Processing NDVI metrics...\n")

ndvi_metrics_file <- here("vegetation_donana", "data", "processed", "ndvi_metrics_updated.csv")

if(!file.exists(ndvi_metrics_file)){
  cat("  - NDVI metrics not found. Running calculations...\n")
  source(here("vegetation_donana", "R", "data_processing", "ndvi_indices_calculations.R"))
  cat("  ✓ NDVI metrics calculated successfully\n")
} else {
  cat("  ✓ NDVI metrics already exist\n")
}

# ============================================================================
# STEP 2: Climate Data & NDVI Predictions ----
# ============================================================================
cat("\nSTEP 2: Generating NDVI predictions...\n")

ndvi_pred_file <- here("vegetation_donana", "data", "results", "ndvi_predictions", "ndvi_predictions_plots.rds")

if(!file.exists(ndvi_pred_file)){
  cat("  - NDVI predictions not found. Running climate download and predictions...\n")
  source(here("vegetation_donana", "R", "modeling", "ndvi_predictions.R"))
  cat("  ✓ NDVI predictions completed\n")
} else {
  cat("  ✓ NDVI predictions already exist\n")
}

# ============================================================================
# STEP 3: Prepare Observed Shrub Data ----
# ============================================================================
cat("\nSTEP 3: Preparing observed shrub abundance data...\n")

observed_total_file <- here("vegetation_donana", "data", "processed", "observed_total.csv")
observed_plot_file <- here("vegetation_donana", "data", "processed", "observed_plot.csv")
year_before_pred <- 2024

if(!file.exists(observed_total_file)){
  cat("  - Processing observed data for validation...\n")
  
  observed_total <- read.csv(here("vegetation_donana", "data", "raw", "shrub_abundance_since2007_18_plots.csv"))%>%
    filter(year>= year_before_pred)%>%
    filter(species %in% c("Halimium halimifolium",
                          "Lavandula stoechas",
                          "Rosmarinus officinalis",
                          "Cistus libanotis"))%>%
    select(plot, year, species, adults)%>%
    group_by(year, species)%>%
    summarize(tot = sum(adults))%>%
    mutate(year = as.numeric(year))

  # Save for validation
  write.csv(observed_total, file = observed_total_file)
  cat("  ✓ Observed totals saved\n")
} else {
  observed_total <- read.csv(observed_total_file)
  cat("  ✓ Observed totals already exist\n")
}
if(!file.exists(observed_plot_file)){
  cat("  - Processing observed data for validation...\n")
  # Load shrub abundance data
  observed_plot <- read.csv(here("vegetation_donana", "data", "raw", "shrub_abundance_since2007_18_plots.csv"))%>%
    filter(year>= year_before_pred)%>%
    filter(species %in% c("Halimium halimifolium",
                          "Lavandula stoechas",
                          "Rosmarinus officinalis",
                          "Cistus libanotis"))%>%
    select(plot, year, species, adults)%>%
    group_by(plot, year, species)%>%
    summarize(obs = mean(adults))%>%
    mutate(year = as.numeric(year))
 
  # Save for validation
  write.csv(observed_plot, file = observed_plot_file)
  cat("  ✓ Observed totals saved\n")
} else {
  observed_plot <- read.csv(observed_plot_file)
  cat("  ✓ Observed totals already exist\n")
}

# ============================================================================
# STEP 4: Temporal Shrub Predictions ----
# ============================================================================
cat("\nSTEP 4: Running temporal shrub abundance predictions...\n")

# Load prediction functions
source(here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
source(here("vegetation_donana", "R", "modeling", "run_predictions_shrubs_updated.R"))

# Define parameter grid
ndvi_metrics <- c("integrated_ndvi", "winter_spring_integrated")
scenarios <- c("ssp245", "ssp370", "ssp585")
bioclims <- c("bio1", "bio12", "bio9", "bio18",
              "bio1_bio12", "bio1_bio9", "bio1_bio18",
              "bio12_bio9", "bio12_bio18", "bio9_bio18",
              "bio1_bio12_bio9", "bio1_bio12_bio18",
              "bio1_bio9_bio18", "bio12_bio9_bio18",
              "bio1_bio12_bio9_bio18")
models <- c("lm", "rf")

# Calculate total combinations
total_combinations <- length(ndvi_metrics) * length(scenarios) *
                     length(bioclims) * length(models)

cat(sprintf("  - Running %d model combinations...\n", total_combinations))

# Track progress
counter <- 0
errors <- 0

# Run all combinations
for(metric in ndvi_metrics) {
  for(scen in scenarios) {
    for(bio in bioclims) {
      for(mod in models) {

        counter <- counter + 1

        pred_file <- here("vegetation_donana", "data", "results", "temporal_predictions",
                         sprintf("model_predictions_%s_%s_%s_%s.csv",
                                metric, scen, bio, mod))

        if(!file.exists(pred_file)){
          cat(sprintf("    [%d/%d] %s, %s, %s, %s...",
                     counter, total_combinations, metric, scen, bio, mod))

          # Run model with error handling
          results <- try(run_model_combination(
            ndvi_metric = metric,
            scenario = scen,
            bioclim = bio,
            model = mod,
            year_before_pred = 2024,
            lag = 0,
            n.years.pred = 4
          ), silent = TRUE)

          if(!inherits(results, "try-error")) {
            # Combine predictions for all species
            shrub_predictions <- rbind(
              results$predictions$halimium,
              results$predictions$lavandula,
              results$predictions$rosmarinus,
              results$predictions$cistus
            )

            # Save predictions
            write.csv(shrub_predictions, file = pred_file,row.names = FALSE)
            cat(" ✓\n")
          } else {
            cat(" ERROR\n")
            errors <- errors + 1
          }
        } else {
          cat(sprintf("    [%d/%d] %s, %s, %s, %s... EXISTS\n",
                     counter, total_combinations, metric, scen, bio, mod))
        }
      }
    }
  }
}

cat(sprintf("\n  ✓ Temporal predictions complete (%d errors)\n", errors))

# ============================================================================
# STEP 5: Spatial Shrub Predictions ----
# ============================================================================
cat("\nSTEP 5: Running spatial shrub abundance predictions...\n")

# Load spatial prediction functions
source(here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
source(here("vegetation_donana", "R", "modeling", "run_spatial_predictions_updated.R"))

# Define NDVI metrics for spatial predictions
spatial_ndvi_metrics <- c(
  "annual_mean",
  "annual_median",
  "integrated_ndvi",
  "summer_integrated",
  "summer_max",
  "summer_mean",
  "summer_median",
  "winter_spring_integrated",
  "winter_spring_max",
  "winter_spring_mean",
  "winter_spring_median"
)

cat(sprintf("  - Processing %d NDVI metrics...\n", length(spatial_ndvi_metrics)))

# Store results
spatial_predictions <- list()
spatial_errors <- 0

# Run predictions for each metric
for(metric in spatial_ndvi_metrics) {
  cat(sprintf("    %s...", metric))

  tryCatch({
    results <- run_spatial_predictions(
      ndvi_metric = metric,
      year_before_pred = 2024,
      lag = 0
    )

    spatial_predictions[[metric]] <- results
    cat(" ✓\n")

  }, error = function(e) {
    cat(sprintf(" ERROR: %s\n", e$message))
    spatial_errors <- spatial_errors + 1
  })
}

# Save all spatial predictions
spatial_output <- here("vegetation_donana", "data", "results", "spatial_predictions",
                      "spatial_predictions_all.rds")
saveRDS(spatial_predictions, file = spatial_output)

cat(sprintf("\n  ✓ Spatial predictions complete (%d errors)\n", spatial_errors))

# ============================================================================
# STEP 6: Temporal forecast skill ----
# ============================================================================
cat("\nSTEP 6: Running assessment of temporal forecast skill...\n")

# load forecast skill function
source(here("vegetation_donana", "R", "modeling", "run_forecast_skill_temporal.R"))

# load necessary observed data (will be update periodically)

  num <- read.csv(here("data","raw","shrub_abundance_since2007_18_plots.csv"))

  obs <- num[num$species %in% c("Halimium halimifolium", "Lavandula stoechas","Rosmarinus officinalis","Cistus libanotis"),]

  # load predicted data: list and read files in temporal prediction folder

  folder_path <- here("vegetation_donana", "data", "results", "temporal_predictions")
  
  files <- list.files(folder_path, full.names = FALSE)
# go through each file and calculate skill
skill_errors <- 0
  for(x in 1:length(files)){

  cat(paste0("Running file ", x, " of ", length(files)))
  
  tryCatch({
    skill_t <- forecast_skill_temp(type="MSE", prediction_file=files[x], folder_file = folder_path)

    cat(" ✓\n")

  }, error = function(e) {
    cat(sprintf(" ERROR: %s\n", e$message))
    skill_errors <- skill_errors + 1
  })

 # Save output

 skill_file <- here("vegetation_donana", "data", "results", "forecast_skill_temporal",
                     sprintf("skill_temp_%s.csv", gsub('.{4}$', '', files[x]))) # removes .csv from original file name
  
  write.csv(skill_t, file = skill_file,row.names = FALSE)

}

# ============================================================================
# STEP 7: Spatial forecast skill ----
# ============================================================================

# # First run spatial predictions, but save as csv output file

# cat("\nSTEP 7: Running spatial shrub abundance predictions for skills assessment...\n")
# 
# # Load spatial prediction functions
# source(here("vegetation_donana", "R", "modeling", "run_spatial_model_updated.R"))
# source(here("vegetation_donana", "R", "modeling", "run_spatial_predictions_csv_output_example.R"))
# 
# # Define NDVI metrics for spatial predictions
# spatial_ndvi_metrics <- c(
#   "annual_mean",
#   "annual_median",
#   "integrated_ndvi",
#   "summer_integrated",
#   "summer_max",
#   "summer_mean",
#   "summer_median",
#   "winter_spring_integrated",
#   "winter_spring_max",
#   "winter_spring_mean",
#   "winter_spring_median"
# )
# 
# cat(sprintf("  - Processing %d NDVI metrics...\n", length(spatial_ndvi_metrics)))
# 
# # Run predictions for each metric
# for(metric in spatial_ndvi_metrics) {
#   cat(sprintf("    %s...", metric))
#   
#   tryCatch({
#     results <- run_spatial_predictions_csv(
#       ndvi_metric = metric,
#       year_before_pred = 2024
#     )
#     
#     # Save predictions
#     pred_file_sp_scv <- here("vegetation_donana", "data", "results", "spatial_predictions_csv",
#                              sprintf("spatial_predictions_%s.csv",
#                                      metric))
#     write.csv(results, file = pred_file_sp_scv,row.names = FALSE)
#     cat(" ✓\n")
#     
#   }, error = function(e) {
#     cat(sprintf(" ERROR: %s\n", e$message))
#     spatial_errors <- spatial_errors + 1
#   })
# }
# 
# cat(sprintf("\n  ✓ Spatial predictions for skill assessment complete (%d errors)\n", spatial_errors))
# 
# # Second run the spatial forecast skill
# 
# cat("\nSTEP 7b: Running assessment of spatial forecast skill...\n")
# 
# # load forecast skill function
# source(here("vegetation_donana", "R", "modeling", "run_forecast_skill_spatial.R"))
# 
# # load predicted data: list and read files in spatial prediction folder (as csv)
# 
# folder_path_pred <- here("vegetation_donana", "data", "results", "spatial_predictions_csv")
# 
# # load validation data: list and read the most recent validation file
# 
# folder_path_valid <- here("vegetation_donana", "data", "validation")
# 
# files <- list.files(folder_path_pred, full.names = FALSE)
# 
# valid_file <- list.files(folder_path_valid, full.names = FALSE)
# # go through each file and calculate skill
# skill_errors <- 0
# for(x in 1:length(files)){
#   
#   cat(paste0("Running file ", x, " of ", length(files)))
#   
#   tryCatch({
#     skill_sp <- forecast_skill_spatial(type="MSE", prediction_file=files[x], validation_file=valid_file, folder_file_pred = folder_path_pred,folder_file_valid = folder_path_valid)
#     
#     cat(" ✓\n")
#     
#   }, error = function(e) {
#     cat(sprintf(" ERROR: %s\n", e$message))
#     skill_errors <- skill_errors + 1
#   })
#   
#   # Save output
#   
#   skill_file_sp <- here("vegetation_donana", "data", "results", "forecast_skill_spatial",
#                         sprintf("skill_spatial_%s.csv", gsub('.{4}$', '', files[x]))) # removes .csv from original file name
#   
#   write.csv(skill_sp, file = skill_file_sp,row.names = FALSE)
#   
# }
# ============================================================================
# STEP 8: Launch Shiny App ----
# ============================================================================
cat("\n=== Analysis Pipeline Complete ===\n")
cat("\nTo start the Shiny app:\n")
cat("  runApp('vegetation_donana')\n")
cat("\nOr run it now? (uncomment line below)\n")

# Uncomment to auto-launch:
# runApp(here("vegetation_donana"))
