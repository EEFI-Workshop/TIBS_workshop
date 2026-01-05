# Get working directory
library(here)

# Load all packages for the analysis ----
source(here("load_packages2.R"))

# Set working directory ----
setwd(here("vegetation_donana"))

# If you want to run the app locally: ----
runApp(here())

# If you want to perform the analysis yourself: -----
## Get NDVI metric calculations ----#### MARIA: SHOULD BE UPDATED
if(!file.exists(here("ndvi_metrics_updated.csv"))){
  source(here("ndvi_indices_calculations.R"))
}else{
  cat("NDVI metrics are already calculated.")
}

## Get climate data, future climate data and predict NDVI in the future ----
if(!file.exists(here("ndvi_predictions.rds"))){
  source(here("ndvi_predictions.R"))
}else{
  cat("NDVI predictions are already calculated.")
}

## Forecast shrub abundances with NDVI predictions ----
source(here("run_predictions_shrubs_updated.R"))
source(here("run_spatial_model_updated.R"))

## Run a single model ----
run_eg = run_model_combination(ndvi_metric = "integrated_ndvi", # NDVI metric as the explanatory variable for the shrub model
                               scenario = "ssp370", # Future scenario with which we predict future NDVI
                               bioclim = "bio1",  # Climatic variable with which we predict future NDVI
                               model = "lm", # Model with which we predict the future NDVI
                               year_before_pred = 2024,
                               lag = 0,
                               n.years.pred = 4, # That's how it was written on the script (MARIA: THTA'S CORRECT)
                               n.years.obs = 2
                               )

## Run all the model combinations for temporal predictions ----

if(!file.exists(here("observed_totals.rds"))){
# Load future observations data
  year_before_pred = 2024
num <- read.csv(here("shrub_abundance_since2007_18_plots.csv"))
  num_fut <- num[num$year>=year_before_pred,]
  sub_fut <- num_fut[num_fut$species %in% c("Halimium halimifolium", "Lavandula stoechas","Rosmarinus officinalis","Cistus libanotis"),]


# Calculate observed totals
# For Lavandula
lav_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Lavandula stoechas",], 
                     function(x) sum(x,na.rm=T))
lav_obs$species <- "Lavandula stoechas"
lav_obs$tot <- lav_obs$adults
lav_obs$year <- as.numeric(as.character(lav_obs$year))

# For Halimium
hal_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Halimium halimifolium",], 
                     function(x) sum(x,na.rm=T))
hal_obs$species <- "Halimium halimifolium"
hal_obs$tot <- hal_obs$adults
hal_obs$year <- as.numeric(as.character(hal_obs$year))

# For Rosmarinus
ros_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Rosmarinus officinalis",], 
                     function(x) sum(x,na.rm=T))
ros_obs$species <- "Rosmarinus officinalis"
ros_obs$tot <- ros_obs$adults
ros_obs$year <- as.numeric(as.character(ros_obs$year))

# For Cistus
cis_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Cistus libanotis",], 
                     function(x) sum(x,na.rm=T))
cis_obs$species <- "Cistus libanotis"
cis_obs$tot <- cis_obs$adults
cis_obs$year <- as.numeric(as.character(ros_obs$year))

# Combine observed data
observed_totals <- rbind(
  lav_obs[, c("year", "species", "tot")],
  hal_obs[, c("year", "species", "tot")],
  ros_obs[, c("year", "species", "tot")],
  cis_obs[, c("year", "species", "tot")]
)

# Save observed data
saveRDS(observed_totals, file = here("observed_totals.rds"))
}else{
  observed_totals = readRDS(here("observed_totals.rds"))
}

# Define parameter combinations - predictions and forecasting skill are saved to "model_runs".
ndvi_metrics <- c("integrated_ndvi", "winter_spring_integrated")
scenarios <- c("ssp370", "ssp245", "ssp585")
bioclims <- c("bio1", "bio12", "bio9", "bio18", "bio1_bio12", "bio1_bio9", 
              "bio1_bio18", "bio12_bio9", "bio12_bio18", "bio9_bio18", "bio1_bio12_bio9", 
              "bio1_bio12_bio18", "bio1_bio9_bio18", "bio12_bio9_bio18", "bio1_bio12_bio9_bio18"
)
models <- c("lm", "rf")

# Create empty list to store results
all_predictions <- list()
counter <- 1

# Run analysis for all combinations
for(metric in ndvi_metrics) {
  for(scen in scenarios) {
    for(bio in bioclims) {
      for(mod in models) {
        
        pred_file = here("model_runs",
                         sprintf("model_predictions_%s_%s_%s_%s.rds", 
                                 metric, scen, bio, mod))
        
        if(!file.exists(pred_file)){
          cat(sprintf("Running combination %s, %s, %s, %s\n", metric, scen, bio, mod))
          
          # Run model
          results <- try(run_model_combination(metric, scen, bio, mod, year_before_pred = 2024, lag = 0, n.years.pred = 4, n.years.obs = 2))
          
          if(!inherits(results, "try-error")) {
            # Prepare predictions dataframe (already includes MSE)
            shrub_predictions <- rbind(
              results$predictions$halimium,
              results$predictions$lavandula,
              results$predictions$rosmarinus,
              results$predictions$cistus
            )
            
            # Save predictions (which now include MSE values)
            saveRDS(shrub_predictions, 
                    file = here("model_runs",
                                sprintf("model_predictions_%s_%s_%s_%s.rds", 
                                        metric, scen, bio, mod)))
          }
        } else {
          cat(sprintf("Combination %s, %s, %s, %s\n", metric, scen, bio, mod), " already exists!")
        }
      }
    }
  }
}

## Run the models for spatial projections ----
source(here("run_spatial_predictions_updated.R"))
ndvi_metrics <- c(
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

# Create list to store results
spatial_predictions <- list()

# Run predictions for each metric
for(metric in ndvi_metrics) {
  cat(sprintf("\nProcessing %s...\n", metric))
  
  # Try to run predictions
  tryCatch({
    results <- run_spatial_predictions(metric, year_before_pred = 2024, lag = 0)
    
    # Store results
    spatial_predictions[[metric]] <- results
    
    cat(sprintf("Successfully processed %s\n", metric))
  }, error = function(e) {
    cat(sprintf("Error processing %s: %s\n", metric, e$message))
  })
}

# Save all predictions to RDS file
saveRDS(spatial_predictions, file = here("spatial_predictions","spatial_predictions.rds"))

## Start the shiny app locally ----
runApp('vegetation_donana')

