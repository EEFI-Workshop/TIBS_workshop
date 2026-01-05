# NDVI Predictions Script
# This script loads pre-calculated bioclim variables and generates NDVI predictions
# NOTE: Run chelsa_download.R FIRST to download climate data and calculate bioclim variables

# Get the working directory
library(here)
here()

# Get the libraries and install if necessary
source(here("load_packages2.R"))

# Load functions
source(here("ndvi_calculation_functions.R"))

# =============================================================================
# LOAD PRE-CALCULATED BIOCLIM VARIABLES (PRESENT)
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("LOADING BIOCLIM VARIABLES\n")
cat("================================================================================\n")

# Load present bioclim variables for plots
plots_bioclim_file <- here("vegetation_donana", "present_bioclim_plots.csv")
if(!file.exists(plots_bioclim_file)) {
  stop("ERROR: present_bioclim_plots.csv not found!\n",
       "Please run chelsa_download.R first to calculate bioclim variables.")
}
present_bioclim_plots <- read.csv(plots_bioclim_file)
cat("✓ Loaded present bioclim for plots:", nrow(present_bioclim_plots), "records\n")

# Load present bioclim variables for land (if available)
land_bioclim_file <- here("vegetation_donana", "present_bioclim_land.csv")
present_bioclim_land <- NULL
if(file.exists(land_bioclim_file)) {
  present_bioclim_land <- read.csv(land_bioclim_file)
  cat("✓ Loaded present bioclim for land:", nrow(present_bioclim_land), "records\n")
} else {
  cat("⚠ Land bioclim data not found - will use plots data only\n")
}

# =============================================================================
# LOAD FUTURE BIOCLIM VARIABLES
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("LOADING FUTURE BIOCLIM VARIABLES\n")
cat("================================================================================\n")

# Load future bioclim for plots
plots_future_file <- here("vegetation_donana", "future_climate_plots.csv")
if(!file.exists(plots_future_file)) {
  stop("ERROR: future_climate_plots.csv not found!\n",
       "Please run chelsa_download.R first to download future climate data.")
}
future_climate_plots <- read.csv(plots_future_file)
cat("✓ Loaded future bioclim for plots:", nrow(future_climate_plots), "rows\n")

# Load future bioclim for land (if available)
land_future_file <- here("vegetation_donana", "future_climate_land.csv")
future_climate_land <- NULL
if(file.exists(land_future_file)) {
  future_climate_land <- read.csv(land_future_file)
  cat("✓ Loaded future bioclim for land:", nrow(future_climate_land), "rows\n")
}

# Combine future bioclim data
if(!is.null(future_climate_land)) {
  future_climate <- bind_rows(future_climate_plots, future_climate_land)
  cat("✓ Combined future bioclim: plots + land =", nrow(future_climate), "rows\n")
} else {
  future_climate <- future_climate_plots
  cat("✓ Using future bioclim for plots only\n")
}

# =============================================================================
# PREPARE NDVI DATA WITH CLIMATE VARIABLES
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("PREPARING NDVI DATA\n")
cat("================================================================================\n")

# Load and process plots NDVI data
ndvi_plots_data <- read.csv(here("ndvi_metrics_updated.csv")) %>%
  filter(year>=2005)%>%
  select(-Long, -Lat) %>%
  select(-any_of("Elevation")) %>%
  distinct() %>%
  left_join(present_bioclim_plots) %>%
  rename(plot = ID)

cat("✓ Processed plots data:", nrow(ndvi_plots_data), "rows\n")

# Load and process land NDVI data (if available)
ndvi_land_data <- NULL
if(file.exists(here("vegetation_donana", "ndvi_metrics_land_updated.csv")) &&
   !is.null(present_bioclim_land)) {

  cat("Processing land NDVI data...\n")

  # Load land data
  land_raw <- read.csv(here("vegetation_donana", "ndvi_metrics_land_updated.csv"))

  # Create unique land IDs (must match the IDs used in chelsa_download.R)
  land_coords_unique <- land_raw %>%
    select(lat, lon) %>%
    distinct() %>%
    mutate(ID = paste0("LAND_", sprintf("%03d", row_number())))

  # Format land NDVI data
  land_data_clean <- land_raw %>%
    select(-spp, -adult.density) %>%
    distinct() %>%
    left_join(land_coords_unique, by = c("lat", "lon")) %>%
    select(ID, year, annual_mean, annual_median, integrated_ndvi,
           summer_integrated, summer_max, summer_mean, summer_median,
           winter_spring_integrated, winter_spring_max, winter_spring_mean,
           winter_spring_median) %>%
    arrange(ID, year)

  # Merge with bioclim data
  ndvi_land_data <- land_data_clean %>%
    left_join(present_bioclim_land) %>%
    rename(plot = ID)

  cat("✓ Processed land data:", nrow(ndvi_land_data), "rows\n")
}

# Combine plots and land data
if(!is.null(ndvi_land_data)) {
  ndvi_data <- bind_rows(
    ndvi_plots_data %>% mutate(dataset = "plots"),
    ndvi_land_data %>% mutate(dataset = "land")
  )
  cat("✓ Combined datasets:", nrow(ndvi_data), "rows\n")
} else {
  cat("⚠ Using plots data only\n")
  ndvi_data <- ndvi_plots_data %>% mutate(dataset = "plots")
}

# NOTE: scenario_data_list will be created separately for plots and land predictions
# to ensure each model only predicts on the appropriate IDs it was trained on

# =============================================================================
# HELPER FUNCTIONS FOR COMPARISON
# =============================================================================

training_metrics <- c("integrated_ndvi", "winter_spring_integrated", "summer_integrated")

# Function to calculate ranges for a dataset
calculate_ranges <- function(data, metrics, source_name) {
  ranges_df <- data.frame()
  for(metric in metrics) {
    metric_values <- data[[metric]]
    ranges_df <- rbind(ranges_df,
                      data.frame(
                        metric = metric,
                        mean = mean(metric_values, na.rm = TRUE),
                        min = min(metric_values, na.rm = TRUE),
                        max = max(metric_values, na.rm = TRUE),
                        sd = sd(metric_values, na.rm = TRUE),
                        source = source_name,
                        range = max(metric_values, na.rm = TRUE) - min(metric_values, na.rm = TRUE)
                      ))
  }
  return(ranges_df)
}

# =============================================================================
# GENERATE PREDICTIONS FOR PLOTS DATA
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("GENERATING PREDICTIONS FOR PLOTS\n")
cat("================================================================================\n")

if(!file.exists(here("vegetation_donana", "ndvi_predictions_plots.rds"))){

  cat("Running models for plots data...\n")
  plots_data_only <- ndvi_data %>% filter(dataset == "plots") %>% select(-dataset)

  # Get unique plot IDs from training data
  plots_ids <- unique(plots_data_only$plot)
  cat("✓ Training data contains", length(plots_ids), "unique plot IDs\n")

  # Create scenario data list filtered for PLOTS only
  scenario_data_list_plots <- list(
    ssp370 = future_climate_plots %>% filter(scenario == "ssp370"),
    ssp245 = future_climate_plots %>% filter(scenario == "ssp245"),
    ssp585 = future_climate_plots %>% filter(scenario == "ssp585")
  )
  cat("✓ Created scenario data for plots predictions\n")

  if(file.exists(here("vegetation_donana", "data", "results", "ndvi_predictions", "integrated_ndvi_predictions.rds"))){
    integrated_ndvi_plots <-  readRDS(here("vegetation_donana", "data", "results", "ndvi_predictions", "integrated_ndvi_predictions.rds"))
  }else{
  integrated_ndvi_plots <- run_models_scenarios(base_data = plots_data_only,
                                                scenario_data_list = scenario_data_list_plots,
                                                ndvi_metrics = "integrated_ndvi")
  }

  winter_spring_integrated_plots <- run_models_scenarios(base_data = plots_data_only,
                                                         scenario_data_list = scenario_data_list_plots,
                                                         ndvi_metrics = "winter_spring_integrated")

  summer_integrated_plots <- run_models_scenarios(base_data = plots_data_only,
                                                  scenario_data_list = scenario_data_list_plots,
                                                  ndvi_metrics = "summer_integrated")

  ndvi_predictions_plots <- bind_rows(integrated_ndvi_plots,
                                     winter_spring_integrated_plots,
                                     summer_integrated_plots)

  saveRDS(ndvi_predictions_plots, file = here("vegetation_donana", "data", "results", "ndvi_predictions", "ndvi_predictions_plots.rds"))
  cat("✓ Saved plots predictions to ndvi_predictions_plots.rds\n")
} else {
  ndvi_predictions_plots <- readRDS(here("vegetation_donana", "data", "results", "ndvi_predictions", "ndvi_predictions_plots.rds"))
  cat("✓ Loaded existing plots predictions\n")
}

# =============================================================================
# PLOTS COMPARISON: Training vs Predicted
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("COMPARING PLOTS: TRAINING VS PREDICTED NDVI RANGES\n")
cat("================================================================================\n")

# Training data for plots
plots_data_only <- ndvi_data %>% filter(dataset == "plots")
training_ranges_plots <- calculate_ranges(plots_data_only, training_metrics, "Training_plots")

# Predicted data for plots (all scenarios combined)
predicted_ranges_plots <- data.frame()
for(metric in training_metrics) {
  metric_predictions <- ndvi_predictions_plots %>%
    filter(metric == !!metric) %>%
    pull(predicted)

  predicted_ranges_plots <- rbind(predicted_ranges_plots,
                                 data.frame(
                                   metric = metric,
                                   mean = mean(metric_predictions, na.rm = TRUE),
                                   min = min(metric_predictions, na.rm = TRUE),
                                   max = max(metric_predictions, na.rm = TRUE),
                                   sd = sd(metric_predictions, na.rm = TRUE),
                                   source = "Predicted_plots_all",
                                   model = "all",
                                   bioclim_vars = "all",
                                   range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                 ))
}

# Calculate ranges by scenario for plots
scenario_ranges_plots <- data.frame()
unique_scenarios_plots <- unique(ndvi_predictions_plots$scenario)
for(scenario_name in unique_scenarios_plots) {
  for(metric in training_metrics) {
    metric_predictions <- ndvi_predictions_plots %>%
      filter(metric == !!metric, scenario == !!scenario_name) %>%
      pull(predicted)

    scenario_ranges_plots <- rbind(scenario_ranges_plots,
                                  data.frame(
                                    metric = metric,
                                    mean = mean(metric_predictions, na.rm = TRUE),
                                    min = min(metric_predictions, na.rm = TRUE),
                                    max = max(metric_predictions, na.rm = TRUE),
                                    sd = sd(metric_predictions, na.rm = TRUE),
                                    source = paste0("Predicted_plots_", scenario_name),
                                    model = "all",
                                    bioclim_vars = "all",
                                    range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                  ))
  }
}

# Calculate ranges by MODEL TYPE for plots
model_ranges_plots <- data.frame()
unique_models <- unique(ndvi_predictions_plots$model)
for(model_type in unique_models) {
  for(metric in training_metrics) {
    metric_predictions <- ndvi_predictions_plots %>%
      filter(metric == !!metric, model == !!model_type) %>%
      pull(predicted)

    model_ranges_plots <- rbind(model_ranges_plots,
                                data.frame(
                                  metric = metric,
                                  mean = mean(metric_predictions, na.rm = TRUE),
                                  min = min(metric_predictions, na.rm = TRUE),
                                  max = max(metric_predictions, na.rm = TRUE),
                                  sd = sd(metric_predictions, na.rm = TRUE),
                                  source = paste0("Predicted_model_", model_type),
                                  model = model_type,
                                  bioclim_vars = "all",
                                  range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                ))
  }
}

# Calculate ranges by BIOCLIM VARIABLES for plots
bioclim_ranges_plots <- data.frame()
unique_bioclim <- unique(ndvi_predictions_plots$bioclim_vars)
for(bioclim_type in unique_bioclim) {
  for(metric in training_metrics) {
    metric_predictions <- ndvi_predictions_plots %>%
      filter(metric == !!metric, bioclim_vars == !!bioclim_type) %>%
      pull(predicted)

    bioclim_ranges_plots <- rbind(bioclim_ranges_plots,
                                  data.frame(
                                    metric = metric,
                                    mean = mean(metric_predictions, na.rm = TRUE),
                                    min = min(metric_predictions, na.rm = TRUE),
                                    max = max(metric_predictions, na.rm = TRUE),
                                    sd = sd(metric_predictions, na.rm = TRUE),
                                    source = paste0("Predicted_bioclim_", bioclim_type),
                                    model = "all",
                                    bioclim_vars = bioclim_type,
                                    range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                  ))
  }
}

# Add model and bioclim_vars columns to training data for consistency
training_ranges_plots$model <- "observed"
training_ranges_plots$bioclim_vars <- "observed"

# Combine plots comparison data
comparison_data_plots <- rbind(training_ranges_plots, predicted_ranges_plots,
                              scenario_ranges_plots, model_ranges_plots,
                              bioclim_ranges_plots)

# Print plots summary
cat("\n=== PLOTS: Training Data (Observed) ===\n")
print(training_ranges_plots %>%
        select(metric, mean, min, max, sd, range) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n=== PLOTS: Predicted Data (All Scenarios) ===\n")
print(predicted_ranges_plots %>%
        select(metric, mean, min, max, sd, range) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n=== PLOTS: Predicted Data by Scenario ===\n")
print(scenario_ranges_plots %>%
        select(metric, source, mean, min, max, sd, range) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n=== PLOTS: Predicted Data by Model Type ===\n")
print(model_ranges_plots %>%
        select(metric, model, mean, min, max, sd, range) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n=== PLOTS: Predicted Data by Bioclim Variables ===\n")
print(bioclim_ranges_plots %>%
        select(metric, bioclim_vars, mean, min, max, sd, range) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

# Calculate differences for plots
cat("\n=== PLOTS: Difference from Training Data ===\n")
differences_plots <- data.frame()
for(metric in training_metrics) {
  train_mean <- training_ranges_plots %>% filter(metric == !!metric) %>% pull(mean)
  train_range <- training_ranges_plots %>% filter(metric == !!metric) %>% pull(range)

  pred_mean <- predicted_ranges_plots %>% filter(metric == !!metric) %>% pull(mean)
  pred_range <- predicted_ranges_plots %>% filter(metric == !!metric) %>% pull(range)

  differences_plots <- rbind(differences_plots,
                            data.frame(
                              metric = metric,
                              mean_diff = pred_mean - train_mean,
                              mean_pct_change = ((pred_mean - train_mean) / train_mean) * 100,
                              range_diff = pred_range - train_range,
                              range_pct_change = ((pred_range - train_range) / train_range) * 100
                            ))
}
print(differences_plots %>% mutate(across(where(is.numeric), ~round(., 3))))

# Save plots comparison data
write.csv(comparison_data_plots,
          file = here("vegetation_donana", "ndvi_predictions_comparison_plots.csv"),
          row.names = FALSE)

# Create plots comparison figures
# PLOT SET 1: Training vs Overall vs Scenarios
scenario_comparison_plots <- rbind(training_ranges_plots, predicted_ranges_plots, scenario_ranges_plots)

p1a_plots <- ggplot(scenario_comparison_plots, aes(x = metric, y = range, fill = source)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "PLOTS: Range Comparison - Training vs Scenarios",
       x = "NDVI Metric",
       y = "Range (Max - Min)",
       fill = "Data Source") +
  scale_fill_manual(values = c("Training_plots" = "#2E86AB",
                                "Predicted_plots_all" = "#A23B72",
                                "Predicted_plots_ssp245" = "#F18F01",
                                "Predicted_plots_ssp370" = "#C73E1D",
                                "Predicted_plots_ssp585" = "#6A040F"))

p1b_plots <- ggplot(scenario_comparison_plots, aes(x = source, y = mean, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "PLOTS: Mean Values - Training vs Scenarios",
       x = "Data Source",
       y = "Mean NDVI Value") +
  scale_fill_manual(values = c("Training_plots" = "#2E86AB",
                               "Predicted_plots_all" = "#A23B72",
                               "Predicted_plots_ssp245" = "#F18F01",
                               "Predicted_plots_ssp370" = "#C73E1D",
                               "Predicted_plots_ssp585" = "#6A040F"))

# PLOT SET 2: Training vs Model Types
model_comparison_plots <- rbind(
  training_ranges_plots,
  model_ranges_plots
)

p2a_plots <- ggplot(model_comparison_plots, aes(x = metric, y = range, fill = model)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "PLOTS: Range Comparison by Model Type",
       x = "NDVI Metric",
       y = "Range (Max - Min)",
       fill = "Model Type") +
  scale_fill_brewer(palette = "Set2")

p2b_plots <- ggplot(model_comparison_plots, aes(x = model, y = mean, fill = model)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "PLOTS: Mean Values by Model Type",
       x = "Model Type",
       y = "Mean NDVI Value") +
  scale_fill_brewer(palette = "Set2")

# Save plots comparison figure (excluding bioclim variable comparisons)
ggsave(filename = here("vegetation_donana", "ndvi_predictions_comparison_plots.pdf"),
       plot = ggarrange(p1a_plots, p1b_plots, p2a_plots, p2b_plots,
                       ncol = 2, nrow = 2),
       width = 16, height = 12)

cat("\n✓ Plots comparison saved: ndvi_predictions_comparison_plots.pdf/csv\n")

# =============================================================================
# PLOTS TIME SERIES: Observed vs Predicted
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CREATING PLOTS TIME SERIES VISUALIZATION\n")
cat("================================================================================\n")

# Prepare observed data for plots - duplicate for each model facet
observed_plots_base <- plots_data_only %>%
  select(plot, year, integrated_ndvi, winter_spring_integrated, summer_integrated) %>%
  pivot_longer(cols = c(integrated_ndvi, winter_spring_integrated, summer_integrated),
               names_to = "metric",
               values_to = "value") %>%
  mutate(type = "Observed",
         scenario = "Observed")

# Duplicate observed data for each model type (lm, rf)
observed_plots <- bind_rows(
  observed_plots_base %>% mutate(model = "lm"),
  observed_plots_base %>% mutate(model = "rf")
)

# Prepare predicted data for plots
predicted_plots <- ndvi_predictions_plots %>%
  rename(value = predicted) %>%
  mutate(type = paste0("Predicted (", scenario, ")"),
         model_scenario = paste0(model, " - ", scenario))

# Combine observed and predicted
timeseries_plots <- bind_rows(
  observed_plots %>% select(plot, year, metric, value, type, model, scenario),
  predicted_plots %>% select(plot, year, metric, value, type, model, scenario)
)

# Create color palette
scenario_colors <- c(
  "Observed" = "#2E86AB",
  "ssp245" = "#F18F01",
  "ssp370" = "#C73E1D",
  "ssp585" = "#6A040F"
)

# Create time series plot for each metric
ts_plots <- list()
for(metric_name in training_metrics) {
  ts_data <- timeseries_plots %>%
    filter(metric == metric_name, year >= 2005)

  p <- ggplot(ts_data, aes(x = year, y = value,
                           color = scenario,
                           group = interaction(plot, model, scenario))) +
    geom_line(alpha = 0.3, linewidth = 0.5) +
    facet_wrap(~model, ncol = 2) +
    scale_color_manual(values = scenario_colors) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = paste0("PLOTS: ", gsub("_", " ", metric_name)),
         x = "Year",
         y = "NDVI Value",
         color = "Scenario") +
    guides(color = guide_legend(nrow = 1))

  ts_plots[[metric_name]] <- p
}

# Save time series plots
ggsave(filename = here("vegetation_donana", "ndvi_timeseries_plots.pdf"),
       plot = ggarrange(plotlist = ts_plots, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom"),
       width = 14, height = 16)

cat("✓ Plots time series saved: ndvi_timeseries_plots.pdf\n")

# =============================================================================
# GENERATE PREDICTIONS FOR LAND DATA
# =============================================================================

if(!is.null(ndvi_land_data)) {

  cat("\n")
  cat("================================================================================\n")
  cat("GENERATING PREDICTIONS FOR LAND\n")
  cat("================================================================================\n")

  if(!file.exists(here("vegetation_donana", "ndvi_predictions_land.rds"))){

    cat("Running models for land data...\n")
    land_data_only <- ndvi_data %>% filter(dataset == "land") %>% select(-dataset) %>% filter(year>=2005)

    # Get unique land IDs from training data
    land_ids <- unique(land_data_only$plot)
    cat("✓ Training data contains", length(land_ids), "unique land IDs\n")

    # Create scenario data list filtered for LAND only
    scenario_data_list_land <- list(
      ssp370 = future_climate_land %>% filter(scenario == "ssp370"),
      ssp245 = future_climate_land %>% filter(scenario == "ssp245"),
      ssp585 = future_climate_land %>% filter(scenario == "ssp585")
    )
    cat("✓ Created scenario data for land predictions\n")

    integrated_ndvi_land <- run_models_scenarios(base_data = land_data_only,
                                                 scenario_data_list = scenario_data_list_land,
                                                 ndvi_metrics = "integrated_ndvi")

    winter_spring_integrated_land <- run_models_scenarios(base_data = land_data_only,
                                                          scenario_data_list = scenario_data_list_land,
                                                          ndvi_metrics = "winter_spring_integrated")

    summer_integrated_land <- run_models_scenarios(base_data = land_data_only,
                                                   scenario_data_list = scenario_data_list_land,
                                                   ndvi_metrics = "summer_integrated")

    ndvi_predictions_land <- bind_rows(integrated_ndvi_land,
                                      winter_spring_integrated_land,
                                      summer_integrated_land)

    saveRDS(ndvi_predictions_land, file = here("vegetation_donana", "data", "results", "ndvi_predictions", "ndvi_predictions_land.rds"))
    cat("✓ Saved land predictions to ndvi_predictions_land.rds\n")
  } else {
    ndvi_predictions_land <- readRDS(here("vegetation_donana", "data", "results", "ndvi_predictions", "ndvi_predictions_land.rds"))
    cat("✓ Loaded existing land predictions\n")
  }

  # =============================================================================
  # LAND COMPARISON: Training vs Predicted
  # =============================================================================

  cat("\n")
  cat("================================================================================\n")
  cat("COMPARING LAND: TRAINING VS PREDICTED NDVI RANGES\n")
  cat("================================================================================\n")

  # Training data for land
  land_data_only <- ndvi_data %>% filter(dataset == "land")
  training_ranges_land <- calculate_ranges(land_data_only, training_metrics, "Training_land")

  # Predicted data for land (all scenarios combined)
  predicted_ranges_land <- data.frame()
  for(metric in training_metrics) {
    metric_predictions <- ndvi_predictions_land %>%
      filter(metric == !!metric) %>%
      pull(predicted)

    predicted_ranges_land <- rbind(predicted_ranges_land,
                                   data.frame(
                                     metric = metric,
                                     mean = mean(metric_predictions, na.rm = TRUE),
                                     min = min(metric_predictions, na.rm = TRUE),
                                     max = max(metric_predictions, na.rm = TRUE),
                                     sd = sd(metric_predictions, na.rm = TRUE),
                                     source = "Predicted_land_all",
                                     model = "all",
                                     bioclim_vars = "all",
                                     range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                   ))
  }

  # Calculate ranges by scenario for land
  scenario_ranges_land <- data.frame()
  unique_scenarios_land <- unique(ndvi_predictions_land$scenario)
  for(scenario_name in unique_scenarios_land) {
    for(metric in training_metrics) {
      metric_predictions <- ndvi_predictions_land %>%
        filter(metric == !!metric, scenario == !!scenario_name) %>%
        pull(predicted)

      scenario_ranges_land <- rbind(scenario_ranges_land,
                                    data.frame(
                                      metric = metric,
                                      mean = mean(metric_predictions, na.rm = TRUE),
                                      min = min(metric_predictions, na.rm = TRUE),
                                      max = max(metric_predictions, na.rm = TRUE),
                                      sd = sd(metric_predictions, na.rm = TRUE),
                                      source = paste0("Predicted_land_", scenario_name),
                                      model = "all",
                                      bioclim_vars = "all",
                                      range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                    ))
    }
  }

  # Calculate ranges by MODEL TYPE for land
  model_ranges_land <- data.frame()
  unique_models_land <- unique(ndvi_predictions_land$model)
  for(model_type in unique_models_land) {
    for(metric in training_metrics) {
      metric_predictions <- ndvi_predictions_land %>%
        filter(metric == !!metric, model == !!model_type) %>%
        pull(predicted)

      model_ranges_land <- rbind(model_ranges_land,
                                  data.frame(
                                    metric = metric,
                                    mean = mean(metric_predictions, na.rm = TRUE),
                                    min = min(metric_predictions, na.rm = TRUE),
                                    max = max(metric_predictions, na.rm = TRUE),
                                    sd = sd(metric_predictions, na.rm = TRUE),
                                    source = paste0("Predicted_model_", model_type),
                                    model = model_type,
                                    bioclim_vars = "all",
                                    range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                  ))
    }
  }

  # Calculate ranges by BIOCLIM VARIABLES for land
  bioclim_ranges_land <- data.frame()
  unique_bioclim_land <- unique(ndvi_predictions_land$bioclim_vars)
  for(bioclim_type in unique_bioclim_land) {
    for(metric in training_metrics) {
      metric_predictions <- ndvi_predictions_land %>%
        filter(metric == !!metric, bioclim_vars == !!bioclim_type) %>%
        pull(predicted)

      bioclim_ranges_land <- rbind(bioclim_ranges_land,
                                    data.frame(
                                      metric = metric,
                                      mean = mean(metric_predictions, na.rm = TRUE),
                                      min = min(metric_predictions, na.rm = TRUE),
                                      max = max(metric_predictions, na.rm = TRUE),
                                      sd = sd(metric_predictions, na.rm = TRUE),
                                      source = paste0("Predicted_bioclim_", bioclim_type),
                                      model = "all",
                                      bioclim_vars = bioclim_type,
                                      range = max(metric_predictions, na.rm = TRUE) - min(metric_predictions, na.rm = TRUE)
                                    ))
    }
  }

  # Add model and bioclim_vars columns to training data for consistency
  training_ranges_land$model <- "observed"
  training_ranges_land$bioclim_vars <- "observed"

  # Combine land comparison data
  comparison_data_land <- rbind(training_ranges_land, predicted_ranges_land,
                                scenario_ranges_land, model_ranges_land,
                                bioclim_ranges_land)

  # Print land summary
  cat("\n=== LAND: Training Data (Observed) ===\n")
  print(training_ranges_land %>%
          select(metric, mean, min, max, sd, range) %>%
          mutate(across(where(is.numeric), ~round(., 3))))

  cat("\n=== LAND: Predicted Data (All Scenarios) ===\n")
  print(predicted_ranges_land %>%
          select(metric, mean, min, max, sd, range) %>%
          mutate(across(where(is.numeric), ~round(., 3))))

  cat("\n=== LAND: Predicted Data by Scenario ===\n")
  print(scenario_ranges_land %>%
          select(metric, source, mean, min, max, sd, range) %>%
          mutate(across(where(is.numeric), ~round(., 3))))

  cat("\n=== LAND: Predicted Data by Model Type ===\n")
  print(model_ranges_land %>%
          select(metric, model, mean, min, max, sd, range) %>%
          mutate(across(where(is.numeric), ~round(., 3))))

  cat("\n=== LAND: Predicted Data by Bioclim Variables ===\n")
  print(bioclim_ranges_land %>%
          select(metric, bioclim_vars, mean, min, max, sd, range) %>%
          mutate(across(where(is.numeric), ~round(., 3))))

  # Calculate differences for land
  cat("\n=== LAND: Difference from Training Data ===\n")
  differences_land <- data.frame()
  for(metric in training_metrics) {
    train_mean <- training_ranges_land %>% filter(metric == !!metric) %>% pull(mean)
    train_range <- training_ranges_land %>% filter(metric == !!metric) %>% pull(range)

    pred_mean <- predicted_ranges_land %>% filter(metric == !!metric) %>% pull(mean)
    pred_range <- predicted_ranges_land %>% filter(metric == !!metric) %>% pull(range)

    differences_land <- rbind(differences_land,
                              data.frame(
                                metric = metric,
                                mean_diff = pred_mean - train_mean,
                                mean_pct_change = ((pred_mean - train_mean) / train_mean) * 100,
                                range_diff = pred_range - train_range,
                                range_pct_change = ((pred_range - train_range) / train_range) * 100
                              ))
  }
  print(differences_land %>% mutate(across(where(is.numeric), ~round(., 3))))

  # Save land comparison data
  write.csv(comparison_data_land,
            file = here("vegetation_donana", "ndvi_predictions_comparison_land.csv"),
            row.names = FALSE)

  # Create land comparison figures
  # PLOT SET 1: Training vs Overall vs Scenarios
  scenario_comparison_land <- rbind(training_ranges_land, predicted_ranges_land, scenario_ranges_land)

  p1a_land <- ggplot(scenario_comparison_land, aes(x = metric, y = range, fill = source)) +
    geom_col(position = "dodge") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    labs(title = "LAND: Range Comparison - Training vs Scenarios",
         x = "NDVI Metric",
         y = "Range (Max - Min)",
         fill = "Data Source") +
    scale_fill_manual(values = c("Training_land" = "#2E86AB",
                                  "Predicted_land_all" = "#A23B72",
                                  "Predicted_land_ssp245" = "#F18F01",
                                  "Predicted_land_ssp370" = "#C73E1D",
                                  "Predicted_land_ssp585" = "#6A040F"))

  p1b_land <- ggplot(scenario_comparison_land, aes(x = source, y = mean, fill = source)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    facet_wrap(~metric, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = "LAND: Mean Values - Training vs Scenarios",
         x = "Data Source",
         y = "Mean NDVI Value") +
    scale_fill_manual(values = c("Training_land" = "#2E86AB",
                                 "Predicted_land_all" = "#A23B72",
                                 "Predicted_land_ssp245" = "#F18F01",
                                 "Predicted_land_ssp370" = "#C73E1D",
                                 "Predicted_land_ssp585" = "#6A040F"))

  # PLOT SET 2: Training vs Model Types
  model_comparison_land <- rbind(
    training_ranges_land,
    model_ranges_land
  )

  p2a_land <- ggplot(model_comparison_land, aes(x = metric, y = range, fill = model)) +
    geom_col(position = "dodge") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    labs(title = "LAND: Range Comparison by Model Type",
         x = "NDVI Metric",
         y = "Range (Max - Min)",
         fill = "Model Type") +
    scale_fill_brewer(palette = "Set2")

  p2b_land <- ggplot(model_comparison_land, aes(x = model, y = mean, fill = model)) +
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    facet_wrap(~metric, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = "LAND: Mean Values by Model Type",
         x = "Model Type",
         y = "Mean NDVI Value") +
    scale_fill_brewer(palette = "Set2")

  # Save land comparison figure (excluding bioclim variable comparisons)
  ggsave(filename = here("vegetation_donana", "ndvi_predictions_comparison_land.pdf"),
         plot = ggarrange(p1a_land, p1b_land, p2a_land, p2b_land,
                         ncol = 2, nrow = 2),
         width = 16, height = 12)

  cat("\n✓ Land comparison saved: ndvi_predictions_comparison_land.pdf/csv\n")

  # =============================================================================
  # LAND TIME SERIES: Observed vs Predicted
  # =============================================================================

  cat("\n")
  cat("================================================================================\n")
  cat("CREATING LAND TIME SERIES VISUALIZATION\n")
  cat("================================================================================\n")

  # Prepare observed data for land - duplicate for each model facet
  observed_land_base <- land_data_only %>%
    select(plot, year, integrated_ndvi, winter_spring_integrated, summer_integrated) %>%
    pivot_longer(cols = c(integrated_ndvi, winter_spring_integrated, summer_integrated),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(type = "Observed",
           scenario = "Observed")

  # Duplicate observed data for each model type (lm, rf)
  observed_land <- bind_rows(
    observed_land_base %>% mutate(model = "lm"),
    observed_land_base %>% mutate(model = "rf")
  )

  # Prepare predicted data for land
  predicted_land <- ndvi_predictions_land %>%
    rename(value = predicted) %>%
    mutate(type = paste0("Predicted (", scenario, ")"),
           model_scenario = paste0(model, " - ", scenario))

  # Combine observed and predicted
  timeseries_land <- bind_rows(
    observed_land %>% select(plot, year, metric, value, type, model, scenario),
    predicted_land %>% select(plot, year, metric, value, type, model, scenario)
  )

  # Create time series plot for each metric
  ts_land_plots <- list()
  for(metric_name in training_metrics) {
    ts_data <- timeseries_land %>%
      filter(metric == metric_name, year >= 2005)

    p <- ggplot(ts_data, aes(x = year, y = value,
                             color = scenario,
                             group = interaction(plot, model, scenario))) +
      geom_line(alpha = 0.3, linewidth = 0.5) +
      facet_wrap(~model, ncol = 2) +
      scale_color_manual(values = scenario_colors) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(title = paste0("LAND: ", gsub("_", " ", metric_name)),
           x = "Year",
           y = "NDVI Value",
           color = "Scenario") +
      guides(color = guide_legend(nrow = 1))

    ts_land_plots[[metric_name]] <- p
  }

  # Save time series plots
  ggsave(filename = here("vegetation_donana", "ndvi_timeseries_land.pdf"),
         plot = ggarrange(plotlist = ts_land_plots, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom"),
         width = 14, height = 16)

  cat("✓ Land time series saved: ndvi_timeseries_land.pdf\n")

  # Combine predictions
  ndvi_predictions <- bind_rows(ndvi_predictions_plots, ndvi_predictions_land)

} else {
  cat("\n⚠ No land data available - using plots predictions only\n")
  ndvi_predictions <- ndvi_predictions_plots
  ndvi_predictions_land <- NULL
}

# =============================================================================
# CREATE VISUALIZATION DATA
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CREATING VISUALIZATION DATA\n")
cat("================================================================================\n")

ndvi_plot <- ndvi_predictions %>%
  select(metric, model, bioclim_vars, scenario, plot, year, predicted) %>%
  rename(value = predicted) %>%
  mutate(type = "predicted") %>%
  bind_rows(
    ndvi_data %>%
      select(plot, year, integrated_ndvi, winter_spring_integrated, summer_integrated) %>%
      pivot_longer(cols = integrated_ndvi:summer_integrated,
                   names_to = "metric",
                   values_to = "value") %>%
      mutate(type = "observed",
             scenario = "observed",
             bioclim_vars = "observed",
             model = "observed")
  )

saveRDS(ndvi_plot, file = here("vegetation_donana", "data", "processed", "ndvi_plot.rds"))

cat("\n")
cat("================================================================================\n")
cat("NDVI PREDICTIONS COMPLETE!\n")
cat("================================================================================\n")
cat("\nSummary:\n")
cat("- Plots predictions:", nrow(ndvi_predictions_plots), "rows\n")
if(!is.null(ndvi_predictions_land)) {
  cat("- Land predictions:", nrow(ndvi_predictions_land), "rows\n")
}
cat("- Total predictions:", nrow(ndvi_predictions), "rows\n")
cat("- Visualization data saved to: ndvi_plot.rds\n")
cat("- Comparison analysis saved:\n")
cat("  * Plots: ndvi_predictions_comparison_plots.pdf/csv\n")
if(!is.null(ndvi_predictions_land)) {
  cat("  * Land: ndvi_predictions_comparison_land.pdf/csv\n")
}
cat("\n")
