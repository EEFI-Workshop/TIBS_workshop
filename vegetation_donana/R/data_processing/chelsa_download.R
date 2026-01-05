# CHELSA Climate Data Download Script
# This script downloads present and future climate data for plots and land coordinates
# Uses hierarchical download strategy: Year -> Month -> Day

# Get the working directory
library(here)
here()

# Load packages
source(here("load_packages2.R"))

# Load functions
source(here("ndvi_calculation_functions.R"))

# Define the forecast year
year_to_forecast = 2030
# =============================================================================
# EFFICIENT CHELSA DOWNLOAD FUNCTIONS WITH YEAR -> MONTH -> DAY HIERARCHY
# =============================================================================

# Helper function to download data for a specific time period
download_chelsa_period <- function(variable, coords, start_date, end_date) {
  tryCatch({
    daily_data <- getChelsa(variable, coords = coords,
                           startdate = start_date, enddate = end_date)
    return(daily_data)
  }, error = function(e) {
    return(NULL)
  })
}

# Function to process downloaded CHELSA data into clean format
process_chelsa_data <- function(data_list, coords, coord_ids, variable) {
  if(length(data_list) == 0) return(NULL)

  processed_data <- data_list %>%
    map(get_present_climate) %>%
    bind_rows() %>%
    left_join(data.frame(lon = coords[,1], lat = coords[,2], ID = coord_ids)) %>%
    mutate(time = as.Date(time, "%Y-%m-%d")) %>%
    select(ID, time, value)

  # Apply units conversion for temperature
  if(variable == "tas") {
    processed_data <- processed_data %>%
      mutate(value = value - 273.15) %>%  # Convert to Celsius
      rename(temperature = value)
  } else {
    processed_data <- processed_data %>%
      rename(precipitation = value)
  }

  return(processed_data)
}

# Main efficient download function with year -> month -> day hierarchy
download_climate_efficient <- function(variable, coords, coord_ids, existing_data,
                                      start_date, end_date, description = "") {

  cat("\n=== Efficient download:", variable, "for", description, "===\n")
  cat("Date range:", as.character(start_date), "to", as.character(end_date), "\n")

  # Find missing dates for this specific variable
  all_dates <- seq(from = start_date, to = end_date, by = "day")

  if(!is.null(existing_data) && nrow(existing_data) > 0) {
    # Check which dates have NON-NA values for this specific variable
    if(variable == "tas" && "temperature" %in% colnames(existing_data)) {
      existing_dates <- unique(existing_data$time[!is.na(existing_data$temperature)])
    } else if(variable == "pr" && "precipitation" %in% colnames(existing_data)) {
      existing_dates <- unique(existing_data$time[!is.na(existing_data$precipitation)])
    } else {
      existing_dates <- unique(existing_data$time)
    }
    missing_dates <- all_dates[!all_dates %in% existing_dates]
  } else {
    missing_dates <- all_dates
  }

  cat("Total dates needed:", length(all_dates), "\n")
  cat("Already have:", length(all_dates) - length(missing_dates), "dates\n")
  cat("Missing dates to download:", length(missing_dates), "\n")

  if(length(missing_dates) == 0) {
    cat("No missing dates - using existing data only\n")
    if(variable == "tas") {
      return(existing_data %>% select(ID, time, temperature))
    } else {
      return(existing_data %>% select(ID, time, precipitation))
    }
  }

  # Organize missing dates by year and month
  missing_df <- data.frame(date = missing_dates) %>%
    mutate(
      year = year(date),
      month = month(date)
    )

  all_new_data <- list()
  success_count <- 0

  # Strategy: Try year -> month -> day
  years_to_download <- unique(missing_df$year)

  for(yr in years_to_download) {
    cat("\n--- Processing year", yr, "---\n")
    year_dates <- missing_df %>% filter(year == yr) %>% pull(date)
    year_start <- min(year_dates)
    year_end <- max(year_dates)

    # STEP 1: Try downloading the entire year at once
    cat("Attempting to download full year", yr, "(", length(year_dates), "dates)...\n")
    year_data <- download_chelsa_period(variable, coords, year_start, year_end)

    if(!is.null(year_data)) {
      cat("✓ Successfully downloaded full year", yr, "\n")
      all_new_data[[paste0("year_", yr)]] <- year_data
      success_count <- success_count + length(year_dates)
      next  # Move to next year
    }

    # STEP 2: Year download failed, try month by month
    cat("✗ Full year download failed. Trying month-by-month...\n")
    months_in_year <- unique(missing_df %>% filter(year == yr) %>% pull(month))

    for(mo in months_in_year) {
      month_dates <- missing_df %>% filter(year == yr, month == mo) %>% pull(date)
      month_start <- min(month_dates)
      month_end <- max(month_dates)

      cat(sprintf("  Attempting to download %d-%02d (%d dates)...\n",
                  yr, mo, length(month_dates)))
      month_data <- download_chelsa_period(variable, coords, month_start, month_end)

      if(!is.null(month_data)) {
        cat(sprintf("  ✓ Successfully downloaded %d-%02d\n", yr, mo))
        all_new_data[[paste0("year_", yr, "_month_", mo)]] <- month_data
        success_count <- success_count + length(month_dates)
        next  # Move to next month
      }

      # STEP 3: Month download failed, download day by day
      cat(sprintf("  ✗ Month download failed. Trying day-by-day for %d-%02d...\n", yr, mo))

      for(i in seq_along(month_dates)) {
        date <- month_dates[i]

        day_data <- download_chelsa_period(variable, coords, date, date)

        if(!is.null(day_data)) {
          all_new_data[[as.character(date)]] <- day_data
          success_count <- success_count + 1
        }

        # Progress update every 20 days
        if(i %% 20 == 0) {
          cat(sprintf("    Progress: %d/%d days in %d-%02d\n",
                      i, length(month_dates), yr, mo))
        }
      }
    }
  }

  cat("\n=== Download complete ===\n")
  cat("Total successful:", success_count, "out of", length(missing_dates), "missing dates\n")

  # Process new data
  new_data <- NULL
  if(length(all_new_data) > 0) {
    cat("Processing", length(all_new_data), "downloaded chunks...\n")
    new_data <- process_chelsa_data(all_new_data, coords, coord_ids, variable)
  }

  # Combine existing and new data using FULL JOIN (not bind_rows!)
  if(!is.null(existing_data) && nrow(existing_data) > 0) {
    if(!is.null(new_data)) {
      # Use full_join to properly merge temperature and precipitation
      if(variable == "tas") {
        # For temperature: merge new temp data with existing data (which may have precip)
        combined_data <- full_join(
          existing_data %>% select(ID, time, any_of(c("temperature", "precipitation"))),
          new_data,
          by = c("ID", "time")
        ) %>%
          mutate(
            # Use new temperature if available, otherwise keep existing
            temperature = coalesce(temperature.y, temperature.x)
          ) %>%
          select(ID, time, temperature, any_of("precipitation")) %>%
          distinct()
      } else {
        # For precipitation: merge new precip data with existing data (which may have temp)
        combined_data <- full_join(
          existing_data %>% select(ID, time, any_of(c("temperature", "precipitation"))),
          new_data,
          by = c("ID", "time")
        ) %>%
          mutate(
            # Use new precipitation if available, otherwise keep existing
            precipitation = coalesce(precipitation.y, precipitation.x)
          ) %>%
          select(ID, time, any_of("temperature"), precipitation) %>%
          distinct()
      }
      cat("Merged", nrow(existing_data), "existing +", nrow(new_data), "new =",
          nrow(combined_data), "total unique records\n")
    } else {
      # No new data, return existing as-is
      combined_data <- existing_data %>% distinct()
      cat("Using", nrow(combined_data), "existing records only\n")
    }
  } else {
    combined_data <- new_data
    if(!is.null(new_data)) {
      cat("Using", nrow(combined_data), "new records only\n")
    }
  }

  return(combined_data)
}

# =============================================================================
# DOWNLOAD PRESENT CLIMATE DATA FOR PLOTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("DOWNLOADING PRESENT CLIMATE DATA FOR PLOTS\n")
cat("================================================================================\n")

# Load plot coordinates
coords_long <- read.csv(here("coords_plot_since2007.csv"))
coords_plots <- data.frame(lon = coords_long$Long, lat = coords_long$Lat)
coord_ids_plots <- coords_long$ID

# File path for plots climate data
plots_present_file <- here("vegetation_donana", "present_climate_plots.csv")

# Load existing climate data if available
existing_climate_plots <- NULL
if(file.exists(plots_present_file)) {
  existing_climate_plots <- read.csv(plots_present_file, stringsAsFactors = FALSE)
  # Parse dates - CSV has YYYY-MM-DD format
  existing_climate_plots$time <- as.Date(existing_climate_plots$time, format = "%Y-%m-%d")

  cat("Loaded existing plots climate data with", nrow(existing_climate_plots), "records\n")
  cat("Date range:", as.character(min(existing_climate_plots$time, na.rm = TRUE)),
      "to", as.character(max(existing_climate_plots$time, na.rm = TRUE)), "\n")
}

# Download temperature data (2005-2023)
tas_plots <- download_climate_efficient("tas", coords_plots, coord_ids_plots,
                                       existing_climate_plots,
                                       as.Date("2005-01-01"), as.Date("2023-12-31"),
                                       "plots temperature")

# Download precipitation data (2005-2023)
pr_plots <- download_climate_efficient("pr", coords_plots, coord_ids_plots,
                                      existing_climate_plots,
                                      as.Date("2005-01-01"), as.Date("2023-12-31"),
                                      "plots precipitation")

# Combine and save
if(!is.null(tas_plots) && !is.null(pr_plots)) {
  # Ensure we only have clean column names before final join
  tas_clean <- tas_plots %>% select(ID, time, temperature)
  pr_clean <- pr_plots %>% select(ID, time, precipitation)

  present_climate_plots <- full_join(tas_clean, pr_clean, by = c("ID", "time"))
  write.csv(present_climate_plots, file = plots_present_file, row.names = FALSE)
  cat("\n✓ Saved plots climate data to:", plots_present_file, "\n")
  cat("  Total records:", nrow(present_climate_plots), "\n")
} else {
  stop("Failed to download climate data for plots")
}

# =============================================================================
# DOWNLOAD PRESENT CLIMATE DATA FOR LAND
# =============================================================================

if(file.exists(here("vegetation_donana", "ndvi_metrics_land_updated.csv"))) {

  cat("\n")
  cat("================================================================================\n")
  cat("DOWNLOADING PRESENT CLIMATE DATA FOR LAND\n")
  cat("================================================================================\n")

  # Load land coordinates
  land_raw <- read.csv(here("vegetation_donana", "ndvi_metrics_land_updated.csv"))
  land_coords_unique <- land_raw %>%
    select(lat, lon) %>%
    distinct() %>%
    mutate(ID = paste0("LAND_", sprintf("%03d", row_number())))

  cat("Created", nrow(land_coords_unique), "unique land coordinate IDs\n")

  coords_land <- data.frame(
    lon = land_coords_unique$lon,
    lat = land_coords_unique$lat
  )
  coord_ids_land <- land_coords_unique$ID

  # File path for land climate data
  land_present_file <- here("vegetation_donana", "present_climate_land.csv")

  # Load existing climate data if available
  existing_climate_land <- NULL
  if(file.exists(land_present_file)) {
    existing_climate_land <- read.csv(land_present_file, stringsAsFactors = FALSE)
    # Parse dates - CSV has YYYY-MM-DD format
    existing_climate_land$time <- as.Date(existing_climate_land$time, format = "%Y-%m-%d")

    cat("Loaded existing land climate data with", nrow(existing_climate_land), "records\n")
    cat("Date range:", as.character(min(existing_climate_land$time, na.rm = TRUE)),
        "to", as.character(max(existing_climate_land$time, na.rm = TRUE)), "\n")
  }

  # Download temperature data (2005-2023)
  tas_land <- download_climate_efficient("tas", coords_land, coord_ids_land,
                                         existing_climate_land,
                                         as.Date("2005-01-01"), as.Date("2023-12-31"),
                                         "land temperature")

  # Download precipitation data (2005-2023)
  pr_land <- download_climate_efficient("pr", coords_land, coord_ids_land,
                                        existing_climate_land,
                                        as.Date("2005-01-01"), as.Date("2023-12-31"),
                                        "land precipitation")

  # Combine and save
  if(!is.null(tas_land) && !is.null(pr_land)) {
    # Ensure we only have clean column names before final join
    tas_land_clean <- tas_land %>% select(ID, time, temperature)
    pr_land_clean <- pr_land %>% select(ID, time, precipitation)

    present_climate_land <- full_join(tas_land_clean, pr_land_clean, by = c("ID", "time"))
    write.csv(present_climate_land, file = land_present_file, row.names = FALSE)
    cat("\n✓ Saved land climate data to:", land_present_file, "\n")
    cat("  Total records:", nrow(present_climate_land), "\n")
  } else {
    cat("Warning: Failed to download complete land climate data\n")
  }

} else {
  cat("\n⚠ Land NDVI data file not found - skipping land climate download\n")
}

# =============================================================================
# DOWNLOAD FUTURE CLIMATE DATA FOR PLOTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("DOWNLOADING FUTURE CLIMATE DATA FOR PLOTS\n")
cat("================================================================================\n")

# Define scenarios and years
scenarios <- c('ssp245', 'ssp370', 'ssp585')
target_years <- 2021:year_to_forecast

# Function to check which years are missing for a given scenario and location type
check_missing_years <- function(scenario, location_type, years) {
  missing_years <- c()
  for(yr in years) {
    file_pattern <- paste0("future_climate_", scenario, "_", location_type, "_", yr, ".csv")
    if(!file.exists(here("vegetation_donana", file_pattern))) {
      missing_years <- c(missing_years, yr)
    }
  }
  return(missing_years)
}

# Check which years need to be downloaded for plots
cat("Checking which years need to be downloaded for plots...\n")

any_missing <- FALSE
for(scenario in scenarios) {
  missing_years <- check_missing_years(scenario, "plots", target_years)

  if(length(missing_years) > 0) {
    any_missing <- TRUE
    cat(sprintf("Downloading %s scenario for plots: years %s\n",
                scenario, paste(missing_years, collapse = ", ")))
    get_future_clim(coords = coords_long, scenario = scenario,
                    output = paste0("future_climate_", scenario, "_plots"),
                    years = missing_years)
  } else {
    cat(sprintf("✓ All years already exist for %s plots\n", scenario))
  }
}

# Only regenerate combined file if we downloaded new data OR if it doesn't exist
if(any_missing || !file.exists(here("vegetation_donana", "future_climate_plots.csv"))){

  # Clean up temporary folders
  folders_to_delete <- list.files(here("vegetation_donana"),
                                  pattern = "future_climate.*(ssp\\d+)_plots$",
                                  full.names = TRUE)
  folders_to_delete <- folders_to_delete[file.info(folders_to_delete)$isdir]
  if(length(folders_to_delete) > 0) {
    unlink(folders_to_delete, recursive = TRUE)
  }

  # Get all the future climate with different scenarios for plots
  future_climate_plots <- get_all_futures(scenario = scenarios, location_type = "plots") %>%
    rename(plot = ID) %>%
    select(plot, scenario, year, variable, value) %>%
    filter(stringr::str_detect(variable, "bio")) %>%
    mutate(value = ifelse(variable %in% c("bio1", "bio2", "bio5", "bio6", "bio7",
                                          "bio8", "bio9", "bio10", "bio11"),
                          value - 273.15, value)) %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # Save future climate data for plots
  write.csv(future_climate_plots,
            file = here("vegetation_donana", "future_climate_plots.csv"),
            row.names = FALSE)
  cat("✓ Saved future climate for plots\n")
} else {
  cat("✓ Future climate for plots already exists\n")
}

# =============================================================================
# DOWNLOAD FUTURE CLIMATE DATA FOR LAND
# =============================================================================

if(file.exists(here("vegetation_donana", "ndvi_metrics_land_updated.csv"))) {

  cat("\n")
  cat("================================================================================\n")
  cat("DOWNLOADING FUTURE CLIMATE DATA FOR LAND\n")
  cat("================================================================================\n")

  # Load land coordinates
  cat("Checking which years need to be downloaded for land...\n")

  land_raw <- read.csv(here("vegetation_donana", "ndvi_metrics_land_updated.csv"))
  land_coords_unique <- land_raw %>%
    select(lat, lon) %>%
    distinct() %>%
    mutate(ID = paste0("LAND_", sprintf("%03d", row_number())))

  coords_land_for_future <- data.frame(
    Long = land_coords_unique$lon,
    Lat = land_coords_unique$lat,
    ID = land_coords_unique$ID
  )

  # Download only missing years for each scenario
  any_missing_land <- FALSE
  for(scenario in scenarios) {
    missing_years <- check_missing_years(scenario, "land", target_years)

    if(length(missing_years) > 0) {
      any_missing_land <- TRUE
      cat(sprintf("Downloading %s scenario for land: years %s\n",
                  scenario, paste(missing_years, collapse = ", ")))
      get_future_clim(coords = coords_land_for_future, scenario = scenario,
                      output = paste0("future_climate_", scenario, "_land"),
                      years = missing_years)
    } else {
      cat(sprintf("✓ All years already exist for %s land\n", scenario))
    }
  }

  # Only regenerate combined file if we downloaded new data OR if it doesn't exist
  if(any_missing_land || !file.exists(here("vegetation_donana", "future_climate_land.csv"))) {

    # Clean up temporary folders
    folders_to_delete <- list.files(here("vegetation_donana"),
                                    pattern = "future_climate.*(ssp\\d+)_land$",
                                    full.names = TRUE)
    folders_to_delete <- folders_to_delete[file.info(folders_to_delete)$isdir]
    if(length(folders_to_delete) > 0) {
      unlink(folders_to_delete, recursive = TRUE)
    }

    # Get all the future climate with different scenarios for land
    future_climate_land <- get_all_futures(scenario = scenarios, location_type = "land") %>%
      rename(plot = ID) %>%
      select(plot, scenario, year, variable, value) %>%
      filter(stringr::str_detect(variable, "bio")) %>%
      mutate(value = ifelse(variable %in% c("bio1", "bio2", "bio5", "bio6", "bio7",
                                            "bio8", "bio9", "bio10", "bio11"),
                            value - 273.15, value)) %>%
      pivot_wider(names_from = "variable", values_from = "value")

    # Save future climate data for land
    write.csv(future_climate_land,
              file = here("vegetation_donana", "future_climate_land.csv"),
              row.names = FALSE)
    cat("✓ Saved future climate for land\n")

  } else {
    cat("✓ Future climate for land already exists\n")
  }
}

# =============================================================================
# CALCULATE BIOCLIM VARIABLES FOR PRESENT CLIMATE
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CALCULATING BIOCLIM VARIABLES FOR PRESENT CLIMATE\n")
cat("================================================================================\n")

# Calculate bioclim variables for plots
cat("Calculating bioclim variables for plots...\n")
present_bioclim_plots <- calculate_bioclim_vars(present_climate_plots,
                                               date_col = "time",
                                               temp_col = "temperature",
                                               precip_col = "precipitation",
                                               id_col = "ID")
cat("✓ Calculated bioclim for", length(unique(present_bioclim_plots$ID)), "plot IDs\n")

# Save present bioclim for plots
write.csv(present_bioclim_plots,
          file = here("vegetation_donana", "present_bioclim_plots.csv"),
          row.names = FALSE)
cat("✓ Saved present_bioclim_plots.csv\n")

# Calculate bioclim variables for land (if available)
present_bioclim_land <- NULL
if(exists("present_climate_land") && !is.null(present_climate_land)) {
  cat("Calculating bioclim variables for land...\n")
  present_bioclim_land <- calculate_bioclim_vars(present_climate_land,
                                                 date_col = "time",
                                                 temp_col = "temperature",
                                                 precip_col = "precipitation",
                                                 id_col = "ID")
  cat("✓ Calculated bioclim for", length(unique(present_bioclim_land$ID)), "land IDs\n")

  # Save present bioclim for land
  write.csv(present_bioclim_land,
            file = here("vegetation_donana", "present_bioclim_land.csv"),
            row.names = FALSE)
  cat("✓ Saved present_bioclim_land.csv\n")
}

# =============================================================================
# PREPARE BIOCLIM TIMESERIES DATA (PRESENT + FUTURE)
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("PREPARING BIOCLIM TIMESERIES DATA\n")
cat("================================================================================\n")

# Select key bioclim variables for plotting
key_bioclim_vars <- c("bio1", "bio12", "bio9", "bio18")

# PLOTS timeseries
cat("Preparing plots bioclim timeseries...\n")

# Present bioclim for plots (long format)
present_plots_long <- present_bioclim_plots %>%
  select(ID, year, all_of(key_bioclim_vars)) %>%
  mutate(scenario = "Present (observed)") %>%
  pivot_longer(cols = all_of(key_bioclim_vars),
               names_to = "variable",
               values_to = "value")

# Future bioclim for plots (long format)
future_plots_long <- NULL
if(file.exists(here("vegetation_donana", "future_climate_plots.csv"))) {
  future_climate_plots_data <- read.csv(here("vegetation_donana", "future_climate_plots.csv"))

  future_plots_long <- future_climate_plots_data %>%
    rename(ID = plot) %>%
    select(ID, scenario, year, all_of(key_bioclim_vars)) %>%
    pivot_longer(cols = all_of(key_bioclim_vars),
                 names_to = "variable",
                 values_to = "value") %>%
    mutate(scenario = paste0("Future (", scenario, ")"))
}

# Combine present and future for plots
plots_bioclim_timeseries <- bind_rows(present_plots_long, future_plots_long)

cat("✓ Prepared plots timeseries with", nrow(plots_bioclim_timeseries), "records\n")

# LAND timeseries (if available)
land_bioclim_timeseries <- NULL
if(!is.null(present_bioclim_land)) {
  cat("Preparing land bioclim timeseries...\n")

  # Present bioclim for land (long format)
  present_land_long <- present_bioclim_land %>%
    select(ID, year, all_of(key_bioclim_vars)) %>%
    mutate(scenario = "Present (observed)") %>%
    pivot_longer(cols = all_of(key_bioclim_vars),
                 names_to = "variable",
                 values_to = "value")

  # Future bioclim for land (long format)
  future_land_long <- NULL
  if(file.exists(here("vegetation_donana", "future_climate_land.csv"))) {
    future_climate_land_data <- read.csv(here("vegetation_donana", "future_climate_land.csv"))

    future_land_long <- future_climate_land_data %>%
      rename(ID = plot) %>%
      select(ID, scenario, year, all_of(key_bioclim_vars)) %>%
      pivot_longer(cols = all_of(key_bioclim_vars),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(scenario = paste0("Future (", scenario, ")"))
  }

  # Combine present and future for land
  land_bioclim_timeseries <- bind_rows(present_land_long, future_land_long)

  cat("✓ Prepared land timeseries with", nrow(land_bioclim_timeseries), "records\n")
}

# =============================================================================
# CREATE BIOCLIM TIMESERIES PLOTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CREATING BIOCLIM TIMESERIES VISUALIZATIONS\n")
cat("================================================================================\n")

# Variable labels
variable_labels <- c(
  "bio1" = "Annual Mean Temperature (°C)",
  "bio12" = "Annual Precipitation (mm)",
  "bio9" = "Mean Temperature of Driest Quarter (°C)",
  "bio18" = "Precipitation of Warmest Quarter (mm)"
)

# PLOTS timeseries figure
# Get all years for x-axis
all_years <- sort(unique(plots_bioclim_timeseries$year))

# Define colors for scenarios
scenario_colors <- c(
  "Present (observed)" = "#2E86AB",
  "Future (ssp245)" = "#F18F01",
  "Future (ssp370)" = "#C73E1D",
  "Future (ssp585)" = "#6A040F"
)

p_plots <- ggplot(plots_bioclim_timeseries,
                  aes(x = year, y = value, group = interaction(ID, scenario), color = scenario)) +
  geom_line(alpha = 0.4, linewidth = 0.5) +
  facet_wrap(~variable, scales = "free_y", ncol = 2,
             labeller = labeller(variable = variable_labels)) +
  scale_x_continuous(breaks = all_years) +
  scale_color_manual(values = scenario_colors,
                     name = "Scenario") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "PLOTS: Bioclim Variables Timeseries (Present & Future)",
    subtitle = "Each line represents one plot location colored by scenario",
    x = "Year",
    y = "Value"
  )

# LAND timeseries figure (if available)
p_land <- NULL
if(!is.null(land_bioclim_timeseries)) {
  # Get all years for x-axis
  all_years_land <- sort(unique(land_bioclim_timeseries$year))

  p_land <- ggplot(land_bioclim_timeseries,
                   aes(x = year, y = value, group = interaction(ID, scenario), color = scenario)) +
    geom_line(alpha = 0.4, linewidth = 0.5) +
    facet_wrap(~variable, scales = "free_y", ncol = 2,
               labeller = labeller(variable = variable_labels)) +
    scale_x_continuous(breaks = all_years_land) +
    scale_color_manual(values = scenario_colors,
                       name = "Scenario") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      title = "LAND: Bioclim Variables Timeseries (Present & Future)",
      subtitle = "Each line represents one land location colored by scenario",
      x = "Year",
      y = "Value"
    )
}

# Save combined PDF
if(!is.null(p_land)) {
  ggsave(
    filename = here("vegetation_donana", "bioclim_timeseries.pdf"),
    plot = ggarrange(p_plots, p_land, ncol = 1, nrow = 2),
    width = 12,
    height = 14
  )
  cat("✓ Saved bioclim_timeseries.pdf (plots + land)\n")
} else {
  ggsave(
    filename = here("vegetation_donana", "bioclim_timeseries.pdf"),
    plot = p_plots,
    width = 12,
    height = 7
  )
  cat("✓ Saved bioclim_timeseries.pdf (plots only)\n")
}

# Save timeseries data
write.csv(plots_bioclim_timeseries,
          file = here("vegetation_donana", "plots_bioclim_timeseries.csv"),
          row.names = FALSE)
cat("✓ Saved plots_bioclim_timeseries.csv\n")

if(!is.null(land_bioclim_timeseries)) {
  write.csv(land_bioclim_timeseries,
            file = here("vegetation_donana", "land_bioclim_timeseries.csv"),
            row.names = FALSE)
  cat("✓ Saved land_bioclim_timeseries.csv\n")
}

cat("\n")
cat("================================================================================\n")
cat("CHELSA DOWNLOAD & BIOCLIM CALCULATION COMPLETE!\n")
cat("================================================================================\n")
cat("\nOutputs created:\n")
cat("  - present_bioclim_plots.csv\n")
if(!is.null(present_bioclim_land)) {
  cat("  - present_bioclim_land.csv\n")
}
cat("  - plots_bioclim_timeseries.csv\n")
if(!is.null(land_bioclim_timeseries)) {
  cat("  - land_bioclim_timeseries.csv\n")
}
cat("  - bioclim_timeseries.pdf\n")
cat("\nYou can now run ndvi_predictions.R to generate predictions.\n\n")
