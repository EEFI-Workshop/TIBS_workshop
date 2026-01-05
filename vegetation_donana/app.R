# Required libraries
library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(colorspace)
library(here)
library(terra)
library(tidyterra)#added to use the rast function
library(aws.s3)#added for MinIO/S3 file upload
library(lubridate)#added for date handling in validation

options(warn = -1)
ndvi_plot <- readRDS(here("data","processed","ndvi_plot.rds"))
ndvi_processed_raw <- readRDS(here("data","processed","ndvi_processed.rds"))
shrub_data <- read.csv(here("data","raw","shrub_number.csv"))
colnames(shrub_data)[5:7]=c("adults","saplings","seedlings")
observed_total <- read_csv(here("data","processed","observed_total.csv"))[,-1]
observed_plot <- read_csv(here("data","processed","observed_plot.csv"))[,-1]
ndvi_processed_raw <- ndvi_processed_raw %>% filter(year>=2007)
# UI
ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Doñana National Park",
    status = "success",
    skin = "light",
    compact = TRUE,
    fixed = TRUE,
    controlbarIcon = NULL
  ),
  bs4DashSidebar(
    status = "success",
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = FALSE,
    bs4SidebarMenu(
      bs4Dash::menuItem("NDVI Data", tabName = "ndvi_data", icon = icon("leaf")),
      bs4Dash::menuItem("Shrub Data", tabName = "shrub_data", icon = icon("tree")),
      bs4Dash::menuItem("NDVI Predictions", tabName = "ndvi_pred", icon = icon("leaf")),
      bs4Dash::menuItem("Shrub Predictions", tabName = "shrub_pred", icon = icon("clock")),
      bs4Dash::menuItem("Shrub Predictions", tabName = "shrub_pred_spatial", icon = icon("map")),
      bs4Dash::menuItem("Submit Forecast", tabName = "submit_forecast", icon = icon("upload")),
      bs4Dash::menuItem("Download Files", tabName = "download_files", icon = icon("download"))

    )
  ),
  
  bs4DashBody(
    tabItems(
      # NDVI Data Tab
      tabItem(
        tabName = "ndvi_data",
        fluidRow(
          column(
            width = 12,
            bs4Card(
              width = 8,
              title = "NDVI Raw Data",
              status = "success",
              plotlyOutput("ndvi_raw_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Data Tab
      tabItem(
        tabName = "shrub_data",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select",
              status = "success",
              selectInput("shrub_data_species", "Choose species:",
                          choices = unique(shrub_data$species))
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "Vegetation surveys",
              status = "success",
              plotlyOutput("shrub_data_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # NDVI Predictions Tab
      tabItem(
        tabName = "ndvi_pred",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("ndvi_pred_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated", "summer_integrated"),
                          selected = "integrated_ndvi"),
              selectInput("ndvi_pred_scenario", "Choose IPCC scenario:",
                          choices = c("ssp370", "ssp245", "ssp585"),
                          selected = "ssp370"),
              selectInput("ndvi_pred_model", "Choose type of model:",
                          choices = c("lm", "rf"),
                          selected = "lm"),
              selectInput("ndvi_pred_climate", "Choose climatic variables:",
                          choices = c("bio1", "bio12", "bio9", "bio18"),
                          multiple = TRUE,
                          selected = "bio1")
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "NDVI Prediction Results",
              status = "success",
              plotlyOutput("ndvi_pred_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Predictions Tab
      tabItem(
        tabName = "shrub_pred",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("shrub_pred_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated", "summer_integrated")),
              selectInput("shrub_pred_scenario", "Choose IPCC scenario:",
                          choices = c("ssp370", "ssp245", "ssp585")),
              selectInput("shrub_pred_climate", "Choose climatic variables:",
                          choices = c("bio1", "bio12", "bio9", "bio18"),
                          multiple = TRUE,
                          selected = "bio1"),
              selectInput("shrub_pred_model", "Choose NDVI model:",
                          choices = c("lm", "rf")),
              selectInput("shrub_pred_species", "Choose species variables:",
                          choices = c("Lavandula stoechas","Halimium halimifolium", "Rosmarinus officinalis","Cistus libanotis"),
                          multiple = FALSE,
                          selected = "Lavandula stoechas")
              
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "Shrub Prediction Results",
              status = "success",
              plotlyOutput("shrub_pred_plot", height = "600px") %>% withSpinner(type = 8)
            ),
            
            bs4Card(
              width = 12,
              title = "Forecasting skill",
              status = "success",
              plotlyOutput("shrub_temp_skill_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Predictions Tab
      tabItem(
        tabName = "shrub_pred_spatial",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("ndvi_spatial_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated")),
              selectInput("shrub_spatial_species", "Choose species variables:",
                          choices = c("Lavandula stoechas","Halimium halimifolium","Rosmarinus officinalis", "Cistus libanotis"),
                          multiple = FALSE,
                          selected = "Lavandula stoechas")
              
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "NDVI indice across the landscape",
              status = "success",
              collapsed = TRUE,
              plotlyOutput("ndvi_spatial_plot", height = "600px") %>% withSpinner(type = 8)
            ),
            
            bs4Card(
              width = 12,
              title = "Shrub abundance across the landscape",
              status = "success",
              plotlyOutput("shrub_spatial_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),

      # Submit Forecast Tab
      tabItem(
        tabName = "submit_forecast",
        fluidRow(
          column(
            width = 6,
            bs4Card(
              width = 12,
              title = "Participant Information",
              status = "success",
              textInput("participant_name", "Name:", placeholder = "Enter your first name"),
              textInput("participant_surname", "Surname:", placeholder = "Enter your last name"),
              textInput("participant_institution", "Institution:", placeholder = "Enter your institution"),
              textInput("participant_email", "Email:", placeholder = "your.email@example.com")
            ),
            bs4Card(
              width = 12,
              title = "Upload Forecast",
              status = "success",
              selectInput("forecast_type", "Forecast Type:",
                         choices = c("Temporal" = "temporal", "Spatial" = "spatial"),
                         selected = "temporal"),
              fileInput("forecast_file", "Choose CSV File:",
                       accept = c(".csv")),
              shiny::actionButton("submit_forecast_btn", "Submit Forecast",
                          class = "btn-success btn-lg",
                          style = "width: 100%;")
            )
          ),
          column(
            width = 6,
            bs4Card(
              width = 12,
              title = "Submission Status",
              status = "info",
              uiOutput("validation_output") %>% withSpinner(type = 8)
            ),
            bs4Card(
              width = 12,
              title = "Instructions",
              status = "warning",
              collapsed = TRUE,
              HTML("<p>Please ensure your forecast file follows the required format:</p>
                   <ul>
                   <li><strong>Temporal forecasts:</strong> Must include columns: project_id, model_id, forecast_type, datetime, reference_datetime, duration, site_id, species, family, uncertainty_component, variable, prediction</li>
                   <li><strong>Spatial forecasts:</strong> Additionally require: cell_id, cell_lat, cell_lon</li>
                   <li>Datetime format: YYYY-MM-DD HH:MM:SS</li>
                   <li>Duration: ISO 8601 format (e.g., P1D for 1 day)</li>
                   <li>Family: normal, lognormal, bernoulli, beta, uniform, gamma, logistic, exponential, poisson, or sample</li>
                   <li>Uncertainty component: For parametric families (e.g., normal), use parameter names (mu, sigma). For sample family, use numeric indices (1, 2, 3...)</li>
                   </ul>
                   <p>See example files for reference format.</p>")
            )
          )
        )
      ),

      # Download Files Tab
      tabItem(
        tabName = "download_files",
        fluidRow(
          column(
            width = 12,
            bs4Card(
              width = 12,
              title = "Available Files for Download",
              status = "primary",
              fluidRow(
                column(
                  width = 8,
                  textInput("file_search", "Search files:",
                           placeholder = "Type to filter files by name...",
                           width = "100%")
                ),
                column(
                  width = 4,
                  shiny::actionButton("refresh_files_btn", "Refresh File List",
                                     icon = icon("refresh"),
                                     class = "btn-primary",
                                     style = "margin-top: 25px;")
                )
              ),
              hr(),
              uiOutput("file_list_output") %>% withSpinner(type = 8)
            )
          )
        )
      )


    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Create reactive dataset for NDVI predictions
  ndvi_filtered_data <- reactive({
    # Filter predictions based on user selections
    ndvi_plot %>%
      filter(
        metric %in% c(input$ndvi_pred_metric, "observed"),
        model %in%  c(input$ndvi_pred_model, "observed"),
        scenario %in% c(input$ndvi_pred_scenario, "observed"),
        bioclim_vars %in% c(paste(input$ndvi_pred_climate, collapse = "_"), "observed")
      ) 
  })
  
  # Render the plotly plot
  output$ndvi_pred_plot <- renderPlotly({
    # Create base ggplot
    p <- ndvi_filtered_data() %>%
      filter(year >= 2007, year <= 2025) %>%
      ggplot(aes(year, value, group = plot, color  = type)) +
      xlim(2007, 2025)+
      geom_point(alpha = 0.3) +
      geom_line(alpha = 0.3) +
      stat_summary(aes(year, value, group = type, color = type), 
                   fun = mean, geom = "line", size = 1.5) +
      stat_summary(aes(year, value, group = type, color = type), 
                   fun = mean, geom = "point", size = 3) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "NDVI Value",
        title = paste("NDVI Predictions-", input$ndvi_pred_metric)
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )+
      scale_color_manual(values = c("grey80", "black"))
    
    # Convert to plotly for interactivity
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 50)
      )
  })

  
  #Raw NDVI Plot
  output$ndvi_raw_plot <- renderPlotly({
    ggplot(ndvi_processed_raw, aes(date, int.NDVI, color = plot), message = FALSE) +
      geom_line(alpha = 0.2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "NDVI Trends Over Time",
           subtitle = "Solid lines = significant trends over the years (p < 0.05)\nDashed lines = non-significant trends over the years",
           x = "Date",
           y = "NDVI") +
      theme(legend.position = "right")
  })
  
  # Shrub Plot
  output$shrub_data_plot <- renderPlotly({
    shrub_data %>%
      filter(species == input$shrub_data_species)%>%
   ggplot(aes(year, adults))+
      geom_point(aes(group = plot))+
      geom_line(aes(group = plot))+
      stat_summary(aes(year, adults), 
                   fun = mean, geom = "line", size = 1.5,
                   color = "green") +
      stat_summary(aes(year, adults), 
                   fun = mean, geom = "point", size = 3,
                   color = "green") +
      theme_minimal()+
      labs(x = "Year", y = "Number of adults")+
      ggtitle("Shrub species trends over time")
  })
  
  # Shrub Plot
  output$shrub_pred_plot <- renderPlotly({
    shrub_pred_data = read_csv(paste0(here("data","results","temporal_predictions/"),
                                 paste(c("model_predictions",
                                    input$shrub_pred_metric,
                                    input$shrub_pred_scenario,
                                    paste(input$shrub_pred_climate, collapse = "_"),
                                    input$shrub_pred_model),
                                    collapse = "_"), ".csv"))%>%
      filter(species == input$shrub_pred_species)%>%
      mutate(metric = input$shrub_pred_metric,
             scenario = input$shrub_pred_scenario,
             bioclim = paste(input$shrub_pred_climate, collapse = "_"),
             model = input$shrub_pred_model,
             year = as.numeric(substr(datetime, 1, 4)))%>%
      group_by(year, uncertainty_component, metric, scenario, bioclim, model, species)%>%
      summarize(prediction = sum(prediction, na.rm = TRUE))

    shrub_observed =
      observed_total %>%
      filter(species == input$shrub_pred_species)%>%
      rename(observed = tot)

    shrub_observed_plot =
      observed_plot %>%
      filter(species == input$shrub_pred_species)%>%
      rename(observed = obs)

    # Combine observed data with predictions for connecting lines
    shrub_combined <- shrub_pred_data %>%
      group_by(uncertainty_component) %>%
      do({
        obs_2024 <- shrub_observed %>% filter(year == 2024)
        if(nrow(obs_2024) > 0) {
          bind_rows(
            data.frame(year = 2024,
                      prediction = obs_2024$observed,
                      uncertainty_component = .$uncertainty_component[1],
                      metric = .$metric[1],
                      scenario = .$scenario[1],
                      bioclim = .$bioclim[1],
                      model = .$model[1],
                      species = .$species[1]),
            .
          )
        } else {
          .
        }
      })

    ggplot(data = shrub_combined)+
      geom_line(aes(year, prediction, group = uncertainty_component), alpha = 0.1)+
      stat_summary(data = shrub_pred_data, aes(x = year, y = prediction),
                   fun = mean,
                   geom = "line",
                   color = "red",
                   linewidth = 1)+
      geom_point(data=shrub_observed, aes(year, observed), size=3,col="blue")+
      scale_color_viridis_c(direction = -1)+
      xlab("Year")+ylab("Number of adults")+
      theme_minimal(base_size=20)+
      ggtitle(input$shrub_pred_species)+
      theme(legend.position = "bottom")+
      labs(x = "Year", y = "Number of adults across the landscape",
           subtitle = "Blue point shows the observed number in 2024, connected to predictions")
  })
  
  # Shrub forecasting skill
  output$shrub_temp_skill_plot <- renderPlotly({
    shrub_temp_skill = read_csv(paste0(here("data","results","forecast_skill_temporal/"),
                                     paste(c("skill_temp_model_predictions",
                                             input$shrub_pred_metric,
                                             input$shrub_pred_scenario,
                                             paste(input$shrub_pred_climate, collapse = "_"),
                                             input$shrub_pred_model),
                                           collapse = "_"), ".csv")) %>%
      filter(species == input$shrub_pred_species) %>%
      mutate(metric = input$shrub_pred_metric,
             scenario = input$shrub_pred_scenario,
             bioclim = paste(input$shrub_pred_climate, collapse = "_"),
             model = input$shrub_pred_model,
             year = as.numeric(substr(datetime, 1, 4)))%>%
      group_by(year, uncertainty_component, metric, scenario, bioclim, model, species)%>%
      summarize(skill = median(skill, na.rm = TRUE))

    p <- ggplot(shrub_temp_skill, aes(x = as.factor(year), skill)) +
      geom_boxplot(outliers = FALSE)+
      theme_minimal() +
      labs(
        title = "Forecasting skill",
        x = "Year",
        y = unique(shrub_temp_skill$type.skill)
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )

    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 50)
      )
  })
  
  output$ndvi_spatial_plot <- renderPlotly({
    ndvi_spatial = rast(here("data","ndvi_rasters","ndvi_metrics_rasters", "2024", paste0(input$ndvi_spatial_metric, "_2024.tif")))
    
    p <- ggplot() +
      geom_spatraster(data = ndvi_spatial) +
      scale_fill_gradientn(
        colors = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"),
        na.value = NA
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 50)
      )
  })

  output$shrub_spatial_plot <- renderPlotly({
    sp = case_when(input$shrub_spatial_species == "Lavandula stoechas"~"lavandula", 
                   input$shrub_spatial_species == "Halimium halimifolium"~"halimium",
                   input$shrub_spatial_species == "Cistus libanotis"~"cistus", 
                   input$shrub_spatial_species == "Rosmarinus officinalis"~"rosmarinus"
                   )
    shrub_spatial = rast(here("data","results","spatial_predictions", paste0(sp,"_", input$ndvi_spatial_metric, ".tif")))
    threshold=25
    shrub_spatial[shrub_spatial>threshold] = threshold
    
    # violet_gradient = c("#f3e5f5","#6a1b9a")
    # orange_gradient = c("#fff3e0","#ef6c00")
    # blue_gradient = c("#BFEFFF", "#436EEE")
    # yellow_gradient = c("#FFFACD", "#CDCD00")
    
    p <- ggplot() +
      geom_spatraster(data = shrub_spatial) +
      # scale_fill_gradient(
      #   low = case_when(sp == "lavandula"~violet_gradient[1], 
      #                   sp == "halimium"~orange_gradient[1],
      #                   sp == "rosmarinus"~blue_gradient[1],
      #                   sp == "cistus"~yellow_gradient[1]),
      #   high = case_when(sp == "lavandula"~violet_gradient[2], 
      #                   sp == "halimium"~orange_gradient[2],
      #                   sp == "rosmarinus"~blue_gradient[2],
      #                   sp == "cistus"~yellow_gradient[2]),
      #   na.value = NA
      # ) +
      scale_fill_gradient2(
        low = "blue", 
        mid = "yellow", 
        high = "red",
        midpoint = threshold/2,
        limits = c(0, threshold),
        na.value = "red",  # Values above threshold become red
        oob = scales::squish,  # Squish out-of-bounds values to limits
        name = "Abundance"
      ) +
      labs(title = "Predicted abudances of Halimium halimifolium in 2025",
           subtitle = paste("Red indicates values ≥", threshold),
           y="Latitude",
           x="Longitude") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 50)
      )
  })

  # ============================================================
  # FORECAST SUBMISSION LOGIC
  # ============================================================

  # MinIO/S3 Configuration (credentials from environment variables)
  # Set these in .Renviron file:
  # AWS_ACCESS_KEY_ID=your_access_key
  # AWS_SECRET_ACCESS_KEY=your_secret_key
  # AWS_S3_ENDPOINT=minio.lifewatch.eu
  # AWS_DEFAULT_REGION=us-east-1

  S3_BUCKET <- "donanadt.lifewatch.dev-private"
  S3_ENDPOINT <- Sys.getenv("AWS_S3_ENDPOINT", "minio.lifewatch.eu")

  # Credentials should come from environment variables
  # For local testing: set in .Renviron file (see .Renviron.template)
  # For server deployment: set in GitLab CI/CD variables or docker-compose.yml

  # Configure aws.s3 for MinIO - use path-style (not virtual-hosted style)
  Sys.setenv(
    "AWS_S3_ENDPOINT" = S3_ENDPOINT,
    "AWS_VIRTUAL_HOSTING" = "FALSE"
  )

  # Reactive value to store validation results
  validation_results <- reactiveVal(NULL)

  # Handle forecast submission
  observeEvent(input$submit_forecast_btn, {
    # Reset validation results
    validation_results(NULL)

    # Validate inputs
    if (is.null(input$forecast_file)) {
      validation_results(list(
        valid = FALSE,
        errors = c("Please select a file to upload"),
        warnings = character()
      ))
      return()
    }

    if (input$participant_name == "" || input$participant_surname == "") {
      validation_results(list(
        valid = FALSE,
        errors = c("Please provide your name and surname"),
        warnings = character()
      ))
      return()
    }

    if (input$participant_institution == "") {
      validation_results(list(
        valid = FALSE,
        errors = c("Please provide your institution"),
        warnings = character()
      ))
      return()
    }

    if (input$participant_email == "" || !grepl("@", input$participant_email)) {
      validation_results(list(
        valid = FALSE,
        errors = c("Please provide a valid email address"),
        warnings = character()
      ))
      return()
    }

    # Show processing message
    validation_results(list(processing = TRUE))

    # Read uploaded file
    tryCatch({
      cat("DEBUG: Reading uploaded file...\n")
      forecast_data <- read.csv(input$forecast_file$datapath, stringsAsFactors = FALSE)
      cat("DEBUG: File read successfully, rows:", nrow(forecast_data), "\n")

      # Source validation functions
      cat("DEBUG: Sourcing validation script...\n")
      source(here("submission_validator.R"), local = TRUE)
      cat("DEBUG: Validation script sourced\n")

      # Run validation
      cat("DEBUG: Running validation...\n")
      val_results <- validate_forecast(forecast_data, input$forecast_type)
      cat("DEBUG: Validation complete. Valid:", val_results$valid, "\n")

      # If validation passes, upload to server
      if (val_results$valid) {
        cat("DEBUG: Validation passed, preparing upload...\n")
        # Create unique filename with metadata
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        filename <- paste0(
          input$forecast_type, "_",
          input$participant_surname, "_",
          input$participant_name, "_",
          timestamp,
          ".csv"
        )
        cat("DEBUG: Filename created:", filename, "\n")

        # Create metadata file
        metadata_filename <- paste0(
          input$forecast_type, "_",
          input$participant_surname, "_",
          input$participant_name, "_",
          timestamp,
          "_metadata.txt"
        )

        metadata_content <- paste0(
          "========================================\n",
          "FORECAST SUBMISSION METADATA\n",
          "========================================\n\n",
          "Submission Information:\n",
          "----------------------\n",
          "Name: ", input$participant_name, "\n",
          "Surname: ", input$participant_surname, "\n",
          "Institution: ", input$participant_institution, "\n",
          "Email: ", input$participant_email, "\n",
          "Submission Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n",
          "Forecast Details:\n",
          "----------------\n",
          "Forecast Type: ", input$forecast_type, "\n",
          "Original Filename: ", input$forecast_file$name, "\n",
          "Uploaded Filename: ", filename, "\n\n",
          "Validation Results:\n",
          "------------------\n",
          "Number of Rows: ", val_results$n_rows, "\n",
          "Number of Sites: ", val_results$n_sites, "\n",
          "Number of Species: ", val_results$n_species, "\n"
        )

        if (input$forecast_type == "spatial") {
          metadata_content <- paste0(metadata_content,
            "Number of Cells: ", val_results$n_cells, "\n")
        }

        if (length(val_results$warnings) > 0) {
          metadata_content <- paste0(metadata_content,
            "\nWarnings:\n",
            paste(paste0("- ", val_results$warnings), collapse = "\n"), "\n")
        }

        metadata_content <- paste0(metadata_content,
          "\n========================================\n")

        # Write metadata to temp file
        cat("DEBUG: Creating metadata file...\n")
        metadata_temp_file <- tempfile(fileext = ".txt")
        writeLines(metadata_content, metadata_temp_file)
        cat("DEBUG: Metadata file created at:", metadata_temp_file, "\n")
        cat("DEBUG: Metadata file size:", file.size(metadata_temp_file), "bytes\n")

        # Try to upload forecast file to MinIO/S3
        cat("DEBUG: Starting CSV upload to MinIO...\n")
        cat("DEBUG: Upload path:", paste0("submissions/", filename), "\n")
        cat("DEBUG: Bucket:", S3_BUCKET, "\n")
        cat("DEBUG: Endpoint:", S3_ENDPOINT, "\n")

        upload_result <- tryCatch({
          cat("DEBUG: Calling put_object...\n")
          flush.console()

          result <- put_object(
            file = input$forecast_file$datapath,
            object = paste0("submissions/", filename),
            bucket = S3_BUCKET,
            region = "",
            base_url = S3_ENDPOINT,
            use_https = TRUE,
            accelerate = FALSE,
            verbose = FALSE
          )

          cat("DEBUG: put_object returned\n")
          flush.console()
          cat("DEBUG: CSV upload successful!\n")
          flush.console()

          list(success = TRUE, error = NULL)
        }, error = function(e) {
          cat("DEBUG: CSV upload FAILED!\n")
          cat("DEBUG: Error:", conditionMessage(e), "\n")
          flush.console()

          error_msg <- paste("Upload failed:", conditionMessage(e))
          # Add more details for debugging
          if (grepl("Could not resolve host", error_msg)) {
            error_msg <- paste0(error_msg, " - Check S3_ENDPOINT: ", S3_ENDPOINT)
          } else if (grepl("403", error_msg) || grepl("Forbidden", error_msg)) {
            error_msg <- paste0(error_msg, " - Check credentials (Access Key/Secret)")
          } else if (grepl("404", error_msg) || grepl("NoSuchBucket", error_msg)) {
            error_msg <- paste0(error_msg, " - Bucket '", S3_BUCKET, "' not found")
          }
          val_results$errors <- c(val_results$errors, error_msg)

          list(success = FALSE, error = error_msg)
        })

        upload_success <- upload_result$success

        cat("DEBUG: After tryCatch for CSV upload\n")
        cat("DEBUG: upload_success value:", upload_success, "\n")
        flush.console()

        # Also upload metadata file - try alternative method
        cat("DEBUG: Starting metadata upload...\n")
        flush.console()

        metadata_upload_success <- FALSE
        if (upload_success) {
          cat("DEBUG: CSV upload was successful, proceeding with metadata...\n")
          flush.console()

          metadata_result <- tryCatch({
            cat("DEBUG: Calling put_object for metadata...\n")
            flush.console()

            put_object(
              file = metadata_temp_file,
              object = paste0("submissions/", metadata_filename),
              bucket = S3_BUCKET,
              region = "",
              base_url = S3_ENDPOINT,
              use_https = TRUE,
              accelerate = FALSE,
              verbose = FALSE
            )

            cat("DEBUG: Metadata put_object returned\n")
            flush.console()

            list(success = TRUE, error = NULL)
          }, error = function(e) {
            cat("DEBUG: Metadata upload error:", conditionMessage(e), "\n")
            flush.console()
            list(success = FALSE, error = conditionMessage(e))
          })

          metadata_upload_success <- metadata_result$success
          cat("DEBUG: Metadata upload result:", metadata_upload_success, "\n")
          flush.console()
        } else {
          cat("DEBUG: Skipping metadata upload because CSV upload failed\n")
          flush.console()
        }
        cat("DEBUG: Metadata upload section complete. Success:", metadata_upload_success, "\n")
        flush.console()

        cat("DEBUG: Setting upload success flags...\n")
        if (upload_success) {
          val_results$upload_success <- TRUE
          val_results$filename <- filename
          cat("DEBUG: Upload marked as successful\n")
        } else {
          val_results$upload_success <- FALSE
          val_results$errors <- c(val_results$errors,
                                 "Failed to upload file to server. Please try again.")
          cat("DEBUG: Upload marked as failed\n")
        }
      }

      cat("DEBUG: Storing validation results...\n")
      validation_results(val_results)
      cat("DEBUG: Validation results stored!\n")

    }, error = function(e) {
      validation_results(list(
        valid = FALSE,
        errors = c(paste("Error reading file:", e$message)),
        warnings = character()
      ))
    })
  })

  # Render validation output
  output$validation_output <- renderUI({
    results <- validation_results()

    if (is.null(results)) {
      return(HTML("<p style='color: gray; font-style: italic;'>Upload a file and click 'Submit Forecast' to validate and submit.</p>"))
    }

    if (!is.null(results$processing) && results$processing) {
      return(HTML("<p style='color: blue;'>Processing and validating your submission...</p>"))
    }

    output_html <- ""

    # Show validation status
    if (results$valid && !is.null(results$upload_success) && results$upload_success) {
      output_html <- paste0(
        "<div style='background-color: #d4edda; padding: 15px; border-radius: 5px; border: 1px solid #c3e6cb;'>",
        "<h4 style='color: #155724; margin-top: 0;'>✓ Submission Successful!</h4>",
        "<p style='color: #155724;'><strong>Validation passed and file uploaded successfully.</strong></p>",
        "<p style='color: #155724;'>File: ", results$filename, "</p>",
        "<p style='color: #155724;'>Rows: ", results$n_rows, " | Species: ", results$n_species, "</p>",
        "</div>"
      )
    } else if (results$valid && is.null(results$upload_success)) {
      output_html <- paste0(
        "<div style='background-color: #d4edda; padding: 15px; border-radius: 5px; border: 1px solid #c3e6cb;'>",
        "<h4 style='color: #155724; margin-top: 0;'>✓ Validation Passed</h4>",
        "<p style='color: #155724;'>Your file format is correct!</p>",
        "</div>"
      )
    } else {
      output_html <- paste0(
        "<div style='background-color: #f8d7da; padding: 15px; border-radius: 5px; border: 1px solid #f5c6cb;'>",
        "<h4 style='color: #721c24; margin-top: 0;'>✗ Validation Failed</h4>",
        "<p style='color: #721c24;'><strong>Please check the documentation for file submission format.</strong></p>"
      )
    }

    # Show errors
    if (length(results$errors) > 0) {
      output_html <- paste0(
        output_html,
        "<div style='margin-top: 10px;'><strong style='color: #721c24;'>Errors:</strong><ul style='color: #721c24;'>"
      )
      for (error in results$errors) {
        output_html <- paste0(output_html, "<li>", error, "</li>")
      }
      output_html <- paste0(output_html, "</ul></div>")
    }

    # Show warnings
    if (length(results$warnings) > 0) {
      output_html <- paste0(
        output_html,
        "<div style='margin-top: 10px;'><strong style='color: #856404;'>Warnings:</strong><ul style='color: #856404;'>"
      )
      for (warning in results$warnings) {
        output_html <- paste0(output_html, "<li>", warning, "</li>")
      }
      output_html <- paste0(output_html, "</ul></div>")
    }

    if (!results$valid || (!is.null(results$upload_success) && !results$upload_success)) {
      output_html <- paste0(output_html, "</div>")
    }

    HTML(output_html)
  })

  # ============================================================
  # FILE DOWNLOAD LOGIC
  # ============================================================

  # Public bucket for downloads
  PUBLIC_BUCKET <- "donanadt.lifewatch.dev"

  # Reactive value to store file list
  file_list <- reactiveVal(NULL)

  # Function to get file list from S3
  get_file_list <- function() {
    tryCatch({
      files <- get_bucket_df(
        bucket = PUBLIC_BUCKET,
        region = "",
        base_url = S3_ENDPOINT,
        use_https = TRUE
      )
      return(files)
    }, error = function(e) {
      cat("Error listing files:", conditionMessage(e), "\n")
      return(NULL)
    })
  }

  # Load file list on app start
  observe({
    file_list(get_file_list())
  })

  # Refresh file list when button clicked
  observeEvent(input$refresh_files_btn, {
    file_list(get_file_list())
  })

  # Render file list with download buttons
  output$file_list_output <- renderUI({
    files <- file_list()

    if (is.null(files)) {
      return(HTML("<p style='color: red;'>Error loading file list. Please try refreshing.</p>"))
    }

    if (nrow(files) == 0) {
      return(HTML("<p style='color: gray;'>No files available for download.</p>"))
    }

    # Filter files based on search query
    search_query <- input$file_search
    if (!is.null(search_query) && search_query != "") {
      files <- files[grepl(search_query, files$Key, ignore.case = TRUE), ]
    }

    # Check if any files match the search
    if (nrow(files) == 0) {
      return(HTML("<p style='color: gray;'>No files found matching your search.</p>"))
    }

    # Create download buttons for each file
    download_buttons <- lapply(1:nrow(files), function(i) {
      file_key <- files$Key[i]
      file_size <- round(as.numeric(files$Size[i]) / 1024, 2)  # KB
      file_modified <- as.character(files$LastModified[i])

      # Create unique output ID for this file
      output_id <- paste0("download_", gsub("[^[:alnum:]]", "_", file_key))

      # Register download handler for this file
      output[[output_id]] <- downloadHandler(
        filename = function() {
          basename(file_key)
        },
        content = function(file) {
          save_object(
            object = file_key,
            bucket = PUBLIC_BUCKET,
            file = file,
            region = "",
            base_url = S3_ENDPOINT,
            use_https = TRUE
          )
        }
      )

      # Return table row HTML
      div(
        class = "row mb-2",
        style = "border-bottom: 1px solid #ddd; padding: 10px;",
        div(class = "col-md-5", strong(file_key)),
        div(class = "col-md-2", paste(file_size, "KB")),
        div(class = "col-md-3", file_modified),
        div(class = "col-md-2",
            downloadButton(output_id, "Download", class = "btn-success btn-sm")
        )
      )
    })

    # Return the list wrapped in a container
    tagList(
      div(
        class = "row mb-2",
        style = "font-weight: bold; border-bottom: 2px solid #333; padding: 10px;",
        div(class = "col-md-5", "File Name"),
        div(class = "col-md-2", "Size"),
        div(class = "col-md-3", "Last Modified"),
        div(class = "col-md-2", "Action")
      ),
      HTML(paste0("<p style='color: #666; font-size: 0.9em; margin: 10px 0;'>Showing ", nrow(files), " file(s)</p>")),
      download_buttons
    )
  })
}
# Run the apptest
# shinyApp(ui = ui, server = server)
# Run the app
shinyApp(
  ui = ui,
  server = server,
  options = list(
    host = "0.0.0.0",  # Listen on all network interfaces
    port = 3838         # The port to be used (change to 3839 if needed)
  )
)


