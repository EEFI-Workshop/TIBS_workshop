run_spatial_predictions_csv <- function(ndvi_metric,year_before_pred) {
  # Run model to get parameters
  temporal_model <- run_spatial_model(ndvi_metric,year_before_pred,lag)
  
  ndvi_raster <- rast(here("data","ndvi_rasters","ndvi_metrics_rasters", paste0(year_before_pred,"/",ndvi_metric, "_",year_before_pred, ".tif")))
  
  # add the ecoregion polygons 
  
  eco <- vect(here("data","spatial","maps","siose_DN.shp"))
  
  # Project polygon to be on the same projection as the raster
  eco.pr <- project(eco,ndvi_raster)
  
  # Subset to suitable habitats
  sut.hab <- c(330,340)
  
  eco.pr.sub <- eco.pr[eco.pr$CODIIGE %in% sut.hab, ]
  
  # Extract values with coordinates
  overlapping_cells <- extract(ndvi_raster, eco.pr.sub,cells=T)
  all_cells <- 1:ncell(ndvi_raster)
  non_overlapping_cells <- setdiff(all_cells, overlapping_cells$cell)
  
  ndvi.sub <- ndvi_raster
  ndvi.sub[non_overlapping_cells] <- NA
  
  
  #### GET INITIAL ABUNDACES FROM LANDSCAPE PLOTS
  landscape_model <- init_abund_pred(ndvi_metric,year_before_pred)
  
  # Create empty rasters for predictions
  pred_raster_hal <- ndvi.sub
  pred_raster_lav <- ndvi.sub
  pred_raster_ros <- ndvi.sub
  pred_raster_cis <- ndvi.sub
  
  # Get model parameters from temporal run
  par.sub <- sample(1:length(temporal_model$mcmc$a0.h), 10) # here, just 10 instead of 100 because otherwise the output file will be huge
  
  # Function to make predictions for one cell or vector of cells
  predict_cell <- function(ndvi_raster, species) {
    
    # Extract values and coordinates
    ndvi_values <- terra::values(ndvi_raster, mat=FALSE)  # Get as vector
    coords <- crds(ndvi_raster, na.rm=FALSE)  # Get all coordinates
    
  
    # Skip if all values are NA
    if(all(is.na(ndvi_values))) return(NA)
    
    # Process only non-NA values
    valid_idx <- !is.na(ndvi_values)
    n_valid <- sum(valid_idx)
    n_par <- length(par.sub)
    
    # Extract valid coordinates
    valid_coords <- coords[valid_idx, , drop=FALSE]
    
    # Initialize output data frame with proper dimensions
    predictions <- data.frame(
      cell_id = rep(which(valid_idx), each = n_par),
      cell_lon = rep(valid_coords[, 1], each = n_par),
      cell_lat = rep(valid_coords[, 2], each = n_par),
      uncertainty_component = rep(par.sub, times = n_valid),
      prediction = NA
    )
    
    
    if(species == "halimium"){
      
      predictions$species <- "Halimium halimifolium"
      # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.h <- temporal_model$mcmc$a0.h[par.sub]
      a1.h <- temporal_model$mcmc$a1.h[par.sub]
      a2.h <- temporal_model$mcmc$a2.h[par.sub]
      
      # Counter for filling predictions data frame
      row_idx <- 1
      
      # Make predictions for valid cells
      for(cell_idx in which(valid_idx)) {
        
        ndvi_value <- ndvi_values[cell_idx]
        
        # Get initial density prediction
        N.h.initial <- predict(landscape_model$m_land,
                               newdata = data.frame(
                                 cov = ((ndvi_value - landscape_model$mean.ndvi.metric.land) / 
                                          landscape_model$sd.ndvi.metric.land),
                                 spp = "Halimium halimifolium",
                                 year = "2024"),
                               type = "response")
        
        print(paste("iteration ", row_idx, "out of ",length(ndvi_values[valid_idx]), ":",N.h.initial))
        
        # Make predictions for each MCMC iteration (par.sub)
        cell_predictions <- sapply(1:n_par, function(i) {
          # Calculate components
          ndvi_component <- a1.h[i] * (ndvi_value - temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
          density_component <- a2.h[i] * log(N.h.initial + 0.001)
          log_expected <- a0.h[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        # Fill predictions data frame for this cell
        predictions$prediction[row_idx:(row_idx + n_par - 1)] <- cell_predictions
        row_idx <- row_idx + n_par
      }
      
      return(predictions)
      
    }else if(species == "lavandula"){
      
        predictions$species <- "Lavandula stoechas"
        
        # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.l <- temporal_model$mcmc$a0.l[par.sub]
      a1.l <- temporal_model$mcmc$a1.l[par.sub]
      a2.l <- temporal_model$mcmc$a2.l[par.sub]
      
      # Counter for filling predictions data frame
      row_idx <- 1
      
      # Make predictions for valid cells
      for(cell_idx in which(valid_idx)) {
        
        ndvi_value <- ndvi_values[cell_idx]
        
        # Get initial density prediction
        N.l.initial <- predict(landscape_model$m_land,
                               newdata = data.frame(
                                 cov = ((ndvi_value - landscape_model$mean.ndvi.metric.land) / 
                                          landscape_model$sd.ndvi.metric.land),
                                 spp = "Lavandula stoechas",
                                 year = "2024"),
                               type = "response")
        
        print(paste("iteration ", row_idx, "out of ",length(ndvi_values[valid_idx]), ":",N.l.initial))
        
        # Make predictions for each MCMC iteration (par.sub)
        cell_predictions <- sapply(1:n_par, function(i) {
          # Calculate components
          ndvi_component <- a1.l[i] * (ndvi_value - temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
          density_component <- a2.l[i] * log(N.l.initial + 0.001)
          log_expected <- a0.l[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        # Fill predictions data frame for this cell
        predictions$prediction[row_idx:(row_idx + n_par - 1)] <- cell_predictions
        row_idx <- row_idx + n_par
      }
      
      return(predictions)
      
      }else if(species == "rosmarinus"){
          
        predictions$species <- "Rosmarinus officinalis"
        
        # Use observed 2023 abundance as initial density
          
          # Get MCMC parameters
          a0.r <- temporal_model$mcmc$a0.r[par.sub]
          a1.r <- temporal_model$mcmc$a1.r[par.sub]
          a2.r <- temporal_model$mcmc$a2.r[par.sub]
          
          # Counter for filling predictions data frame
          row_idx <- 1
          
          # Make predictions for valid cells
          for(cell_idx in which(valid_idx)) {
            
            ndvi_value <- ndvi_values[cell_idx]
            
            # Get initial density prediction
            N.h.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric),
                                                                             spp="Halimium halimifolium",
                                                                             year="2024"),type="response")
            N.r.initial <- predict(landscape_model$m_ros_pre,newdata=data.frame(Hal=N.h.initial,
                                                                year="2022"),type="response")
            
            print(paste("iteration ", row_idx, "out of ",length(ndvi_values[valid_idx]), ":",N.r.initial))
            
            # Make predictions for each MCMC iteration (par.sub)
            cell_predictions <- sapply(1:n_par, function(i) {
              # Calculate components
              ndvi_component <- a1.r[i] * (ndvi_value - temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
              density_component <- a2.r[i] * log(N.r.initial + 0.001)
              log_expected <- a0.r[i] + ndvi_component + density_component
              expected_abundance <- exp(log_expected)
            })
            
            # Fill predictions data frame for this cell
            predictions$prediction[row_idx:(row_idx + n_par - 1)] <- cell_predictions
            row_idx <- row_idx + n_par
          }
          return(predictions)
          
      }else if(species == "cistus"){
        
        predictions$species <- "Cistus libanotis"
        
        # Use observed 2023 abundance as initial density
        
        # Get MCMC parameters
        a0.c <- temporal_model$mcmc$a0.c[par.sub]
        a1.c <- temporal_model$mcmc$a1.c[par.sub]
        a2.c <- temporal_model$mcmc$a2.c[par.sub]
        
        # Counter for filling predictions data frame
        row_idx <- 1
        
        # Make predictions for valid cells
        for(cell_idx in which(valid_idx)) {
          
          ndvi_value <- ndvi_values[cell_idx]
          
          N.l.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric),
                                                                           spp="Lavandula stoechas",
                                                                           year="2024"),type="response")
          N.c.initial <- predict(landscape_model$m_cis_pre,newdata=data.frame(Lav=N.l.initial,
                                                              year="2022"),type="response")
          
          
          print(paste("iteration ", row_idx, "out of ",length(ndvi_values[valid_idx]), ":",N.c.initial))
          
          # Make predictions for each MCMC iteration (par.sub)
          cell_predictions <- sapply(1:n_par, function(i) {
            # Calculate components
            ndvi_component <- a1.c[i] * (ndvi_value - temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
            density_component <- a2.c[i] * log(N.c.initial + 0.001)
            log_expected <- a0.c[i] + ndvi_component + density_component
            expected_abundance <- exp(log_expected)
          })
          
          # Fill predictions data frame for this cell
          predictions$prediction[row_idx:(row_idx + n_par - 1)] <- cell_predictions
          row_idx <- row_idx + n_par
        }
        
        return(predictions)
      }
    
    
  }
  
  # Predict raster values in a dataframe format
  hal_predictions <- predict_cell(ndvi.sub, "halimium")
  lav_predictions <- predict_cell(ndvi.sub, "lavandula")
  ros_predictions <- predict_cell(ndvi.sub, "rosmarinus")
  cis_predictions <- predict_cell(ndvi.sub, "cistus")
  
  # join species-specific predictions into one data frame
  
  spatial_pred <- rbind(hal_predictions,lav_predictions,ros_predictions,cis_predictions)
  
  # Assign all the other important column information to data
  
  spatial_pred$project_id ="donana_forecast_V1"
  spatial_pred$model_id ="gam"
  spatial_pred$forecast_type ="spatial"
  spatial_pred$reference_datetime = paste(year_before_pred,"-05-20 18:00:00",sep = "")
  spatial_pred$duration = "P1Y"
  spatial_pred$family="sample"
  spatial_pred$variable=paste("abundance",ndvi_metric, sep="_")
  
  return(spatial_pred)
}
