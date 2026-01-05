run_spatial_predictions <- function(ndvi_metric,year_before_pred, lag) {
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
  overlapping_cells <- terra::extract(ndvi_raster, eco.pr.sub,cells=T)
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
  par.sub <- sample(1:length(temporal_model$mcmc$a0.h), 100)
  
  # Function to make predictions for one cell or vector of cells
  predict_cell <- function(ndvi_values, species = "halimium") {
    predictions <- rep(NA, length(ndvi_values))  # Initialize output vector
    
    # Skip if all values are NA
    if(all(is.na(ndvi_values))) return(predictions)
    
    # Process only non-NA values
    valid_idx <- !is.na(ndvi_values)
    
    if (species == "halimium") {
      # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.h <- temporal_model$mcmc$a0.h[par.sub]
      a1.h <- temporal_model$mcmc$a1.h[par.sub]
      a2.h <- temporal_model$mcmc$a2.h[par.sub]
      
      # Make predictions for valid cells
      valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
        # Make predictions for each MCMC iteration
        
        ### 
        N.h.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-landscape_model$mean.ndvi.metric.land)/landscape_model$sd.ndvi.metric.land),
                               spp="Halimium halimifolium",
                               year="2024"),type="response")

        #print(N.h.initial)
        cell_predictions <- sapply(1:length(par.sub), function(i) {
          # Calculate components
          ndvi_component <- a1.h[i] * (ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
          density_component <- a2.h[i] * log(N.h.initial + 0.001)
          log_expected <- a0.h[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        median(cell_predictions)
      })
      
      predictions[valid_idx] <- valid_predictions
      return(predictions)
      
    } else 
      
      if (species == "lavandula") {
      # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.l <- temporal_model$mcmc$a0.l[par.sub]
      a1.l <- temporal_model$mcmc$a1.l[par.sub]
      a2.l <- temporal_model$mcmc$a2.l[par.sub]
      
      # Make predictions for valid cells
      valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
        
        N.l.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-landscape_model$mean.ndvi.metric.land)/landscape_model$sd.ndvi.metric.land),
                                                                         spp="Lavandula stoechas",
                                                                         year="2024"),type="response")
        
        # Make predictions for each MCMC iteration
        cell_predictions <- sapply(1:length(par.sub), function(i) {
          # Calculate components
          ndvi_component <- a1.l[i] * (ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
          density_component <- a2.l[i] * log(N.l.initial + 0.001)
          log_expected <- a0.l[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        median(cell_predictions)
      })
      
      predictions[valid_idx] <- valid_predictions
      return(predictions)
      } else if (species == "rosmarinus") {
          # Use observed 2023 abundance as initial density
          
          # Get MCMC parameters
          a0.r <- temporal_model$mcmc$a0.r[par.sub]
          a1.r <- temporal_model$mcmc$a1.r[par.sub]
          a2.r <- temporal_model$mcmc$a2.r[par.sub]
          
          # Make predictions for valid cells
          valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
            
            N.h.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric),
                                                                             spp="Halimium halimifolium",
                                                                             year="2024"),type="response")
            N.r.initial <- predict(landscape_model$m_ros_pre,newdata=data.frame(Hal=N.h.initial,
                                                                year="2022"))
            # Make predictions for each MCMC iteration
            cell_predictions <- sapply(1:length(par.sub), function(i) {
              # Calculate components
              ndvi_component <- a1.r[i] * (ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
              density_component <- a2.r[i] * log(N.r.initial + 0.001)
              log_expected <- a0.r[i] + ndvi_component + density_component
              expected_abundance <- exp(log_expected)
            })
            
            median(cell_predictions)
          })
          
          predictions[valid_idx] <- valid_predictions
          return(predictions)
      } else {
        # Use observed 2023 abundance as initial density
        
        # Get MCMC parameters
        a0.c <- temporal_model$mcmc$a0.r[par.sub]
        a1.c <- temporal_model$mcmc$a1.r[par.sub]
        a2.c <- temporal_model$mcmc$a2.r[par.sub]
        
        # Make predictions for valid cells
        valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
          
          N.l.initial <- predict(landscape_model$m_land,newdata=data.frame(cov=((ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric),
                                                                           spp="Lavandula stoechas",
                                                                           year="2024"),type="response")
          N.c.initial <- predict(landscape_model$m_cis_pre,newdata=data.frame(Lav=N.l.initial,
                                                              year="2022"))
          # Make predictions for each MCMC iteration
          cell_predictions <- sapply(1:length(par.sub), function(i) {
            # Calculate components
            ndvi_component <- a1.c[i] * (ndvi_value-temporal_model$mean.ndvi.metric)/temporal_model$sd.ndvi.metric
            density_component <- a2.c[i] * log(N.c.initial + 0.001)
            log_expected <- a0.c[i] + ndvi_component + density_component
            expected_abundance <- exp(log_expected)
          })
          
          median(cell_predictions)
        })
        
        predictions[valid_idx] <- valid_predictions
        return(predictions)
      }
  }
  
  # Get all raster values at once
  hal_predictions <- predict_cell(terra::values(ndvi.sub), "halimium")
  lav_predictions <- predict_cell(terra::values(ndvi.sub), "lavandula")
  ros_predictions <- predict_cell(terra::values(ndvi.sub), "rosmarinus")
  cis_predictions <- predict_cell(terra::values(ndvi.sub), "cistus")
  
  # Assign predictions back to rasters
  
  terra::values(pred_raster_hal) <- hal_predictions
  terra::values(pred_raster_lav) <- lav_predictions
  terra::values(pred_raster_ros) <- ros_predictions
  terra::values(pred_raster_cis) <- cis_predictions
  
  # Save predictions (FOR NOW COMMENTED)
  if (!dir.exists(here("data","results","spatial_predictions"))) {
    dir.create(here("data","results","spatial_predictions"), recursive = TRUE)
  }

  writeRaster(pred_raster_hal,
              filename = here("data","results","spatial_predictions",
                              sprintf("halimium_%s.tif", ndvi_metric)),
              overwrite = TRUE)

  writeRaster(pred_raster_lav,
              filename = here("data","results","spatial_predictions",
                              sprintf("lavandula_%s.tif", ndvi_metric)),
              overwrite = TRUE)

  writeRaster(pred_raster_ros,
              filename = here("data","results","spatial_predictions",
                              sprintf("rosmarinus_%s.tif", ndvi_metric)),
              overwrite = TRUE)

  writeRaster(pred_raster_cis,
              filename = here("data","results","spatial_predictions",
                              sprintf("cistus_%s.tif", ndvi_metric)),
              overwrite = TRUE)

  # writeRaster(pred_raster_hal,filename ="/Users/maria/Dropbox/collaborations/EEFI/workshop/plots/test_pred_hal.tif",overwrite = TRUE)
  # writeRaster(pred_raster_lav,filename ="/Users/maria/Dropbox/collaborations/EEFI/workshop/plots/test_pred_lav.tif",overwrite = TRUE)
  # writeRaster(pred_raster_ros,filename ="/Users/maria/Dropbox/collaborations/EEFI/workshop/plots/test_pred_ros.tif",overwrite = TRUE)
  # writeRaster(pred_raster_cis,filename ="/Users/maria/Dropbox/collaborations/EEFI/workshop/plots/test_pred_cis.tif",overwrite = TRUE)

  # Return the prediction rasters
  return(list(
    halimium = pred_raster_hal,
    lavandula = pred_raster_lav,
    rosmarinus = pred_raster_ros,
    cistus = pred_raster_cis
  ))
}
