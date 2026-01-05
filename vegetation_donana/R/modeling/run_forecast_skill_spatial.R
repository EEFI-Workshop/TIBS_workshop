# Function to assess forecast skill of the spatial (or spatiotemporal) model
# This is a generalized script that calls all output files in a folder & goes through each file to create a forecast skill output

forecast_skill_spatial <- function(type,prediction_file,validation_file,folder_file_pred,folder_file_valid) {
  # Which type of forecast skill (for now MSE)
  type <-  "MSE"
 
  # Loop through and read each file
  # And calculate the MSE
  
  skill.spatial <- NULL
  
  pred <- read.csv(paste0(folder_file_pred, "/", prediction_file))

  coords_pred <- pred[!duplicated(pred$cell_id),]
  
  # Create spatial vector
  pred_sp <- vect(coords_pred, geom = c("cell_lon", "cell_lat"), crs = "EPSG:32629")
  
  # Load validation data 
  
  validation <- read.csv(paste0(folder_file_valid, "/", validation_file))
  
  # Create spatial vector (assuming WGS84 initially, adjust CRS as needed)
  obs_sp <- vect(validation, geom = c("Long", "Lat"), crs = "EPSG:4326")

  # Reproject observation to match prediction projection
  obs_sp_reprojected <- project(obs_sp, crs(pred_sp))
  
  # Find nearest neighbor for each point in observational data
  nearest_idx <- nearest(obs_sp_reprojected, pred_sp)
  
  # Extract nearest neighbor data 
  pred_sp_df <- as.data.frame(pred_sp)
  
  nearest_neighbor_data <- pred_sp_df[nearest_idx$to_id, ] # !!! NEED TO DOUBLE CHECK THIS
  
  # Convert validation from wide to long 
  obs <- reshape(
    validation,
    varying = c("Hal", "Lav", "Ros", "Cis"),
    v.names = "Abundance",
    timevar = "species",
    times =  c("Hal", "Lav", "Ros", "Cis"),
    direction = "long",
    idvar = c("ID", "Elevation")
  )
  
  obs$species <- factor(obs$species)
  
  levels(obs$species) <- c("Cistus libanotis","Halimium halimifolium","Lavandula stoechas","Rosmarinus officinalis")
  
  species <- levels(obs$species) # obs is already loaded in workflow.R
  
  for(a in 1:length(species)){
    
    pred_sub <- pred[pred$species%in%species[a]&pred$cell_id%in%nearest_neighbor_data$cell_id,]
    
    for(b in 1:length(unique(pred_sub$uncertainty_component))){
      
      pred_sub_unc <- pred_sub[pred_sub$uncertainty_component==unique(pred_sub$uncertainty_component)[b],]
      
      
      pred_sub_unc$skill <- (obs$Abundance[obs$species%in%species[a]]-pred_sub_unc$prediction)^2
      
    
      pred_sub_unc$type.skill <- type
      
      skill.spatial <- rbind(skill.spatial,pred_sub_unc)
      
    }
    
  }
  
return(skill.spatial)
}
