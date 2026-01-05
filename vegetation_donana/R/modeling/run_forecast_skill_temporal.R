# Function to assess forecast skill of the temporal model
# This is a generalized script that calls all output files in a folder & goes through each file to create a forecast skill output
forecast_skill_temp <- function(type,prediction_file,folder_file) {
  # Which type of forecast skill (for now MSE)
  type <-  "MSE"
 
  # Loop through and read each file
  # And calculate the MSE
  
  skill.temporal <- NULL
  
  pred <- read.csv(paste0(folder_file, "/", prediction_file))
  
  species <- unique(obs$species)
  
  for(a in 1:length(species)){
    
    pred_sub <- pred[pred$species%in%species[a],]
    
    time <- unique(obs$year[obs$year%in%unique(year(pred_sub$datetime))])
    
    for(b in 1:length(time)){
      
      pred_sub_time <-  pred_sub[grep(time[b],pred_sub$datetime),]
      
      # Get observed totals (summed across different sites)
      obs.total <- sum(obs$adults[obs$species==species[a] &
                                    obs$year == time[b]], na.rm=TRUE)
      
      
      pred.total <- aggregate(prediction~project_id +model_id +forecast_type +datetime+          
                                reference_datetime+ duration+ family +parameter_draw +variable + species, sum, data= pred_sub_time)
      
      pred.total$skill <- (obs.total - pred.total$prediction)^2
      
      pred.total$type.skill <- type
      
      skill.temporal <- rbind(skill.temporal,pred.total)
      
    }
    
  }
  
return(skill.temporal)
}
