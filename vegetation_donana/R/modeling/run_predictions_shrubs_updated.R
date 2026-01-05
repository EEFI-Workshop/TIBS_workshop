# Function to run model for a specific parameter combination
run_model_combination <- function(ndvi_metric, scenario, bioclim, model,year_before_pred,lag,n.years.pred) {
  # Store parameters
  params <- list(
    ndvi_metric = ndvi_metric,
    scenario = scenario,
    bioclim = bioclim,
    model = model
  )
  
  # Load and process NDVI data
  df.ndvi <- read.csv(here("data","processed","ndvi_metrics_updated.csv"))
  df.ndvi <- df.ndvi[,c("ID", "year", ndvi_metric)]
  colnames(df.ndvi) <- c("plot", "year", ndvi_metric)
  df.ndvi <- df.ndvi[df.ndvi$year<year_before_pred,]

    # SCALE DATA
  df.ndvi.scaled <- df.ndvi[df.ndvi$year>1999&df.ndvi$year<year_before_pred,] # subset to the training years 
  df.ndvi.scaled$year <- as.numeric(df.ndvi.scaled$year) + lag
  df.ndvi.scaled$cov <- as.numeric(scale(df.ndvi.scaled[, ndvi_metric]))
  
  mean.ndvi.metric=mean(df.ndvi.scaled[, ndvi_metric]) # has to be changed
  sd.ndvi.metric=sd(df.ndvi.scaled[, ndvi_metric]) # ha
  
  
  # Load NDVI predictions with specific parameters
  ndvi_predictions <- readRDS(here("data","results","ndvi_predictions","ndvi_predictions_plots.rds"))
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$metric == ndvi_metric, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$scenario == scenario, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$bioclim_vars == bioclim, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$model == model, ]
  ndvi_predictions <- ndvi_predictions[, c("plot", "year", "predicted")]
  colnames(ndvi_predictions) <- c("plot", "year", ndvi_metric)
  
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$year>=year_before_pred, ]
  
  ndvi_predictions$year <- as.numeric(ndvi_predictions$year) + lag
  
  ### SCALE PREDICTED VALUES (based on mean and sd of observed)
  ndvi_predictions$cov <- (ndvi_predictions[, ndvi_metric]-mean.ndvi.metric)/sd.ndvi.metric
  
 
  df.ndvi.all = rbind(df.ndvi.scaled, ndvi_predictions)
 
  # Load abundance data
  num <- read.csv(here("data","raw","shrub_abundance_since2007_18_plots.csv"))
  num_fut <- num[num$year>=year_before_pred,]
  num_fut_sub <- num_fut[num_fut$species %in% c("Halimium halimifolium", "Lavandula stoechas","Rosmarinus officinalis","Cistus libanotis"),]

  ############### 
  # FORECAST NEXT YEARS
  
  temporal_model <- run_spatial_model(ndvi_metric,year_before_pred,lag)
  
  # The abundances at the landscape level are already in the data frame ab_land
  
  # Predict NDVI based on weather (needs to be done)
  
  n.years.pred = n.years.pred  # Extend predictions to 4 years (2025-2028)
  n.plots=18
 # Extend NDVI predictions array
  ndvi.pred = array(NA, c(n.plots, n.years.pred))
  for(i in 1:n.plots){ # site loop
    for(k in 1:n.years.pred){
      year = as.character(seq(year_before_pred,year_before_pred+n.years.pred-1))[k]  # Extended years
      plot = unique(num_fut_sub$plot)[i]
      
      cov = df.ndvi.all$cov[df.ndvi.all$plot%in%plot&df.ndvi.all$year%in%year]
      
      if(length(cov)>0)  ndvi.pred[i,k] = cov
    }
  }
  
  
  # Sample posterior values
  
  par.sub = sample(1:length(temporal_model$mcmc$a0.c), 1000)
  
  # Arrays for predictions (4 years)
  n.hal.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  n.lav.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  n.ros.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  n.cis.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  
  
 # set NA to 0
  temporal_model$observed_fut$halimium[is.na(temporal_model$observed_fut$halimium)] <- 0
  temporal_model$observed_fut$lavandula[is.na(temporal_model$observed_fut$lavandula)] <- 0
  temporal_model$observed_fut$rosmarinus[is.na(temporal_model$observed_fut$rosmarinus)] <- 0
  temporal_model$observed_fut$cistus[is.na(temporal_model$observed_fut$cistus)] <- 0
  
  for(x in 1:length(par.sub)){
    for(i in 1:n.plots) {
      # Initial abundance
      N.h <- temporal_model$observed_fut$halimium[i]
      N.l <- temporal_model$observed_fut$lavandula[i]
      N.r <- temporal_model$observed_fut$rosmarinus[i]
      N.c <- temporal_model$observed_fut$cistus[i]
      
      # Loop for all predictions (4 years)
      for(t in 1:n.years.pred) {
        # Halimium predictions
        N.h <- exp(temporal_model$mcmc$a0.h[par.sub[x]] + 
                     temporal_model$mcmc$a1.h[par.sub[x]] * ndvi.pred[i,t] + 
                     temporal_model$mcmc$a2.h[par.sub[x]] * log(N.h+0.001))
        
        n.hal.pred[x,i,t] <- rpois(1,N.h)
        
        # Lavandula predictions
        N.l <- exp(temporal_model$mcmc$a0.l[par.sub[x]] + 
                     temporal_model$mcmc$a1.l[par.sub[x]] * ndvi.pred[i,t] + 
                     temporal_model$mcmc$a2.l[par.sub[x]] * log(N.l+0.001))
        
        n.lav.pred[x,i,t] <- rpois(1,N.l)
        
        # Rosmarinus predictions
        N.r <- exp(temporal_model$mcmc$a0.r[par.sub[x]] + 
                     temporal_model$mcmc$a1.r[par.sub[x]] * ndvi.pred[i,t] + 
                     temporal_model$mcmc$a2.r[par.sub[x]] * log(N.r+0.001))
        
        n.ros.pred[x,i,t] <- rpois(1,N.r)
        
        # Rosmarinus predictions
        N.c <- exp(temporal_model$mcmc$a0.c[par.sub[x]] + 
                     temporal_model$mcmc$a1.c[par.sub[x]] * ndvi.pred[i,t] + 
                     temporal_model$mcmc$a2.c[par.sub[x]] * log(N.c+0.001))
        
        n.cis.pred[x,i,t] <- rpois(1,N.c)
      }
    }
    
    
  }
  
  # Create final outputs csv in standardized format 
  
  # HALIMIUM 
  
  halimium_predictions <- array2DF(n.hal.pred)
  colnames(halimium_predictions) <- c("uncertainty_component","site_id","datetime","prediction")
  
  halimium_predictions$uncertainty_component <- as.numeric(factor(halimium_predictions$uncertainty_component))
  halimium_predictions$site_id=factor(halimium_predictions$site_id)
  levels(halimium_predictions$site_id) <- unique(num_fut$plot)
  halimium_predictions$site_id=as.character(halimium_predictions$site_id)
  
  halimium_predictions$datetime=factor(halimium_predictions$datetime)
  levels(halimium_predictions$datetime) <- paste(seq(year_before_pred+1,year_before_pred+n.years.pred),"-05-20 18:00:00",sep = "")
  halimium_predictions$datetime=as.character(halimium_predictions$datetime)
  
  halimium_predictions$project_id ="donana_forecast_V1"
  halimium_predictions$model_id ="nimble"
  halimium_predictions$forecast_type ="temporal"
  halimium_predictions$reference_datetime = paste(year_before_pred,"-05-20 18:00:00",sep = "")
  halimium_predictions$duration = "P1Y"
  halimium_predictions$species="Halimium halimifolium"
  halimium_predictions$family="sample"
  halimium_predictions$variable=paste("abundance",ndvi_metric,scenario,bioclim,model, sep="_")
  
  # ORDER column names
  halimium_predictions <- halimium_predictions[,c("project_id","model_id","forecast_type",
                                                  "datetime","reference_datetime","duration",
                                                  "site_id","species","family","uncertainty_component",
                                                  "variable","prediction")]
  

  # LAVANDULA
  
  lavandula_predictions <- array2DF(n.lav.pred)
  colnames(lavandula_predictions) <- c("uncertainty_component","site_id","datetime","prediction")
  
  lavandula_predictions$uncertainty_component <- as.numeric(factor(lavandula_predictions$uncertainty_component))
  lavandula_predictions$site_id=factor(lavandula_predictions$site_id)
  levels(lavandula_predictions$site_id) <- unique(num_fut$plot)
  lavandula_predictions$site_id=as.character(lavandula_predictions$site_id)
  
  lavandula_predictions$datetime=factor(lavandula_predictions$datetime)
  levels(lavandula_predictions$datetime) <- paste(seq(year_before_pred+1,year_before_pred+n.years.pred),"-05-20 18:00:00",sep = "")
  lavandula_predictions$datetime=as.character(lavandula_predictions$datetime)
  
  lavandula_predictions$project_id ="donana_forecast_V1"
  lavandula_predictions$model_id ="nimble"
  lavandula_predictions$forecast_type ="temporal"
  lavandula_predictions$reference_datetime = paste(year_before_pred,"-05-20 18:00:00",sep = "")
  lavandula_predictions$duration = "P1Y"
  lavandula_predictions$species="Lavandula stoechas"
  lavandula_predictions$family="sample"
  lavandula_predictions$variable=paste("abundance",ndvi_metric,scenario,bioclim,model, sep="_")
  
  # ORDER column names
  lavandula_predictions <- lavandula_predictions[,c("project_id","model_id","forecast_type",
                                                  "datetime","reference_datetime","duration",
                                                  "site_id","species","family","uncertainty_component",
                                                  "variable","prediction")]
  
  
  # ROSMARINUS
  
  rosmarinus_predictions <- array2DF(n.ros.pred)
  colnames(rosmarinus_predictions) <- c("uncertainty_component","site_id","datetime","prediction")
  
  rosmarinus_predictions$uncertainty_component <- as.numeric(factor(rosmarinus_predictions$uncertainty_component))
  rosmarinus_predictions$site_id=factor(rosmarinus_predictions$site_id)
  levels(rosmarinus_predictions$site_id) <- unique(num_fut$plot)
  rosmarinus_predictions$site_id=as.character(rosmarinus_predictions$site_id)
  
  rosmarinus_predictions$datetime=factor(rosmarinus_predictions$datetime)
  levels(rosmarinus_predictions$datetime) <- paste(seq(year_before_pred+1,year_before_pred+n.years.pred),"-05-20 18:00:00",sep = "")
  rosmarinus_predictions$datetime=as.character(rosmarinus_predictions$datetime)
  
  rosmarinus_predictions$project_id ="donana_forecast_V1"
  rosmarinus_predictions$model_id ="nimble"
  rosmarinus_predictions$forecast_type ="temporal"
  rosmarinus_predictions$reference_datetime = paste(year_before_pred,"-05-20 18:00:00",sep = "")
  rosmarinus_predictions$duration = "P1Y"
  rosmarinus_predictions$species="Rosmarinus officinalis"
  rosmarinus_predictions$family="sample"
  rosmarinus_predictions$variable=paste("abundance",ndvi_metric,scenario,bioclim,model, sep="_")
  
  # ORDER column names
  rosmarinus_predictions <- rosmarinus_predictions[,c("project_id","model_id","forecast_type",
                                                  "datetime","reference_datetime","duration",
                                                  "site_id","species","family","uncertainty_component",
                                                  "variable","prediction")]
  
  
  # CISTUS 
  
  cistus_predictions <- array2DF(n.cis.pred)
  colnames(cistus_predictions) <- c("uncertainty_component","site_id","datetime","prediction")
  
  cistus_predictions$uncertainty_component <- as.numeric(factor(cistus_predictions$uncertainty_component))
  cistus_predictions$site_id=factor(cistus_predictions$site_id)
  levels(cistus_predictions$site_id) <- unique(num_fut$plot)
  cistus_predictions$site_id=as.character(cistus_predictions$site_id)
  
  cistus_predictions$datetime=factor(cistus_predictions$datetime)
  levels(cistus_predictions$datetime) <- paste(seq(year_before_pred+1,year_before_pred+n.years.pred),"-05-20 18:00:00",sep = "")
  cistus_predictions$datetime=as.character(cistus_predictions$datetime)
  
  cistus_predictions$project_id ="donana_forecast_V1"
  cistus_predictions$model_id ="nimble"
  cistus_predictions$forecast_type ="temporal"
  cistus_predictions$reference_datetime = paste(year_before_pred,"-05-20 18:00:00",sep = "")
  cistus_predictions$duration = "P1Y"
  cistus_predictions$species="Cistus libanotis"
  cistus_predictions$family="sample"
  cistus_predictions$variable=paste("abundance",ndvi_metric,scenario,bioclim,model, sep="_")
  
  # ORDER column names
  cistus_predictions <- cistus_predictions[,c("project_id","model_id","forecast_type",
                                                  "datetime","reference_datetime","duration",
                                                  "site_id","species","family","uncertainty_component",
                                                  "variable","prediction")]
  
  
  # Create results list
  results <- list(
    parameters = params,
    predictions = list(
      halimium = halimium_predictions,
      lavandula = lavandula_predictions,
      rosmarinus = rosmarinus_predictions,
      cistus = cistus_predictions
    )
  )
}
