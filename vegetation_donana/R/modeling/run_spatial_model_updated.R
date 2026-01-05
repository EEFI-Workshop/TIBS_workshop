run_spatial_model <- function(ndvi_metric,year_before_pred,lag) {
  # Load and process NDVI data
  df.ndvi <- read.csv(here("data","processed","ndvi_metrics_updated.csv"))
  df.ndvi <- df.ndvi[,c("ID", "year", ndvi_metric)]
  
  # Load abundance data
  num <- read.csv(here("data","raw","shrub_abundance_since2007_18_plots.csv"))
  num <- num[num$year<=year_before_pred,]
  
  # Add satellite derived measure to abundance data 
  df.ndvi.mu <- df.ndvi[df.ndvi$year>1999&df.ndvi$year<year_before_pred,] # subset to the training years 
  colnames(df.ndvi.mu)[1] <- "plot"
  df.ndvi.mu$year <- as.numeric(df.ndvi.mu$year) + lag
  df.ndvi.mu$cov <- as.numeric(scale(df.ndvi.mu[, ndvi_metric]))
  
  # Focus on four abundant shrubs
  sub <- num[num$species %in% c("Halimium halimifolium", "Lavandula stoechas","Rosmarinus officinalis","Cistus libanotis"),]
  
  n.plots <- length(unique(num$plot))
  n.years <- length(2007:max(num$year))
  ### Halimium
  C.hal <- array(NA, c(n.plots, n.years))
  cov.hal <- array(NA, c(n.plots, n.years))
  
  hal <- sub[sub$species %in% "Halimium halimifolium",]
  hal$saplings[is.na(hal$saplings)] <- 0
  hal$adults[is.na(hal$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:max(num$year))[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(hal[hal$plot %in% plot & hal$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.hal[i,k] <- sum
      if(length(cov) > 0) cov.hal[i,k] <- cov
    }
  }
  
  C.hal[5,16] <- 0
  
  ## Lavandula
  C.lav <- array(NA, c(n.plots, n.years))
  cov.lav <- array(NA, c(n.plots, n.years))
  
  lav <- sub[sub$species %in% "Lavandula stoechas",]
  lav$saplings[is.na(lav$saplings)] <- 0
  lav$adults[is.na(lav$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:max(num$year))[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(lav[lav$plot %in% plot & lav$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.lav[i,k] <- sum
      if(length(cov) > 0) cov.lav[i,k] <- cov
    }
  }
  
  C.lav[5,16] <- 0
  
  ### Rosemary
  C.ros <- array(NA, c(n.plots, n.years))
  cov.ros <- array(NA, c(n.plots, n.years))
  
  ros <- sub[sub$species %in% "Rosmarinus officinalis",]
  ros$saplings[is.na(ros$saplings)] <- 0
  ros$adults[is.na(ros$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:max(num$year))[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(ros[ros$plot %in% plot & ros$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.ros[i,k] <- sum
      if(length(cov) > 0) cov.ros[i,k] <- cov
    }
  }
  
  C.ros[5,16] <- 0
  
  ### Cistus libanotis
  C.cis <- array(NA, c(n.plots, n.years))
  cov.cis <- array(NA, c(n.plots, n.years))
  
  cis <- sub[sub$species %in% "Cistus libanotis",]
  cis$saplings[is.na(cis$saplings)] <- 0
  cis$adults[is.na(cis$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:max(num$year))[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(cis[cis$plot %in% plot & cis$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.cis[i,k] <- sum
      if(length(cov) > 0) cov.cis[i,k] <- cov
    }
  }
  
  C.cis[6,16] <- 0
  
  
  # Get observed for prediction start
  start.fut=as.character(year_before_pred)
  
  species_fut <- aggregate(adults~plot+year+species,sub,mean, drop=F)
  
  hal_fut <-  species_fut[species_fut$year == start.fut&species_fut$species%in%"Halimium halimifolium",]
  
  lav_fut <-species_fut[species_fut$year == start.fut&species_fut$species%in%"Lavandula stoechas",]
  
  
  ros_fut <-  species_fut[species_fut$year == start.fut&species_fut$species%in%"Rosmarinus officinalis",]
  
 
  cis_fut <-  species_fut[species_fut$year == start.fut&species_fut$species%in% "Cistus libanotis",]
  
  
  #### Bundle data for NIMBLE model
  
  # Data preparation (same as JAGS)
  bdata <- list(n.hal = C.hal, C.hal = C.hal,
                n.lav = C.lav, C.lav = C.lav,
                n.ros = C.ros, C.ros = C.ros,
                n.cis = C.cis, C.cis = C.cis,
                nsites = dim(C.hal)[1],
                cov.hal = cov.hal,
                cov.lav = cov.lav,
                cov.ros = cov.ros,
                cov.cis = cov.cis,
                nyears = dim(C.hal)[2]
  )
  
  # Define model in NIMBLE code
  abundanceCode <- nimbleCode({
    # Priors
    a0.h ~ dnorm(0, sd = 316.23)  # 1/sqrt(1.0E-05) = 316.23
    a1.h ~ dnorm(0, sd = 316.23) 
    a2.h ~ dnorm(0, sd = 316.23)
    
    a0.l ~ dnorm(0, sd = 316.23)
    a1.l ~ dnorm(0, sd = 316.23) 
    a2.l ~ dnorm(0, sd = 316.23)
    
    a0.r ~ dnorm(0, sd = 316.23)
    a1.r ~ dnorm(0, sd = 316.23) 
    a2.r ~ dnorm(0, sd = 316.23)
    
    a0.c ~ dnorm(0, sd = 316.23)
    a1.c ~ dnorm(0, sd = 316.23) 
    a2.c ~ dnorm(0, sd = 316.23)
    
    for(i in 1:nsites) { # Loop over sites
      #Initial abundance
      N.h[i,1] <- C.hal[i,1]
      N.l[i,1] <- C.lav[i,1]
      
      N.r[i,1] <- C.ros[i,1]
      N.c[i,1] <- C.cis[i,1]
      
      #Specify the model for years 2 through nYears
      for(t in 1:(nyears-1)) {
        # Halimium halimifolium
        n.hal[i,t+1] ~ dpois(N.h[i,t+1])
        log(N.h[i,t+1]) <- a0.h + a1.h * cov.hal[i,t] + a2.h * log(N.h[i,t] + 0.001) 
        
        # Lavandula stoechas 
        n.lav[i,t+1] ~ dpois(N.l[i,t+1])
        log(N.l[i,t+1]) <- a0.l + a1.l * cov.lav[i,t] + a2.l * log(N.l[i,t] + 0.001) 
        
        # Rosmarinus officinalis 
        n.ros[i,t+1] ~ dpois(N.r[i,t+1])
        log(N.r[i,t+1]) <- a0.r + a1.r * cov.ros[i,t] + a2.r * log(N.r[i,t] + 0.001)
        
        # Cistus libanotis 
        n.cis[i,t+1] ~ dpois(N.c[i,t+1])
        log(N.c[i,t+1]) <- a0.c + a1.c * cov.cis[i,t] + a2.c * log(N.c[i,t] + 0.001)
        
        
        
        
      }
    }
    
    
  })
  
  # Set up initial values
  inits <- list(a0.h = rnorm(1, 0, 0.01),
                a1.h = rnorm(1, 0, 0.01),
                a2.h = rnorm(1, 0, 0.01),
                a0.l = rnorm(1, 0, 0.01),
                a1.l = rnorm(1, 0, 0.01),
                a2.l = rnorm(1, 0, 0.01),
                a0.r = rnorm(1, 0, 0.01),
                a1.r = rnorm(1, 0, 0.01),
                a2.r = rnorm(1, 0, 0.01),
                a0.c = rnorm(1, 0, 0.01),
                a1.c = rnorm(1, 0, 0.01),
                a2.c = rnorm(1, 0, 0.01))
  
  # Run MCMC
  nc <- 3  # number of chains
  ni <- 200000  # number of iterations
  nb <- 50000  # burn-in
  nt <- 100     # thinning
  
  out1 <- nimbleMCMC(code = abundanceCode,
                     data = bdata,
                     constants = list(nsites = bdata$nsites, nyears = bdata$nyears),
                     inits = inits,
                     monitors = c("a0.h", "a1.h", "a2.h",
                                  "a0.l", "a1.l", "a2.l",
                                  "a0.r", "a1.r", "a2.r",
                                  "a0.c", "a1.c", "a2.c"),
                     niter = ni,
                     nburnin = nb,
                     thin = nt,
                     nchains = nc,
                     setSeed = TRUE,
                     summary = TRUE,
                     WAIC = TRUE)
  
  # Convert to mcmc.list
  
  out1_mcmc <- as.mcmc.list(lapply(out1$samples, as.mcmc))

  out1_mcmc_pooled <- do.call(rbind, lapply(out1_mcmc, as.data.frame))
  
  if(any(gelman.diag(out1_mcmc)$psrf[,2]>1.02)) print("Warning: Rhat values suggest convergence issues")
  
  # Return only what we need for spatial predictions
  return(list(
    mcmc = out1_mcmc_pooled, #for simplicity just return the first model output if chains converge
    observed_fut = list(
      halimium = hal_fut$adults,
      lavandula = lav_fut$adults,
      rosmarinus = ros_fut$adults,
      cistus= cis_fut$adults
    ),
    mean.ndvi.metric=mean(df.ndvi.mu[, ndvi_metric]),
    sd.ndvi.metric=sd(df.ndvi.mu[, ndvi_metric])
  ))
}


