init_abund_pred <- function(ndvi_metric,year_before_pred){
  
  # Load NDVI for landscape level
   df.land.ndvi <- read.csv(here("ndvi_metrics_land_updated.csv"))
   
   df.ndvi <- df.land.ndvi[,c("lat" ,"lon", "adult.density","spp", "year", ndvi_metric)]
   
   df.ndvi.mu <- df.ndvi[df.ndvi$year==year_before_pred,] # subset to the training years 
   df.ndvi.mu$cov <- as.numeric(scale(df.ndvi.mu[, ndvi_metric]))
   
   #there is too much data because 2023 and 2024 records were treated as separate XY
   #so subset (taking advantage of the fact that order is the same)
   
   ab_land <- read.csv("coordinates_2023_02.csv")
   
   df.ndvi.mu <- df.ndvi.mu[ab_land$year==year_before_pred,]
   
   df.ndvi.mu$ID <- factor(paste(df.ndvi.mu$lon,df.ndvi.mu$lat))
  levels(df.ndvi.mu$ID) = 1:length(levels(df.ndvi.mu$ID))
   df.ndvi.mu$year=factor(df.ndvi.mu$year)
   df.ndvi.mu$adults=ceiling(df.ndvi.mu$adult.density)
   df.ndvi.mu$spp=factor(df.ndvi.mu$spp)
   
   m_land=gam(adults~ spp+s(cov,by=spp,k=3),family="poisson",data=df.ndvi.mu,method = 'REML')
   
   # get abundances of unknown shrubs based on correlations
   
   num <- read.csv("shrub_abundance_since2007_18_plots.csv")
   
   # create wide data frame required for modelling
   num.w=reshape(num[,c("plot", "year", "species", "adults")], timevar="species", idvar = c("plot","year"),v.names = "adults",direction="wide")
   num.w=num.w[,1:12]
   
   colnames(num.w) =c("plot", "year","Cis","Hal_c","Hal","Hel","Jun","Lav","Ros","Stau","Thy","Ulex")
   
   num.w$year=factor(num.w$year)
   
   num.w$Cis[is.na(num.w$Cis)]=0
   num.w$Hal[is.na(num.w$Hal)]=0
   num.w$Lav[is.na(num.w$Lav)]=0
   num.w$Ros[is.na(num.w$Ros)]=0
   
   m_ros_pre=gam(Ros ~ s(Hal)+s(year, bs = 're'),family="poisson",data=num.w,method = 'REML')
   m_cis_pre=gam(Cis ~ s(Lav)+s(year, bs = 're'),family="poisson",data=num.w,method = 'REML')
   
   return(list(
     m_land = m_land,
     m_ros_pre = m_ros_pre,
     m_cis_pre = m_cis_pre,
     mean.ndvi.metric.land=mean(df.ndvi.mu[, ndvi_metric]), # has to be changed
     sd.ndvi.metric.land=sd(df.ndvi.mu[, ndvi_metric]) # has to be changed 
   ))
  
}
