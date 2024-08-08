#Parameters for climate change in SORTIE

rm(list=ls())

library(tidyverse)

#Temperature
temp_max<- read_csv("climate_ferld/temp_max/candcs_u6_subset_grid_point_dataset_48_522_79_377.nc_365_day.csv")
temp_min<- read_csv("climate_ferld/temp_min/candcs_u6_subset_grid_point_dataset_48_528_79_382.nc_365_day.csv")


temp_max <- temp_max %>%
  mutate(
    ssp126 = rowMeans(select(., contains("ssp126")), na.rm = TRUE),
    ssp245 = rowMeans(select(., contains("ssp245")), na.rm = TRUE),
    ssp585 = rowMeans(select(., contains("ssp585")), na.rm = TRUE)) %>%
  select(time, ssp126, ssp245, ssp585) %>% 
  pivot_longer(cols=ssp126:ssp585, names_to = "ssp", values_to = "temp_max") %>% 
  filter(month(time) %in% 6:8)

temp_min <- temp_min %>%
  mutate(
    ssp126 = rowMeans(select(., contains("ssp126")), na.rm = TRUE),
    ssp245 = rowMeans(select(., contains("ssp245")), na.rm = TRUE),
    ssp585 = rowMeans(select(., contains("ssp585")), na.rm = TRUE)) %>%
  select(time, ssp126, ssp245, ssp585) %>% 
  pivot_longer(cols=ssp126:ssp585, names_to = "ssp", values_to = "temp_min") %>% 
  filter(month(time) %in% 6:8)


temp <- temp_min %>% mutate(temp_max=temp_max$temp_max,
                            tave=(temp_min+temp_max)/2) %>%
  group_by(year(time),ssp) %>% 
  summarize(tave=mean(tave)) %>% 
  rename(year=`year(time)`)


#Precipitations
prec<- read_csv("climate_ferld/prec/candcs_u6_subset_grid_point_dataset_48_522_79_377.nc_365_day.csv")

prec <- prec %>%
  mutate(
    ssp126 = rowMeans(select(., contains("ssp126")), na.rm = TRUE),
    ssp245 = rowMeans(select(., contains("ssp245")), na.rm = TRUE),
    ssp585 = rowMeans(select(., contains("ssp585")), na.rm = TRUE)) %>%
  select(time, ssp126, ssp245, ssp585) %>% 
  pivot_longer(cols=ssp126:ssp585, names_to = "ssp", values_to = "precipitation") %>% 
  filter(month(time) %in% 6:8) %>% 
  group_by(year(time), month(time), ssp) %>% 
  summarize(m_prec=sum(precipitation)) %>% 
  rename(year=`year(time)`) %>% 
  ungroup() %>% 
  group_by(year, ssp) %>% 
  summarize(MSP=mean(m_prec))

#Keep same scaling on climate variable with the growth data
coef_scale_clim <- readRDS("coef_scale_clim.rds")

climate_ferld <- right_join(temp, prec) %>% 
  filter(year>1990) %>% 
  mutate(tave=(tave-coef_scale_clim$mean_tave)/coef_scale_clim$sd_tave, 
         MSP=(MSP-coef_scale_clim$mean_MSP)/coef_scale_clim$sd_MSP) %>% 
  pivot_longer(cols=`tave`:`MSP`, names_to="climate_variable", values_to = "values") %>% 
  mutate(year=year-1990)


res <- NULL

fit=function(p){ #Function to minimize
  ypred=p[1]+p[2]*data$year^p[3]
  err=sum((data$values-ypred)^2)
  return(err)
}



for(sspi in unique(climate_ferld$ssp)){
  print(sspi)
  for(climate in unique(climate_ferld$climate_variable )){
    print(climate)
    data <- climate_ferld %>% 
      filter(ssp==sspi,
             climate_variable==climate)
    
    opti=optim(par=c(0,1,0.2),fit, method = "Nelder-Mead")$par

    
    res <- res %>% 
      bind_rows(data.frame(ssp=sspi, climate_variable=climate, 
               T1=opti[1], B=opti[2], C=opti[3]))

  }
}

#We need climate normals to have simulation without climate change
climate_ferld_noCC <- right_join(temp, prec) %>% 
  filter(year<1990, ssp=="ssp126") %>% 
  mutate(tave=(tave-coef_scale_clim$mean_tave)/coef_scale_clim$sd_tave, 
         MSP=(MSP-coef_scale_clim$mean_MSP)/coef_scale_clim$sd_MSP) %>% 
  select(-ssp) %>% 
  mutate(ssp="noCC") %>% 
  group_by(ssp) %>% 
  summarize(tave=mean(tave),
            MSP=mean(MSP))

res <- res %>% bind_rows(data.frame(ssp=climate_ferld_noCC$ssp, 
                                    climate_variable=c("tave", "MSP"), 
                                    T1=c(climate_ferld_noCC$tave, climate_ferld_noCC$MSP),
                                    B=0,
                                    C=1))





saveRDS(res, "coef_FERLD_climate.rds")



