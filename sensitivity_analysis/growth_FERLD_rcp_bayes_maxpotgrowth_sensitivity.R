#Script that give the growth of the species with the climate of the ferld fort the sensitivity analisys of MaxPotGrowth:

rm(list=ls())
setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

library(tidyverse)
theme_set(theme_bw())
library(brms)
library(ggpubr)


ferld_growth <- readRDS(file="growth/ferld_growth_rcp_period.rds") %>% 
  filter(rcp=="rcp45") %>% 
  select(-low_growth_no_biais, -high_growth_no_biais)


parameter_df <- readRDS(file="../result_simulations/sensitivity/sensitivity_analysis_design_percent_maxptogrowth.rds") %>% 
  rename(species2=species)

res <- NULL

for(i in 1:nrow(parameter_df)){ #i=1
  sub <- do.call("rbind", replicate(36, parameter_df[i,], simplify = FALSE))
  sub2 <- cbind(sub, ferld_growth)
  
  sp <- unique(sub2$species2)
  
  if(sub2$crossing[1] == "no"){
    if(sp=="ERS"){
      sub2 <- mutate(sub2, growth_no_biais=case_when(
        species=="Sugar_Maple" ~ growth_no_biais * as.numeric(sens_value),
        TRUE ~ growth_no_biais
      ))
    }
    if(sp=="ERR"){
      sub2 <- mutate(sub2, growth_no_biais=case_when(
        species=="Red_Maple" ~ growth_no_biais * as.numeric(sens_value),
        TRUE ~ growth_no_biais
      ))
    }
    if(sp=="BOJ"){
      sub2 <- mutate(sub2, growth_no_biais=case_when(
        species=="Yellow_Birch" ~ growth_no_biais * as.numeric(sens_value),
        TRUE ~ growth_no_biais
      ))
    }
  }else{
    sp2 <- sub2$crossing[1]
    
    if(sp=="ERS" & sp2=="ERR"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Sugar_Maple",8] <- sub3_period[sub3_period$species=="Red_Maple",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    if(sp=="ERS" & sp2=="BOJ"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Sugar_Maple",8] <- sub3_period[sub3_period$species=="Yellow_Birch",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    if(sp=="ERR" & sp2=="ERS"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Red_Maple",8] <- sub3_period[sub3_period$species=="Sugar_Maple",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    if(sp=="ERR" & sp2=="BOJ"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Red_Maple",8] <- sub3_period[sub3_period$species=="Yellow_Birch",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    if(sp=="BOJ" & sp2=="ERS"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Yellow_Birch",8] <- sub3_period[sub3_period$species=="Sugar_Maple",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    if(sp=="BOJ" & sp2=="ERR"){
      sub3 <- NULL
      for(j in unique(sub2$period)){ #j=1991
        sub3_period <- filter(sub2, period==j)
        sub3_period[sub3_period$species=="Yellow_Birch",8] <- sub3_period[sub3_period$species=="Red_Maple",8]
        sub3 <- rbind(sub3, sub3_period)
      }
    }
    sub2 <- sub3
  }
  res <- rbind(res, sub2)
}

saveRDS(res, file="sensitivity/ferld_growth_rcp_period_sens.rds")
