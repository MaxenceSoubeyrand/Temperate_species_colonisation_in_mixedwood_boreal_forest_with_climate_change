#Script that give the growth of the species with the climate of the ferld:
#-Open the growth data (one year embargo) 
#-Compute the growth climate relationship

rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(brms)
library(ggpubr)
library(viridis)

#Growth data (one year embargo)
clim_growth <- readRDS(file = "growth_clim_NA.rds")

#We save the normalization coefficients for the climate modeling script. 
coef_scale_clim <- data.frame(
    mean_tave = mean(clim_growth$tave),
    sd_tave = sd(clim_growth$tave),
    mean_MSP = mean(clim_growth$MSP),
    sd_MSP = sd(clim_growth$MSP))

saveRDS(coef_scale_clim, "coef_scale_clim.rds")


#Centered reduce variables
clim_growth_norm <- clim_growth 
clim_growth_norm$MSP <- (clim_growth$MSP-mean(clim_growth$MSP))/sd(clim_growth$MSP)
clim_growth_norm$tave <- (clim_growth$tave-mean(clim_growth$tave))/sd(clim_growth$tave) 


#table to receive parameter values. 
summary_tab <- data.frame()

clim_par <- data.frame()


#The same normalization coefficients are used for DBH
coef_scale <- readRDS("coef_scale_dbh.rds") %>%
  mutate(species=c("Yellow birch", "Red maple", "Sugar maple", "Balsam fir", 
                   "Jack pine", "Trembling aspen", "Paper birch", "White spruce", "White cedar"))

for(i in unique(clim_growth_norm$species)[1:8]){ #i="Trembling aspen"
  d_esp <- filter(clim_growth_norm, species==i)
  
  coef_scale_sp <- filter(coef_scale, species==d_esp$species[1]) 
  d_esp$ldbh <- (log(d_esp$DBH) - coef_scale_sp$mean) / coef_scale_sp$sd
  
  ggplot(d_esp, aes(x=tave, y=DBHI)) + 
    geom_point()+ 
    geom_smooth(method="lm", formula=y~poly(x,2))
  
  # Modele de regression quantile bayesien
  # Devrait prendre ~20 min total
  # brq <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
  #               interc ~ 1, lin ~ 0 + ldbh + MSP + tave ,
  #               quad ~ 0 + I(ldbh^2) + I(MSP^2) + I(tave^2) ),
  #            prior = c(prior(normal(0, 1), nlpar = interc),
  #                      prior(normal(0, 1), class = b, nlpar = lin),
  #                      prior(normal(0, 1), class = b, ub = -0.01, nlpar = quad)),
  #            data = d_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))
  # 
  # saveRDS(brq, paste0("bayes_model_species/brq_res_", d_esp$species[1],".rds"))
  
  brq <- readRDS(paste0("bayes_model_species/brq_res_", d_esp$species[1],".rds"))
  
  #conditional_effects(brq)
  
  summary_tab_sp <- mutate(data.frame(fixef(brq)), species=d_esp$species[1], effect=rownames(data.frame(fixef(brq))))
  summary_tab <- bind_rows(summary_tab, summary_tab_sp)
  
  fixef <- data.frame(fixef(brq))
  fixef$effect <- rownames(fixef)
  
  #conditional_effects(brq)
  
  #Temperature effect
  A_temp <- sqrt(-0.5/filter(fixef, effect=="quad_ItaveE2")$Estimate) #A=sqrt(-0.5/quadratic effect)
  C_temp <- filter(fixef, effect=="lin_tave")$Estimate/(-2*filter(fixef, effect=="quad_ItaveE2")$Estimate) #C=linear effect / -2*quadratic effect.
  
  #Precipitation effect
  A_prec <- sqrt(-0.5/filter(fixef, effect=="quad_IMSPE2")$Estimate)  
  C_prec <- filter(fixef, effect=="lin_MSP")$Estimate/(-2*filter(fixef, effect=="quad_IMSPE2")$Estimate)
  
  #Precipitation effect
  A_ldbh <- sqrt(-0.5/filter(fixef, effect=="quad_IldbhE2")$Estimate)  
  C_ldbh <- filter(fixef, effect=="lin_ldbh")$Estimate/(-2*filter(fixef, effect=="quad_IldbhE2")$Estimate)
  
  #Calculation of MaxPotGrowth, which is the growth when tave, MSP and ldbh are at their optimal values for growth.
  max_pot_growth <- exp(predict(brq, data.frame(ldbh=C_ldbh, tave=C_temp, MSP=C_prec))[1])
  

  clim_par <- data.frame(species=i,
                            A_temp=A_temp,
                            A_prec=A_prec,
                            B=2,
                            C_temp=C_temp,
                            C_prec=C_prec,
                            MaxPotGrowth=max_pot_growth) %>% 
    bind_rows(clim_par)
}

saveRDS(clim_par, "clim_par_sp.rds")


#Produce growth curves using ferld climate data. 

#Ferld climate
clim_coef <- readRDS("coef_FERLD_climate.rds")

#we redo the climate series as SORTIE does.
clim <- NULL
for(i in unique(clim_coef$ssp)){ #
  Temp <- filter(clim_coef, ssp==i, climate_variable=="tave")$T1+
    filter(clim_coef, ssp==i, climate_variable=="tave")$B*
    (1:110)^filter(clim_coef, ssp==i, climate_variable=="tave")$C
  
  Prec <- filter(clim_coef, ssp==i, climate_variable=="MSP")$T1+
    filter(clim_coef, ssp==i, climate_variable=="MSP")$B*
    (1:110)^filter(clim_coef, ssp==i, climate_variable=="MSP")$C
  
  #Reset to original scales (off-center and reduce)
  Temp_or <- Temp*coef_scale_clim$sd_tave+coef_scale_clim$mean_tave
  
  Prec_or <- Prec*coef_scale_clim$sd_MSP+coef_scale_clim$mean_MSP
  
  clim <- clim %>% bind_rows(data.frame(ssp=i,
                                        clim_val=c(Temp,Prec), 
                                        clim_val_or=c(Temp_or, Prec_or),
                                        clim_var=c(rep("TAVE", 110), 
                                                 rep("MSP", 110)), 
                                        year=rep(1:110, 2)))
}

clim_plot <- clim

colnames(clim_plot)[1] <- "Climate scenarios"

clim_plot$`Climate scenarios` <- factor(clim_plot$`Climate scenarios`, levels = c("noCC", "ssp126", "ssp245", "ssp585"),
                                labels=c("Current climate", "SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5"))

clim_plot$clim_var  <- factor(clim_plot$clim_var, levels = c("MSP", "TAVE"),
                   labels=c("MSP (in mm)", "TAVE (in °C)"))

clim_ferld <- ggplot(clim_plot, aes(x=year+1990, y=clim_val_or, color=`Climate scenarios`, group=`Climate scenarios`)) +
  geom_line(linewidth=1) + 
  scale_color_viridis_d() +
  facet_wrap(~clim_var, scales="free_y") +
  ylab("Value at FERLD") +
  xlab("Time") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.position="bottom")



#For each species, calculate climate and precipitation effect (between 0 and 1)

growth_effect=NULL
for(sp in unique(clim_par$species)){ #sp="Red maple"
  for(i in unique(clim_coef$ssp)){
    
    temp_effect=exp(-0.5*((filter(clim, ssp==i, clim_var=="TAVE")$clim_val-
                             filter(clim_par, species==sp)$C_temp)/
                            filter(clim_par, species==sp)$A_temp)^filter(clim_par, species==sp)$B)
    
    prec_effect=exp(-0.5*((filter(clim, ssp==i, clim_var=="MSP")$clim_val-
                             filter(clim_par, species==sp)$C_prec)/
                            filter(clim_par, species==sp)$A_prec)^filter(clim_par, species==sp)$B)
    
    growth=filter(clim_par, species==sp)$MaxPotGrowth*temp_effect*prec_effect
    
    
    growth_effect <- growth_effect %>% bind_rows(data.frame(species=sp, 
               ssp=i, 
               growth=growth,
               effect=c(rep("TAVE", 110), rep("MSP", 110)), 
               effect_val=c(temp_effect, prec_effect),
               year=rep(1:110, 2),
               clim=c(filter(clim, ssp==i, clim_var=="TAVE")$clim_val,
                      filter(clim, ssp==i, clim_var=="MSP")$clim_val)))
  }
}

growth_effect <- growth_effect %>% 
  filter(species %in% c("Red maple", "Sugar maple", "Yellow birch"))

colnames(growth_effect)[2] <- "Climate scenarios"

growth_effect$`Climate scenarios` <- factor(growth_effect$`Climate scenarios`, levels = c("noCC", "ssp126", "ssp245", "ssp585"),
                                        labels=c("Current climate", "SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5"))

growth_effect_plot <- ggplot(growth_effect, aes(x=year+1990, y=growth, color=`Climate scenarios`, group=`Climate scenarios`)) +
  geom_line(linewidth=1) + 
  scale_color_viridis_d() +
  facet_grid(~species) +ylim(0,NA) +
  ylab("MaxPotGrowth x TE (TAVE) x\n PE (MSP) (in cm.year⁻¹)") +
  xlab("Time") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.position="bottom",
        plot.margin = margin(l = 14, r = 10, b = 5, t = 5))

growth_effect_plot


clim_growth <- ggarrange(clim_ferld, growth_effect_plot, 
          ncol=1, 
          labels="AUTO",  font.label = list(size = 18), hjust=0, vjust = c(1.5, 0), 
          common.legend = T, legend="bottom")

pdf(file = "../figures/Climate_growth.pdf", width = 9, height = 8)
clim_growth
dev.off()

png(file = "../figures/Climate_growth.png", width = 9, height = 8, unit="in", res= 300)
clim_growth
dev.off()


