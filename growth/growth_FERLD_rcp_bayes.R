#Script that give the growth of the species with the climate of the ferld:
#-Find climatics variable in ferld
#-Open the growth data (one year embargo) 
#-Compute the growth climate relationship
#-Compute growth for each period and scenarios

rm(list=ls())
setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

library(tidyverse)
theme_set(theme_bw())
library(brms)
library(ggpubr)

#climate variables given by climateNA for the ferld 1970-2019 are retrieved
ferld_clim_present <- read.csv("growth/ferld_climateNA_1970-2019MSY.csv")
ferld_clim_present <- ferld_clim_present %>% 
  mutate(CMI_JJA=rowSums(ferld_clim_present[c("CMI06",	"CMI07",	"CMI08")])) %>%
  group_by(Latitude, Longitude, Elevation) %>%
  summarise(CMI_JJA = mean(CMI_JJA),
            MSP=mean(MSP),
            DD5=mean(DD5),
            MAT=mean(MAT)) %>%
  select(Latitude, Longitude, CMI_JJA, MSP, DD5, MAT)

ferld_clim_present <- cbind(rbind(ferld_clim_present, ferld_clim_present, ferld_clim_present),
                            c("rcp26_1991", "rcp45_1991", "rcp85_1991"))

colnames(ferld_clim_present)[7] <- "rcp_period"

#Starting in 2025
ferld_clim_futur <- read.csv("growth/ferld_climateNA_9GCMsMSY.csv")
ferld_clim_futur <- ferld_clim_futur %>% 
  separate(Year, c("ID", "rcp", "period"), sep = "_") %>% 
  mutate(CMI_JJA=rowMeans(ferld_clim_futur[c("CMI06",	"CMI07",	"CMI08")]),
         period=str_remove(period, ".gcm")) %>%
  dplyr::select(Latitude, Longitude, rcp, period, CMI_JJA, MSP, MAT, DD5) %>% 
  unite("rcp_period", rcp:period, sep="_")

ferld_clim <- rbind(ferld_clim_present, ferld_clim_futur)



#Opening the growth data (one year embargo)

# Species code
# sugar maple (Acer saccharum) = 318
# red maple (Acer rubrum) = 316
# yellow birch (Betula alleghaniensis) = 371
# paper birch (Betula papyrifera) = 375
# trembling aspen (Populus tremuloides) = 746
# white cedar (Thuja occidentalis) = 241
# white spruce (Picea glauca) = 94
# balsam fir (Abies balsamea) = 12
# jack pine (Pinus banksiana) = 105

# Variables used:
# MSP: mean summer precipitations
# Max_ST: maximum summer temperature
# DD5: Degree days 5
# CMI_JJA: Climate Moisture Index for June July August


load("growth/growth_clim_NA.Rdata")

code_esp <- data.frame(esp=c("Sugar_Maple","Red_Maple", "Yellow_Birch","Paper_Birch",
                             "Trembling_Aspen", "White_Cedar", "White_Spruce",
                             "Balsam_Fir","Jack_Pine"),
                       code=c(318, 316, 371, 375, 746, 241, 94, 12, 105))

#Centered reduce variables
d <- d %>% filter(MAT!=-9999)
d_norm <- d  %>% 
  mutate(MAT=scale(MAT),
         MSP=scale(MSP),
         CMI_JJA=scale(CMI_JJA),
         DD5=scale(DD5)) %>% 
  left_join(code_esp, by = c("SpeciesFIA"="code"))

# Normalization with the mean and sd of the growth file
ferld_clim_norm <- ferld_clim %>% 
  mutate(MAT=(MAT-mean(d$MAT))/sd(d$MAT),
         MSP=(MSP-mean(d$MSP))/sd(d$MSP),
         DD5=(DD5-mean(d$DD5))/sd(d$DD5),
         CMI_JJA=(CMI_JJA-mean(d$CMI_JJA))/sd(d$CMI_JJA))

#Table for the growth of the species in the sites
ferld_growth <- data.frame(rcp_period=as.character(),
                           species=as.character(),
                           log_growth=as.numeric(),
                           error_growth=as.numeric())

#table for growth curves
response_curve_data <- data.frame(species=as.character(),
                                  fix_eff=as.character(),
                                  x=as.numeric(),
                                  y=as.numeric(),
                                  se=as.numeric())


#table to receive the parameter values. 
summary_tab <- data.frame()


coef_scale <- readRDS("~growth/coef_scale.rds")



for(i in code_esp$code){ 
  d_esp <- filter(d_norm, SpeciesFIA==i)

  
  coef_scale_sp <- filter(coef_scale, species==d_esp$esp[1]) 
  d_esp$ldbh <- (log(d_esp$DBH) - coef_scale_sp$m) / coef_scale_sp$sd
  
  d_esp$dbh_effect <- coef_scale_sp$coef_lin * d_esp$ldbh + coef_scale_sp$coef_quad * d_esp$ldbh^2
  
  # Modele de regression quantile bayesien
  # Devrait prendre ~20 min total
  # brq <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.95, nl = TRUE,
  #               interc ~ 1, lin ~ 0 + ldbh + MAT + MSP + DD5 + CMI_JJA,
  #               quad ~ 0 + I(ldbh^2) + I(MSP^2) + I(MAT^2) + I(DD5^2) + I(CMI_JJA^2)),
  #            prior = c(prior(normal(0, 1), nlpar = interc),
  #                      prior(normal(0, 1), class = b, nlpar = lin),
  #                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
  #            data = d_esp, family = asym_laplace(), chains = 2)
  # 
  # saveRDS(brq, paste0("res_mod_bayes/brq_res_", d_esp$esp[1],".rds"))
  
  brq <- readRDS(paste0("~growth/bayes_model_species/brq_res_", d_esp$esp[1],".rds"))
  summary_tab_sp <- mutate(data.frame(fixef(brq)), species=d_esp$esp[1], effect=rownames(data.frame(fixef(brq))))
  
  summary_tab <- bind_rows(summary_tab, summary_tab_sp)
  
  
  
  X0 <- (log(coef_scale_sp$X0) - coef_scale_sp$m) / coef_scale_sp$sd 
  ferld_clim_norm$dbh_effect <- coef_scale_sp$coef_lin * X0 + coef_scale_sp$coef_quad * X0^2
  fit <- fitted(brq, newdata=ferld_clim_norm, dpar="mu")
  
  ferld_growth <- rbind(ferld_growth,
                            data.frame(rcp_period=ferld_clim_norm$rcp_period,
                                       species=d_esp$esp[1],
                                       log_growth=fit[,1],
                                       error_growth=fit[,2]))
  
  #table for growth curves
  cond_eff <- conditional_effects(brq, dpar="mu")
  for(j in c("MAT", "MSP", "DD5", "CMI_JJA")){
    response_curve_data <- rbind(response_curve_data,
                                 data.frame(species=d_esp$esp[1],
                                            fix_eff=j,
                                            x=cond_eff[[j]][["effect1__"]],
                                            y=cond_eff[[j]][["estimate__"]],
                                            se=cond_eff[[j]][["se__"]]))
  }
}

ferld_growth <- ferld_growth %>% separate(rcp_period, c("rcp", "period")) %>% 
  mutate(low_growth=log_growth-error_growth,
         high_growth=log_growth+error_growth) %>% 
  dplyr::select(rcp, period, species, log_growth, low_growth, high_growth) 

#Bias correction 
biais <- readRDS("~growth/biais_log_croissance_pot_max.rds")

ferld_growth <- ferld_growth %>% left_join(biais) %>% 
  mutate(growth_no_biais=exp(log_growth+biais),
         low_growth_no_biais=exp(low_growth+biais),
         high_growth_no_biais=exp(high_growth+biais)) %>% 
  select(rcp, period, species, growth_no_biais, low_growth_no_biais, high_growth_no_biais)

ferld_growth_noCC <- filter(ferld_growth, rcp=="rcp26", period==1991) %>% 
  mutate(rcp="noCC") %>% 
  arrange(species, rcp, period)

ferld_growth <- bind_rows(ferld_growth,
                          ferld_growth_noCC, 
                          mutate(ferld_growth_noCC, period="2025"),
                          mutate(ferld_growth_noCC, period="2055"),
                          mutate(ferld_growth_noCC, period="2085")) %>% 
  arrange(rcp, period, species)

saveRDS(ferld_growth, file="growth/ferld_growth_rcp_period.rds")


#Continuer avec les courbes de croissances pour voir si on ne va pas au delà des courbes de croissances
ferld_clim_norm2 <- ferld_clim_norm %>%
  pivot_longer(c(`CMI_JJA`: `MAT`), names_to = "fix_eff", values_to = "value")

response_curve_data$species <- str_replace(response_curve_data$species, "_", "\n")


#Pour schéma conceptuel
sc <- response_curve_data %>% 
  filter(species=="Yellow\nBirch") %>% 
  mutate(species="Yellow Birch")

ferld_clim_norm3 <- ferld_clim_norm2 %>% 
  filter(rcp_period %in% c("rcp45_2085", "rcp45_1991")) %>% 
  rename(rcp45=rcp_period) %>% 
  mutate(rcp45=str_remove(rcp45,"rcp45_"))


ferld_growth$species <- str_replace(ferld_growth$species, "_", " ")


ferld_clim <- ferld_clim %>% separate(rcp_period, c("rcp", "period")) %>% 
  pivot_longer(c(`CMI_JJA`: `MAT`), names_to = "variable", values_to = "value")

#We must add without climate change:
ferld_clim_noCC <- filter(ferld_clim, rcp=="rcp26", period==1991) %>% 
  mutate(rcp="noCC") %>% 
  arrange(rcp, period)

ferld_clim <- bind_rows(ferld_clim,
                          ferld_clim_noCC, 
                          mutate(ferld_clim_noCC, period="2025"),
                          mutate(ferld_clim_noCC, period="2055"),
                          mutate(ferld_clim_noCC, period="2085")) %>% 
  arrange(rcp, period)

ferld_clim$rcp[ferld_clim$rcp=="noCC"] <- "Current climate"

ferld_clim$rcp <- factor(ferld_clim$rcp, 
                   levels = c("Current climate", "rcp26", "rcp45", "rcp85"), 
                   labels = c("Current climate", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

ferld_clim$variable <- factor(ferld_clim$variable, 
                         levels = c("CMI_JJA", "MSP", "DD5", "MAT"), 
                         labels = c("CMI JJA", "MSP", "DD5", "MAT"))

ferld_clim$period <- as.numeric(ferld_clim$period)

#Climate variables by period and rcp
clim <- ggplot(ferld_clim, aes(x=period, y=value, color=rcp, group=rcp)) + 
  geom_point() + geom_line() +
  facet_wrap(~variable, scales="free", nrow=1) +
  ylab("Value at FERLD") + xlab("Periods")+
  scale_color_discrete("Climate scenario")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(1991, 2025, 2055, 2085, 2100))

#Growth of species according to period and rcp
ferld_growth2 <- ferld_growth %>% 
  filter(species !="Jack Pine") %>% 
  rename(Species=species)

ferld_growth3 <- ferld_growth2 %>% 
  mutate(period=case_when(period=="1991" ~ "2024",
                     period=="2025" ~ "2054",
                     period=="2055" ~ "2084",
                     period=="2085" ~ "2100",
  )) %>% 
  rbind(ferld_growth2)

ferld_growth3$rcp[ferld_growth3$rcp=="noCC"] <- "Current climate"
colnames(ferld_growth3)[1] <- "Climate scenario"

ferld_growth3$`Climate scenario` <- factor(ferld_growth3$`Climate scenario`, 
                         levels = c("Current climate", "rcp26", "rcp45", "rcp85"), 
                         labels = c("Current climate", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

max_pot_growth <- ggplot(filter(ferld_growth3, Species %in% c("Yellow Birch", "Red Maple", "Sugar Maple")), 
       aes(x=as.numeric(period), y=growth_no_biais, fill=`Climate scenario`, color=`Climate scenario`, group=`Climate scenario`, ymax=high_growth_no_biais, ymin=low_growth_no_biais)) + 
  # geom_point(size=2) + geom_line(size=1) +
  geom_ribbon(alpha=0.5)+
  facet_wrap(~Species) +
  ylim(c(0,NA)) +  xlab("Periods") +
  ylab(expression(''*italic(MaxPotGrowth)*~~(cm.year^-1))) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks=c(1991, 2025, 2055, 2085, 2100))

pdf(file = "growth/Max_growth_rcp.pdf", width = 12, height = 9.2)
  ggarrange(clim, max_pot_growth, ncol=1, labels = c("A", "B"), 
            font.label=list(color="black",size=20))
dev.off()
