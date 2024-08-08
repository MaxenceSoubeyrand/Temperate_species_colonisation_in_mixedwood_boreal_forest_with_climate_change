#The goal is to decompose the growth and to look at the impact of the different 
#effects modeled according to the scenarios tested, we will be able to know what 
#limits the growth the most. 

#We only have the growth and the light from Sortie output, but with the growth
#equation, we are able to find DBH effect, climate effect and shadow effect. With the maximum
#potential growth, we are able to find the crowding effect. 
rm(list=ls())


library(tidyverse)
theme_set(theme_bw())
library(viridis)
library(ggh4x)

#Loading the data
res_ad <- readRDS(file="../result_simulations/res_detailed_adult.rds")
head(res_ad)

#Calculate the timestep
res <- res_ad %>% 
  filter(!is.na(DBH),
         patch_size=="400") %>% 
  mutate(year=as.numeric(timestep)+1990,
         Growth=Growth/5) %>%  
  mutate(d=(X-50)^2+(Y - 50)^2) %>% 
  mutate(patch_size=ifelse(patch_size=="NA", "0", patch_size)) %>% 
  mutate(d2=as.numeric(patch_size)/pi) %>% 
  filter(d2<d) %>% 
  select(-perturbation, -stage, -X, -Y, -d2, -d) %>% 
  filter(Growth != 0, species != "Jack_Pine")



#Get parameters (X0, Xb, m and MaxPotGrowth)
clim_par_sp <- readRDS("~/PhD/chap3/new_simulation/growth_climateNA/clim_par_sp.rds") %>% 
  bind_cols(species2 = c("Balsam_Fir", "White_Spruce", "White_Cedar", 
                        "Trembling_Aspen", "Paper_Birch", 
                        "Yellow_Birch", "Red_Maple", "Sugar_Maple"))

par_growth <- data.frame(species2 = c("Sugar_Maple", "Red_Maple", "Yellow_Birch", 
                                     "Paper_Birch", "Trembling_Aspen", "White_Cedar", 
                                     "White_Spruce", "Balsam_Fir"),
           X0 = c(21.654829,	20.189625,	16.182373, 15.866908, 45.282974, 103.21548, 16.386118, 13.686995),
           Xb = c(1.1890368,	1.8364733,	1.0655644, 0.90537345, 1.0310016, 2.8542862, 0.8052472, 0.7543218),
           m = c(1.16,	0.564334,	0.8021385, 0.72, 0.84, 0.42, 0.83, 0.7)) %>% 
  right_join(clim_par_sp) %>% 
  select(-species) %>% 
  rename(species=species2)



climate <- readRDS("~/PhD/chap3/new_simulation/growth_climateNA/coef_FERLD_climate.rds")

# Create a complete grid with all combinations of climate varaibles and ssp with years. 
climate_year <- climate %>%
  expand_grid(data.frame(timestep = 0:109)) %>%
  mutate(climate_value=T1+B*timestep^C) %>% 
  filter(timestep %in% c(seq(0,105,5), 109)) %>% 
  mutate(timestep=as.character(timestep)) %>% 
  select(-T1, -B, -C, climate_scenario=ssp) %>% 
  pivot_wider(names_from = climate_variable, values_from = climate_value)

res <- right_join(res, par_growth) %>% 
  right_join(climate_year) %>% 
  filter(Growth<MaxPotGrowth)

res_effect <- res %>% 
  filter(species %in% c("Red_Maple", "Sugar_Maple", "Yellow_Birch")) %>% 
#calculating effects
  mutate(DBH_effect=exp(-0.5*((log(DBH)-log(X0))/Xb)^2),
         shading_effect=exp(-m*Light),
         temp_effect=exp(-0.5*((tave-C_temp)/A_temp)^2),
         prec_effect=exp(-0.5*((MSP-C_prec)/A_prec)^2),
         crowding_effect=Growth/(MaxPotGrowth*DBH_effect*shading_effect*temp_effect*prec_effect)) %>% 
 group_by(fire_year, species, climate_scenario, year, patch_size) %>% 
  summarise(

            MaxPotGrowth = mean(MaxPotGrowth),
            DBH_effect=mean(DBH_effect),
            shading_effect = mean(shading_effect),
            crowding_effect = mean(crowding_effect),
            temp_effect = mean(temp_effect),
            prec_effect =mean(prec_effect)) %>% 
  pivot_longer(cols = c(`DBH_effect`, `shading_effect`, `temp_effect`,
                        `prec_effect`, `crowding_effect`), names_to = "Effects", values_to="Growth")





res_effect$Effects <- factor(res_effect$Effects, levels = c('DBH_effect', 'shading_effect', 'crowding_effect', 'temp_effect', 'prec_effect', "Realized_growth"),
                                labels=c('Size effect', 'Shading effect', 'Crowding effect', 'Temperature effect', 'Precipitation effect',"Realized growth"))

res_effect$species <- factor(res_effect$species, levels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"),
                                labels=c("Red maple",  "Sugar maple", "Yellow birch"))

res_effect$fire_year <- factor(res_effect$fire_year, levels = c("1760", "1823", "1964", "1923"),
                                labels=c("1760", "1823", "1964", "Harvested"))

res_effect$climate_scenario <- factor(res_effect$climate_scenario, levels = c("noCC", "ssp126", "ssp245", "ssp585"),
                                  labels=c("Current\nclimate", "SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5"))

plot <- ggplot(filter(res_effect, patch_size=="400"), aes(x=year, y=Growth, group=Effects, color=Effects)) +
  geom_line(linewidth=1.5) + scale_color_viridis("", discrete = T) +
  #geom_line(data=res_effect_maxpotgrowt, aes(x=year, y=MaxPotGrowth), color="red") + 
  facet_nested(fire_year~species+climate_scenario) + 
  # ylim(NA,NA) +
  ylab("Limiting effects on tree growth (each effect varies between 0 and 1)") + 
  xlab("Year") +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.position="bottom") + 
  guides(x = guide_axis(angle = 90))



pdf(file = "../figures/growth_decomposition.pdf", width = 15.3, height = 8)
plot
dev.off()

png(filename = "../figures/growth_decomposition.png", width = 15.3, height = 8, units="in", res=500)
plot
dev.off()
