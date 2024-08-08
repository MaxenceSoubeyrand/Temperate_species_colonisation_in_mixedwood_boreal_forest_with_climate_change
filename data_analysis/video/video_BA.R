
library(tidyverse)
theme_set(theme_bw())


res_detailed_adult <- readRDS("../result_simulations/res_detailed_adult.rds") %>% 
  filter(climate_scenario=="ssp245",
         patch_size=="400",
         !is.na(DBH))

res_detailed_sapling <- readRDS("../result_simulations/res_detailed_sapling.rds") %>% 
  filter(climate_scenario=="ssp245",
         patch_size=="400",
         !is.na(DBH))

res <- bind_rows(res_detailed_adult, res_detailed_sapling) %>% 
  select(-perturbation) %>% 
  mutate(year=as.numeric(timestep)+1990)

res$species[res$species=='Red_Maple'] <- "Temperate species"
res$species[res$species=='Sugar_Maple'] <- "Temperate species"
res$species[res$species=='Yellow_Birch'] <- "Temperate species"

res


 res$species <- factor(res$species, 
                      levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch",
                                 "Trembling_Aspen", "White_Cedar", "White_Spruce",
                                 "Temperate species"), 
                      labels = c("Balsam fir", "Jack pine", "Paper birch",
                                 "Trembling aspen", "White cedar", "White spruce",
                                 "Temperate species"))

res$species_test <- factor(res$species_test, 
                           levels = c("ERR", "ERS", "BOJ"), 
                           labels = c("Red maple", "Sugar maple", "Yellow birch"))

res$fire_year <- factor(res$fire_year, 
                        levels = c("1760", "1823", "1964", "1923"), 
                        labels = c("1760", "1823", "1964", "1923 - Harvested"))

colors <- c("#0077BB", "#33BBEE", "#009988","#EE7733", "#CC3311", "#EE3377", "black", "#E6AB02", "#66A61E")
sq <- scales::trans_new("squared", function(x) x^2, sqrt)

res <- res %>% 
  mutate(`DBH class`=ifelse(DBH<10, "<10","\u2265 10"))



make_plot <- function(){
  for(i in sort(unique(res$year))){ #i=1990
    p <- ggplot(filter(res, year==i), 
                aes(x=X, y=Y, color=species, size=`DBH class`)) +
      geom_point() +
      geom_point(data=filter(res, year==i, species %in% c("Temperate species")), 
                 aes(x=X, y=Y, size=`DBH class`), color="black") +
      scale_size_manual(values=c(0.1, 1)) +

      scale_color_manual("Species", values=colors) +
      facet_grid(species_test~fire_year) +
      annotate("path",
               color="red", linewidth=0.7,
               x=50+sqrt(400/pi)*cos(seq(0,2*pi,length.out=100)),
               y=50+sqrt(400/pi)*sin(seq(0,2*pi,length.out=100)))+
      xlim(0,100) + ylim(0,100) +
      ggtitle(paste0(i))
    
    ggsave(p, filename=paste0("graph/", i,".png"), width=20, height=13, unit="cm")
  }
}

make_plot() 


#Video with Microsoft clipchamp



