rm(list=ls())
gc()

library(tidyverse)
theme_set(theme_bw())
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(gtable)
library(grid)
library(ggh4x)
library(ggpubr)

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

sap_bor <- readRDS(file="result_simulations/res_detailed_sapling_boreal_species.rds")

gc()

sap_bor <- sap_bor %>% 
  select(-perturbation, -subplot) 



gc()

sap_bor <- sap_bor %>%
  filter(!is.na(DBH),
         !(timestep=="0" & period %in% c("2025", "2055", "2085")))

gc()

sap_bor <- sap_bor %>% #I do this otherwise it takes into account the tfinal of each period + the tinitial of the next period.
  mutate(year=as.numeric(timestep)+as.numeric(period)) %>% 
  mutate(d=(X-50)^2+(Y - 50)^2,
         density=ifelse(density=="NA", "0", density),
         d2=as.numeric(density)/pi) %>% 
  filter(d2<d) 

gc()

sap_bor <- sap_bor%>%
  #Calculation of BA for each species and each simulation
  group_by(fire_year, species_test, density, species, year, rcp) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(density)/10000)*4) %>% 
  ungroup() %>% 
  pivot_wider(names_from="species", values_from=BA, values_fill=0) %>% 
  mutate(Red_Maple=0, Sugar_Maple=0, Yellow_Birch=0)

saveRDS(sap_bor, file="result_simulations/sap_bor_PCA.rds")

gc()

sap_bor <- readRDS(file="result_simulations/sap_bor_PCA.rds")

#The same for other species and other stages
sap_temp <- readRDS(file="result_simulations/res_detailed_sapling_temperate_species.rds")
ad_bor <- readRDS(file="result_simulations/res_detailed_adult_boreal_species.rds")
ad_temp <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds")

res <- bind_rows(sap_temp, ad_bor, ad_temp) %>% #sap_bor is bind_row() later
  select(-perturbation, -subplot) %>% 
  filter(!is.na(DBH),
         !(timestep=="0" & period %in% c("2025", "2055", "2085"))) %>% #I do this otherwise it takes into account the tfinal of each period + the tinitial of the next period.
  mutate(year=as.numeric(timestep)+as.numeric(period)) %>% 
  mutate(d=(X-50)^2+(Y - 50)^2,
         density=ifelse(density=="NA", "0", density),
         d2=as.numeric(density)/pi) %>% 
  filter(d2<d) %>%
  #Calculation of BA for each species and each simulation
  group_by(fire_year, species_test, density, species, year, rcp) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(density)/10000)) %>% 
  ungroup() %>% 
  pivot_wider(names_from="species", values_from=BA, values_fill=0) %>% 
  bind_rows(sap_bor) %>% 
  group_by(fire_year, species_test, density, year, rcp) %>% 
  summarize_all(sum)


tail(res)
colnames(res)[6:14] <- str_replace(colnames(res)[6:14], "_", " ")
#La PCA
res.pca <- PCA(scale(res[6:14]),  graph = FALSE)

#Graph of the variances explained by the axes
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Correlation circle with colored contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
cercle <- fviz_pca_var(res.pca,repel = TRUE) +
  theme(plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  text = element_text(size = 18),
  axis.text = element_text(size = 12))

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


df_pca <- cbind(data.frame(get_pca_ind(res.pca)$coord), res)


#We are trying to recover the centroids. 
centroid_fire_year <- df_pca %>% 
  group_by(year, fire_year, density, species_test,rcp) %>% 
  summarise(centroid_dim1=mean(Dim.1),
            centroid_dim2=mean(Dim.2))

centroid_fire_year <- centroid_fire_year %>% filter(!fire_year %in% c("1910", "1923"))

centroid_fire_year$species_test <- factor(centroid_fire_year$species_test, 
                                          levels = c("control", "BOJ", "ERS", "ERR", "all"), 
                                          labels = c("Control", "Yellow Birch", "Sugar Maple", "Red Maple", "All"))

centroid_fire_year$density <- factor(centroid_fire_year$density,
                                     levels = c("0", "10", "30", "80", "150", "250", "400"))

#We're trying to add control in the studs:
control <- filter(centroid_fire_year, species_test=="Control")
centroid_fire_year <- filter(centroid_fire_year, species_test!="Control") %>% 
  bind_rows(mutate(control, species_test="Yellow Birch"),
            mutate(control, species_test="Red Maple"),
            mutate(control, species_test="Sugar Maple"))


colnames(centroid_fire_year)[2:3] <- c("Fire year", "Density")

centroid_fire_year <- centroid_fire_year %>% 
  filter(Density %in% c("0","10", "80", "400")) %>% 
  mutate(patch="Patch size (in m²)")

centroid_fire_year <- centroid_fire_year %>% 
  rename(X=centroid_dim1, Y=centroid_dim2)
saveRDS(centroid_fire_year, "data_pca.rds")

centroid_fire_year <- readRDS("data_pca.rds") %>% 
  filter(rcp =="rcp45") %>% 
  filter(Density %in% c("0", "400")) %>% 
  rename(`Patch size (in m²)` = Density)

centroid_fire_year$`Patch size (in m²)` <- factor(centroid_fire_year$`Patch size (in m²)`, 
                                          levels = c("0", "80", "400"), 
                                          labels = c("No temperate trees", "80", "400 m²"))

p <- ggplot(centroid_fire_year, aes(x=X, y=Y, 
                                    color=`Patch size (in m²)`, fill=`Patch size (in m²)`, 
                                    group=interaction(`Fire year`, `Patch size (in m²)`), 
                                    shape=`Fire year`)) +
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_vline(xintercept=0, linetype="dashed") +
  geom_path(alpha=1, size=0.8) +
  geom_point(data=filter(centroid_fire_year, year==2100), size=3) +
  scale_size_manual(values=c(2,2,2,2.5,2.5,2)) +
  geom_point(data=filter(centroid_fire_year, year==1991), aes(shape=`Fire year`), color="black", fill="black", size=4) +
  scale_shape_manual(values=c(8, 16, 15,18,17,25)) +
  scale_color_manual(" ") +
  facet_wrap(~species_test, ncol=2) +
  xlab("Dimension 1 (27.9%)") + ylab("Dimension 2 (19.8%)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.position="bottom",legend.box="vertical", legend.margin=margin())

p
cercle

pg <- ggplotGrob(p)
cercleg <- ggplotGrob(cercle)

pl <- gtable_filter(pg, 'panel', trim=F)$layout
pg <- gtable_add_grob(pg, cercleg, t=max(pl$t), l=max(pl$l))

grid.newpage()
grid.draw(pg)

png(filename = "/figures/experimental_design/PCA.png", width = 700, height = 750)
  grid.newpage()
  grid.draw(pg)
dev.off()

pdf(file = "~/figures/experimental_design/PCA.pdf", width = 7, height = 8)
  grid.newpage()
  grid.draw(pg)
dev.off()