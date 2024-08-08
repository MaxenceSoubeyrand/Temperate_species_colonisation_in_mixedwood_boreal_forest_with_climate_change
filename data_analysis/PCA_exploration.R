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


res_ad <- readRDS(file="../result_simulations/res_detailed_adult.rds")

res_sap <- readRDS(file="../result_simulations/res_detailed_sapling.rds")

res <- bind_rows(res_ad, res_sap) %>%
  select(-perturbation) %>% 
  filter(!is.na(DBH)) %>%
  mutate(year=as.numeric(timestep)+1990) %>% 
  mutate(d=(X-50)^2+(Y - 50)^2,
         patch_size=ifelse(patch_size=="NA", "0", patch_size),
         d2=as.numeric(patch_size)/pi) %>% 
  filter(d2<d) %>%
  #BA calculation for each species and each simulation
  group_by(fire_year, species_test, patch_size, species, year, climate_scenario) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(patch_size)/10000)) %>% 
  ungroup() %>% 
  pivot_wider(names_from="species", values_from=BA, values_fill=0) %>%
  group_by(fire_year, species_test, patch_size, year, climate_scenario) %>% 
  summarize_all(sum)


tail(res)
colnames(res)[6:14] <- c("Balsam fir", "Jack pine", "Paper birch", "Trembling aspen", "White cedar", 
                         "White spruce", "Yellow birch", "Red maple", "Sugar maple")
#PCA
res.pca <- PCA(scale(res[6:14]),  graph = FALSE)

#Graphique des varainces expliqu?es par les axes
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
#Axe1: 30.8% et axe 2: 18.5% 

#Cercle de corr?lation avec en couleur les contributions
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

centroid_fire_year <- df_pca %>% 
  group_by(year, fire_year, patch_size, species_test, climate_scenario) %>% 
  summarise(centroid_dim1=mean(Dim.1),
            centroid_dim2=mean(Dim.2))

centroid_fire_year <- centroid_fire_year %>% filter(!fire_year %in% c("1910", "1923"))

centroid_fire_year$species_test <- factor(centroid_fire_year$species_test, 
                                          levels = c("control", "BOJ", "ERS", "ERR", "all"), 
                                          labels = c("Control", "Yellow birch", "Sugar maple", "Red maple", "All"))

centroid_fire_year$patch_size <- factor(centroid_fire_year$patch_size,
                                     levels = c("0", "10", "30", "80", "150", "250", "400"))


control <- filter(centroid_fire_year, species_test=="Control")
centroid_fire_year <- filter(centroid_fire_year, species_test!="Control") %>% 
  bind_rows(mutate(control, species_test="Yellow birch"),
            mutate(control, species_test="Red maple"),
            mutate(control, species_test="Sugar maple"))


colnames(centroid_fire_year)[2:3] <- c("Fire year", "Patch size")

centroid_fire_year <- centroid_fire_year %>% 
  filter(`Patch size` %in% c("0","10", "80", "400")) %>% 
  mutate(patch="Patch size (in m²)")

centroid_fire_year <- centroid_fire_year %>% 
  rename(X=centroid_dim1, Y=centroid_dim2)

centroid_fire_year <- centroid_fire_year %>% 
  filter(climate_scenario =="ssp245") %>% 
  filter(`Patch size` %in% c("0", "400")) %>% 
  rename(` ` = `Patch size`)

centroid_fire_year$` ` <- factor(centroid_fire_year$` `, 
                                          levels = c("0", "80", "400"), 
                                          labels = c("No temperate trees", "80", "With temperate trees"))

p <- ggplot(centroid_fire_year, aes(x=X, y=Y, 
                                    color=` `, fill=` `, 
                                    group=interaction(`Fire year`, ` `), 
                                    shape=`Fire year`)) +
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_vline(xintercept=0, linetype="dashed") +
  geom_path(alpha=1, linewidth=0.8) +
  geom_point(data=filter(centroid_fire_year, year==2100), size=3) +
  scale_size_manual(values=c(2,2,2,2.5,2.5,2)) +
  geom_point(data=filter(centroid_fire_year, year==1990), aes(shape=`Fire year`), color="black", fill="black", size=4) +
  scale_shape_manual(values=c(8, 16, 15,18,17,25)) +
  facet_wrap(~species_test, ncol=2) +
  xlab("Dimension 1 (30.8%)") + ylab("Dimension 2 (18.5%)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.position="bottom",legend.box="vertical", legend.margin=margin())

pdf(file = "../figures/PCA.pdf", width = 7, height = 8)
  p
dev.off()

pdf(file = "../figures/cercle.pdf", width = 7/2, height = 8/2)
  cercle
dev.off()

#Combine these two graphs with inkscape
