rm(list=ls())

library(raster)
library(tidyverse)

source("process_output_functions.R")

parameter_file <- Sys.getenv("parameter_file")
  
print(parameter_file)

## Detailed output ----
#Second we took more detailed informations: adult growth, regeneration.

### Adult ----
placette <- str_remove(parameter_file, pattern=".xml")
placette <- str_remove(placette, pattern="../parameter_files/")


# Files containing detailed time results for a replica at all time steps of 5
t_files <- dir("../output", pattern = paste0("^", placette, "_"), full.names = TRUE)
step <- str_split(t_files, "_", simplify = TRUE)[,6]
step <- as.numeric(str_remove(step, ".xml"))
step <- unique(c(subset(step, step %in% seq(0,200,5)), max(step)))
t_files <- paste0("../output/", placette,"_",step,".xml")

# Produce one data frame per file (treemap; mature trees only) stored in a list
t <- map(t_files, ~ treemap_from_file(.)$adult)
names(t) <- step

t2 <- compact(t)

t <- t2

#Changes the name of the 5th column of the tables in t.
for(j in names(t)) {
    colnames(t[[j]])[5] <- paste0("DBH", j)
    colnames(t[[j]])[6] <- paste0("Growth", j)
    colnames(t[[j]])[7] <- paste0("Light", j)
}

#Merge the data in a single table with a column for the DBH of each timestep (will allow to do the mortality)
t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

#Reverse the table to make it tidy
t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)")

#Put in a sub-plot the size of the government plots
r <- raster(ncol=10, nrow=10, xmn=0, xmx=200, ymn=0, ymx=200)
values(r) <- 1:ncell(r)
t <- t %>% mutate(subplot=raster::extract(r, SpatialPoints(cbind(t$X, t$Y)))) %>%
  arrange(timestep, subplot)
t$plot <- paste0(placette, "_adult")

save(t, file=paste0("../output_df/",placette, "_adult.Rdata"))



# Produce one data frame per file (treemap; mature trees only) stored in a list
t <- map(t_files, ~ treemap_from_file(.)$sapling)
names(t) <- step

t2 <- compact(t)

t <- t2

#Changes the name of the 5th column of the tables in t.
for(j in names(t)) {
  colnames(t[[j]])[5] <- paste0("DBH", j)
  colnames(t[[j]])[6] <- paste0("Growth", j)
  colnames(t[[j]])[7] <- paste0("Light", j)
}

#Merge the data in a single table with a column for the DBH of each timestep (will allow to do the mortality)
t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

#Reverse the table to make it tidy
t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)")

#Put in a sub-plot the size of the government plots
r <- raster(ncol=10, nrow=10, xmn=0, xmx=200, ymn=0, ymx=200)
values(r) <- 1:ncell(r)
t <- t %>% mutate(subplot=raster::extract(r, SpatialPoints(cbind(t$X, t$Y)))) %>%
  arrange(timestep, subplot)
t$plot <- paste0(placette, "_sapling")

save(t, file=paste0("../output_df/",placette, "_sapling.Rdata"))





