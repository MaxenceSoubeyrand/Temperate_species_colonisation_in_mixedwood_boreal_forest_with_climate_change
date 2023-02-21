#Create a function that creates a parameter set based on the initial data set for cond init, 
#growth table, and seedling table that I fail based on the test I want to run.  
first_parameter_file <- function(fire_year = 1760,
                                 growth = ferld_growth, rcp_f = "noCC", period_f = 2025, #2025 means 2025-2055, 2055 means 2055-2085 and 2085 means 2085-2100
                                 density_test_f = "low", species_test_f = "ERS",
                                 plot_size=100){
  

  #We filter the growth data, for the name of the species in growth it is important that it is Genre_Espece
  growth <- growth %>% filter(rcp == rcp_f, 
                              period == period_f) 
  
  #We put an empty seedling density table to avoid duplicating the tree maps
  dens_init_vide <- data.frame(plot_id="1760", year=1991, 
                               species_id="ABA", dbh_class=10, density=0)
  
  #compute timestep
  timesteps <- case_when(period_f == 1991 ~ 34,
                         period_f == 2025 ~ 30,
                         period_f == 2055 ~ 30,
                         period_f == 2085 ~ 15)
  
  #tree map name of simulation
  tree_map_name <- paste0(fire_year, "_", species_test_f, "_", density_test_f, "_", rcp_f, "_", period_f, ".txt")
  
  #out_dir
  out_dir <- "" #Path where the output will be directed.
  par_dir <- "" #Path where the parameter files will be directed.
  
  #The name of the parameter file
  par_name <- paste(fire_year, species_test_f, density_test_f, rcp_f, period_f, sep="_")
  
  create_param_file(data=dens_init_vide, file_name=par_name,  growth=growth,
                    timesteps=timesteps, out_dir=out_dir, par_dir=par_dir,
                    plot_size=100, tree_map=T, tree_map_name=tree_map_name)
  
  return(par_name)
}



#Function that combines the initially created parameter file and the spatialization of the trees 
#in the output of the last time step of the previous simulation. 
#The function also updates the growth and timestep. 
change_first_par_files <- function(par_name=par_name,   
                                   growth = ferld_growth, rcp_f = "noCC", period_f = 2025){
  
  #Growth is recovered for the right scenario
  growth <- growth %>% filter(rcp == rcp_f, 
                              period == period_f) 
  
  #Let's create the txt file (treemap) of the last time step of the previous simulation. 
  #The value of the last time step of the previous simulation (I could have taken just max, but it is not supposed to move anyway)
  last_timestep <- case_when(period_f == 2025 ~ 34,
                             period_f == 2055 ~ 30,
                             period_f == 2085 ~ 30) 
  
  #Recover data
  map <- treemap_from_file(paste0("../output/", par_name, "_", last_timestep, ".xml")) 
  map[["seedling"]]$DBH <- 0.5
  
  #We transform to put it in the right format
  map <- bind_rows(map, .id="Type")%>% 
    mutate(Height=0) %>% 
    rename(Species=species, Diam=DBH) %>% 
    dplyr::select(X, Y, Species, Type, Diam, Height) %>% 
    mutate(Type=str_to_sentence(Type))
  
  #save the treemap
  tree_map_name <- paste0(substr(par_name, 1, nchar(par_name)-4), period_f, ".txt")
  write.table(x = map, file = paste0("../tree_maps/", tree_map_name), sep="\t", row.names=F, quote=F)
  
  #Simulation time
  timesteps <- case_when(period_f == 2025 ~ 30,
                         period_f == 2055 ~ 30,
                         period_f == 2085 ~ 15)
  
  #open the previous parameter file
  par_xml <- read_xml(paste0("../parameter_files/", par_name, ".xml")) #ok
  par_list <- as_list(par_xml)
  par_list$paramFile$plot$timesteps[[1]] <- timesteps #on change le timestep
  
  #we change the growth
  #Growth, could be optimized but I don't know how, now it's going really fast
  n <- length(par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth) #n species
  for(i in 1:n){
    sp <- attributes(par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth[[i]])$species #The ith species
    growth_sp <- growth$growth[which(growth$species==sp)] #The growth of the ith species in the plot
    par_list$paramFile$NCIMasterGrowth7$gr_nciMaxPotentialGrowth[[i]][1] <- as.character(growth_sp) #We replace the growth in the parameter file
  }
  
  #We add the initial conditions (the treemap)
  par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0("/home/msoubeyr/projects/def-fgennare/msoubeyr/chap3/tree_maps/", tree_map_name)
  
  #Name of the new parameter set
  new_par_name <- paste0(substr(par_name, 1, nchar(par_name)-4), period_f)
  
  #We change the outputs
  par_list$paramFile$Output$ou_filename[[1]] <- paste0("/home/msoubeyr/projects/def-fgennare/msoubeyr/chap3/output/", new_par_name, ".gz.tar")
  par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0("/home/msoubeyr/projects/def-fgennare/msoubeyr/chap3/output/", new_par_name, ".out")
  
  #Convert back to XML format and save in new file
  par_xml_new <- as_xml_document(par_list)
  write_xml(par_xml_new, paste0("/home/msoubeyr/projects/def-fgennare/msoubeyr/chap3/parameter_files", "/", new_par_name, ".xml"))
  return(new_par_name)
}

#Function to launch the simulations for a timeline. Uses the two functions from before.
#Function that will run the whole chronology for a stand type (fire year), for a test (species_test), for a test density and for a climate scenario (rcp)
#-create the first parameter file: 1991-2025
#-run the simulation and wait for it to finish
#-create the parameter file 2025-2055 (change the growths and take the treemap of the previous simulation)
#-run the simulation and wait for it to finish 
#etc until 2100
#Take the simulation 2100 and create a treemap text file
submit_chrono <- function(fire_year, species_test, density, rcp){
  
  ferld_growth <- readRDS("/growth/ferld_growth_rcp_period.rds") #Load growth data
 
 
  #We create the first parameter file and we keep in memory in par_name the name of the parameter file
  par_name <- first_parameter_file(fire_year = fire_year,
                                   growth = ferld_growth, rcp_f = rcp, period_f = 1991,
                                   density_test_f = density, species_test_f = species_test,
                                   plot_size=100)
  #OK the first parameter set is created. 
  
  print(par_name)
  
  #Start the simulation with the parameter set
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  # 
  #While the simulation is running we wait
  sq <- system("squeue --format='%.40j' --me", intern=T)
  
  
  while(any(grepl(par_name, sq))){ #as long as the parameter file name is in the sq then wait. 
    Sys.sleep(180)
    sq <- system("squeue --format='%.40j' --me", intern=T)
    print("Wait 3 min")
  }
  
  #First we have to check that the azimuth bug is not present
  #here we will look in the last file of the simulation if the azimuth bug is present. 
  pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  if(pres_azi) print("AZIMUTH")
  
  while(pres_azi){
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  sq <- system("squeue --format='%.40j' --me", intern=T)
    while(any(grepl(par_name, sq))){ #tant que le nom du fichier de paramètre est dans le sq alors attendre. 
      Sys.sleep(180)
      sq <- system("squeue --format='%.40j' --me", intern=T)
      print("wait 3 min azimuth")
    }
    
  #We check again here
  pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  }
  
  
  if(!pres_azi){
    print("1991:2025; Pas le bug de l'Aizimuth: no_azimuth")
  }
  
  #Here, the 1991-2025 simulation has finished running
  #Here we must take the distribution of the last timestep of the 1991-2025 simulation, change the growths and start again.  
  
  #we run 2025-2055
  par_name <- change_first_par_files(par_name=par_name, 
                                     growth = ferld_growth, rcp_f = rcp, period_f = 2025)
  
  print(par_name)
  
  #We launch the simulation with the parameter set here
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  # 
  #While the simulation is running we wait
  sq <- system("squeue --format='%.40j' --me", intern=T)

  while(any(grepl(par_name, sq))){ #as long as the parameter file name is in the sq then wait.  
    print("Wait 3 min")
    Sys.sleep(180)
    sq <- system("squeue --format='%.40j' --me", intern=T)
  }

  #First we have to check that the azimuth bug is not present
  #here we will look in the last file of the simulation if the azimuth bug is present. 
  pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  if(pres_azi) print("AZIMUTH")
  
  while(pres_azi){
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  sq <- system("squeue --format='%.40j' --me", intern=T)
    while(any(grepl(par_name, sq))){ #As long as the parameter file name is in the sq then wait.
      print("wait 3 min azimuth") 
      Sys.sleep(180)
      sq <- system("squeue --format='%.40j' --me", intern=T)
    }
    
  #We check again here
    pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  }
  
  
  if(!pres_azi){
    print("2025:2055; Pas le bug de l'Aizimuth: no_azimuth")
  }
  
  #executting 2055-2085
  par_name <- change_first_par_files(par_name=par_name, 
                                     growth = ferld_growth, rcp_f = rcp, period_f = 2055)
  
  print(par_name)
  
  #We launch the simulation with the parameter set here
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))

  #While the simulation is running we are waiting
  sq <- system("squeue --format='%.40j' --me", intern=T)

  while(any(grepl(par_name, sq))){ #As long as the parameter file name is in the sq then wait. 
    print("Wait 3 min")
    Sys.sleep(180)
    sq <- system("squeue --format='%.40j' --me", intern=T)
  }
  
  #First we have to check that the azimuth bug is not present
  #here we will look in the last file of the simulation if the azimuth bug is present. 
  pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  if(pres_azi) print("AZIMUTH")
  
  while(pres_azi){
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  sq <- system("squeue --format='%.40j' --me", intern=T)
    while(any(grepl(par_name, sq))){ #As long as the parameter file name is in the sq then wait. 
      Sys.sleep(180)
      sq <- system("squeue --format='%.40j' --me", intern=T)
      print("wait 3 min azimuth")
    }
    
  #We check again here
    pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  }
  
  
  if(!pres_azi){
    print("2055:2085; Pas le bug de l'Aizimuth: no_azimuth")
  }


  #on lance 2085-2100
  par_name <- change_first_par_files(par_name=par_name, 
                                     growth = ferld_growth, rcp_f = rcp, period_f = 2085)
                                     
  print(par_name)                                   
  
  #We launch the simulation with the parameter set here
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  
  #While the simulation is running we are waiting
  sq <- system("squeue --format='%.40j' --me", intern=T)

  while(any(grepl(par_name, sq))){ #As long as the parameter file name is in the sq then wait. 
    print("Wait 3 min")
    Sys.sleep(180)
    sq <- system("squeue --format='%.40j' --me", intern=T)
  }
  
  pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  if(pres_azi) print("AZIMUTH")
  
  while(pres_azi){
  system(paste("sbatch -J", par_name, "submit_SORTIE.sh", paste0("../parameter_files/", par_name, ".xml")))
  sq <- system("squeue --format='%.40j' --me", intern=T)
    while(any(grepl(par_name, sq))){ #As long as the parameter file name is in the sq then wait. 
      print("wait 3 min azimuth")
      Sys.sleep(180)
      sq <- system("squeue --format='%.40j' --me", intern=T)
    }
    
    #We check here again
    pres_azi <- suppressWarnings(any(grepl("Azimuth", readLines(paste0("../out/", par_name, ".out")))))
  }
  
  
  if(!pres_azi){
    print("2085:2100; Pas le bug de l'Aizimuth: no_azimuth")
  }
  
  
  #We create a .txt for year 100, I won't use it to run other simus but it will be easier to combine the results after.
  map <- treemap_from_file(paste0("../output/", par_name, "_15.xml")) 
  map[["seedling"]]$DBH <- 0.5
  
  map <- bind_rows(map, .id="Type")%>% 
    mutate(Height=0) %>% 
    rename(Species=species, Diam=DBH) %>% 
    dplyr::select(X, Y, Species, Type, Diam, Height) %>% 
    mutate(Type=str_to_sentence(Type))
  
  tree_map_name <- paste0(substr(par_name, 1, nchar(par_name)-4), "2100.txt")
  write.table(x = map, file = paste0("../tree_maps/", tree_map_name), sep="\t", row.names=F, quote=F)
  print("C'est termine")
}