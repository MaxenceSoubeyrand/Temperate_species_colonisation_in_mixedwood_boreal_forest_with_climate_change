# setwd("~/PhD/chap3/new_simulation/simulation")
# library(xml2)


#####Keep this function to give a empty table of initial density since we use tree maps. 
get_transect_init_dens <- function(data, plot, yr) {
  # This data frame is to translate between the species_id in the transect data
  #  and the species_name in the SORTIE model
  sp_tab <- data.frame(
    species_name = c("White_Cedar", "Balsam_Fir", "Mountain_Maple", "White_Spruce",
                     "Jack_Pine", "Trembling_Aspen", "Paper_Birch", "Sugar_Maple", 
                     "Red_Maple", "Yellow_Birch"),
    species_id = c("TOC", "ABA", "ASP", "PGL", "PBA", "PTR", "BPA", "ERS", "ERR", "BOJ")
  )
  
  dens_df <- data %>%
    # join with sp_tab to get species names
    inner_join(sp_tab, by = "species_id") %>%
    mutate(
      # change species_name to factor to get same ordering of species 
      #  as sORTIE file (not sure if that's important)
      species_name = factor(species_name, levels = sp_tab$species_name),
      # convert DBH class to size classes written as in SORTIE file (e.g.: 10 becomes s10.0)
      size_class = ifelse(dbh_class == 0.5, "Seedling", 
                          paste0("s", dbh_class, ".0")),
      # get density and convert density written as in SORTIE file
      density = paste0(density, ".0")
    ) %>%
    # sort by species and dbh_class to get same ordering as original parameter file
    #  (maybe not needed)
    arrange(species_name, dbh_class) %>% ungroup() %>%
    dplyr::select(species_name, size_class, density)
  
  # nest the data by species (so dens_nest has one row by species, 
  #  and the 2nd column contains one data frame by species)
  dens_nest <- nest_by(dens_df, species_name, .key = "size_density_data") %>%
    # retransform species name to character (not factor)
    mutate(species_name = as.character(species_name))
  
  # this creates the initial tree density list for one species (one row of dens_nest)
  create_init_dens_list <- function(species_name, size_density_data) {
    # for each row in size_density_data, produces a list of one element (the density)
    #  with one attribute (the size class)
    init_dens <- pmap(size_density_data,
                      function(size_class, density) structure(list(density), sizeClass = size_class))
    # set the name of every element of the list to "tr_initialDensity" to match SORTIE param file
    init_dens <- set_names(init_dens, rep("tr_initialDensity", length(init_dens)))
    # structure adds the whatSpecies attribute (species name) to the list
    structure(init_dens, whatSpecies = species_name)
  }
  
  # apply the function above to all species
  dens_nest_list <- pmap(dens_nest, create_init_dens_list)
  # name every element of list as "tr_idVals" to match SORTIE parameter file
  set_names(dens_nest_list, rep("tr_idVals", length(dens_nest_list)))
}

################################################################################

create_parameter_file <- function(fire_year = 1964, ssp_f = "ssp245",
                                 patch_size= "150", species_test_f = "ERS",
                                 parameter="a", par_value="0.34"){
  
  par_xml <- read_xml("../data/Parameter_file.xml")
  par_list <- as_list(par_xml)
  
  simulation_name <- paste(fire_year, species_test_f, patch_size, ssp_f, parameter, par_value, sep="_")
  
  #Change the size of the plot:
  par_list[["paramFile"]][["plot"]][["plot_lenX"]][[1]] <- 100
  par_list[["paramFile"]][["plot"]][["plot_lenY"]][[1]] <- 100
  
  
  
  tree_map_name <- paste0(fire_year, "_", species_test_f, "_", patch_size, "_", 
                          ssp_f, "_", parameter, "_", par_value, "_1991", ".txt")
  
  #Put the link in the paramater file:
  par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0("/project/6071506/msoubeyr/chap3_sensitivity_new/tree_maps/", 
                                                         tree_map_name)
  #Empty seedling density table to avoid duplicating tree maps
  dens_init_vide <- data.frame(plot_id="1760", year=1991, 
                               species_id="ABA", dbh_class=10, density=0)
  
  
  
  # Produce new tree initial density info
  dens_new <- get_transect_init_dens(data=dens_init_vide) #plot=i; plot_id=i
  
  # Replace density info in original list
  par_list$paramFile$trees$tr_initialDensities <- dens_new
  
  # Change number of timesteps
  par_list$paramFile$plot$timesteps[[1]] <- as.character(110)
  
  # Change output filenames
  
  out_dir <- paste0("/project/6071506/msoubeyr/chap3_sensitivity_new/output/", simulation_name, "/")
  par_dir <- "/project/6071506/msoubeyr/chap3_sensitivity_new/parameter_files/" #L? o? les fichiers de param?tres vont ?tre dirig?s
  
  par_list$paramFile$Output$ou_filename[[1]] <- paste0(out_dir, simulation_name, ".gz.tar")
  par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0(out_dir, simulation_name, ".out")
  
  #Change the climate following climate change scenario. 
  #Opening climate change parameter
  #coef_FERLD_climate <- readRDS("~/PhD/chap3/new_simulation/sensitivity_analysis/data/coef_FERLD_climate.rds")
  coef_FERLD_climate <- readRDS("/project/6071506/msoubeyr/chap3_sensitivity_new/data/coef_FERLD_climate.rds")
  
  #1. Change mean annual temperature and mean annual precipitation (T1)
  par_list[["paramFile"]][["plot"]][["plot_precip_mm_yr"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="MSP")$T1) #T1 prec

  par_list[["paramFile"]][["plot"]][["plot_temp_C"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="tave")$T1) #T1 temp


  #2. Change the other parameters (B et C)
  par_list[["paramFile"]][["ClimateChange1"]][["sc_climateChangePrecipB"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="MSP")$B) #B prec
  par_list[["paramFile"]][["ClimateChange1"]][["sc_climateChangePrecipC"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="MSP")$C) #C prec


  par_list[["paramFile"]][["ClimateChange2"]][["sc_climateChangeTempB"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="tave")$B) #B temp

  par_list[["paramFile"]][["ClimateChange2"]][["sc_climateChangeTempC"]][[1]] <-
    as.character(dplyr::filter(coef_FERLD_climate, ssp==ssp_f, climate_variable=="tave")$C) #C temp
  
  #Changes parameter value for sensitivity analysis
  options( "scipen"=100) 
  
  #change parameter value 
  if(parameter=="STR"){
    par_list$paramFile$GapDisperse20$di_weibullCanopySTR[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullCanopySTR[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullCanopySTR[[10]][1] <- as.character(as.numeric(par_value))
    
    par_list$paramFile$GapDisperse20$di_weibullGapSTR[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullGapSTR[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullGapSTR[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="d"){
    par_list$paramFile$GapDisperse20$di_weibullCanopyDispersal[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullCanopyDispersal[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullCanopyDispersal[[10]][1] <- as.character(as.numeric(par_value))
    
    par_list$paramFile$GapDisperse20$di_weibullGapDispersal[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullGapDispersal[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$GapDisperse20$di_weibullGapDispersal[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="x0"){
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectX0[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectX0[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectX0[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="xb"){
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectXb[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectXb[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciSizeEffectXb[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="m"){
    par_list$paramFile$NCIMasterGrowth8$nciShadingCoefficient[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciShadingCoefficient[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciShadingCoefficient[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="c"){
    par_list$paramFile$NCIMasterGrowth8$nciCrowdingC[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciCrowdingC[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciCrowdingC[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="alpha"){
    par_list$paramFile$NCIMasterGrowth8$nciAlpha[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciAlpha[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciAlpha[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="beta"){
    par_list$paramFile$NCIMasterGrowth8$nciBeta[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciBeta[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciBeta[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="a"){
    par_list$paramFile$AbsUnlimGrowth9$gr_asympDiameterGrowth[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$AbsUnlimGrowth9$gr_asympDiameterGrowth[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$AbsUnlimGrowth9$gr_asympDiameterGrowth[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="s"){
    par_list$paramFile$AbsUnlimGrowth9$gr_slopeGrowthResponse[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$AbsUnlimGrowth9$gr_slopeGrowthResponse[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$AbsUnlimGrowth9$gr_slopeGrowthResponse[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  #Mortalité
  if(parameter=="z"){
    par_list$paramFile$CompetitionMortality16$mo_CompMort[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$CompetitionMortality16$mo_CompMort[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$CompetitionMortality16$mo_CompMort[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="max"){
    par_list$paramFile$CompetitionMortality16$mo_CompMortMax[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$CompetitionMortality16$mo_CompMortMax[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$CompetitionMortality16$mo_CompMortMax[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="randadult"){
    par_list$paramFile$StochasticMortality12$mo_stochasticMortRate[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$StochasticMortality12$mo_stochasticMortRate[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$StochasticMortality12$mo_stochasticMortRate[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="randjuv"){
    par_list$paramFile$StochasticMortality13$mo_stochasticMortRate[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$StochasticMortality13$mo_stochasticMortRate[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$StochasticMortality13$mo_stochasticMortRate[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="M2"){
    par_list$paramFile$BCMortality17$mo_lightDependentMortality[[8]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$BCMortality17$mo_lightDependentMortality[[9]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$BCMortality17$mo_lightDependentMortality[[10]][1] <- as.character(as.numeric(par_value))
  }
  
  #MaxPotGrowth and tree growth climate parameters
  if(parameter=="MaxPotGrowth"){
    print(par_value)
    par_list$paramFile$NCIMasterGrowth8$gr_nciMaxPotentialGrowth[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$gr_nciMaxPotentialGrowth[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$gr_nciMaxPotentialGrowth[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="Atemp"){
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffA[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffA[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffA[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="Ctemp"){
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffC[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffC[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibTempEffC[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="Aprec"){
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffA[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffA[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffA[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  if(parameter=="Cprec"){
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffC[[6]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffC[[7]][1] <- as.character(as.numeric(par_value))
    par_list$paramFile$NCIMasterGrowth8$nciWeibPrecipEffC[[8]][1] <- as.character(as.numeric(par_value))
  }
  
  
  # Convert back to XML format and save in new file
  par_xml_new <- as_xml_document(par_list)
  write_xml(par_xml_new, paste0(par_dir, simulation_name, ".xml"))
  
  return(simulation_name)
}