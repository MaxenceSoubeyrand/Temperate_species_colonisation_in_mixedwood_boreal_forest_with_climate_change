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

create_parameter_file <- function(fire_year = 1760, ssp_f = "ssp26", #2025 means 2025-2055, 2055 means 2055-2085 and 2085 means 2085-2100
                                 patch_size= "10", species_test_f = "ERS"){
  
  # ssp_f = "ssp126"; fire_year = 1760; patch_size = "10"; species_test_f = "ERS"

  
  #opens the original parameter file. 
  par_xml <- read_xml("../data/Parameter_file.xml")
  par_list <- as_list(par_xml)
  
  simulation_name <- paste(fire_year, species_test_f, patch_size, ssp_f, sep="_")
  
  #Change the size of the plot:
  par_list[["paramFile"]][["plot"]][["plot_lenX"]][[1]] <- 100
  par_list[["paramFile"]][["plot"]][["plot_lenY"]][[1]] <- 100
  
  
  
  tree_map_name <- paste0(fire_year, "_", species_test_f, "_", patch_size, "_", ssp_f, "_1991", ".txt")
  
  #Put the link in the paramater file:
  par_list$paramFile$trees$tr_treemapFile[[1]] <- paste0("/project/6071506/msoubeyr/chap3_new/tree_maps/", 
                                                         tree_map_name)
  #Empty seedling density table to avoid duplicating tree maps  dens_init_vide <- data.frame(plot_id="1760", year=1991, 
                               species_id="ABA", dbh_class=10, density=0)
  
  
  
  # Produce new tree initial density info
  dens_new <- get_transect_init_dens(data=dens_init_vide) #plot=i; plot_id=i
  
  # Replace density info in original list
  par_list$paramFile$trees$tr_initialDensities <- dens_new
  
  # Change number of timesteps
  par_list$paramFile$plot$timesteps[[1]] <- as.character(110)
  
  # Change output filenames
  
  out_dir <- paste0("/project/6071506/msoubeyr/chap3_new/output/", simulation_name, "/")
  par_dir <- "/project/6071506/msoubeyr/chap3_new/parameter_files/" #L? o? les fichiers de param?tres vont ?tre dirig?s
  
  par_list$paramFile$Output$ou_filename[[1]] <- paste0(out_dir, simulation_name, ".gz.tar")
  par_list$paramFile$ShortOutput$so_filename[[1]] <- paste0(out_dir, simulation_name, ".out")
  
  #Change the climate following climate change scenario. 
  #Opening climate change parameter
  coef_FERLD_climate <- readRDS("/project/6071506/msoubeyr/chap3_new/data/coef_FERLD_climate.rds") #IL MANQUE LE CONTRÔLE!!!!!!
  
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
  
  
  # Convert back to XML format and save in new file
  par_xml_new <- as_xml_document(par_list)
  write_xml(par_xml_new, paste0(par_dir, simulation_name, ".xml"))
  
  return(simulation_name)
}