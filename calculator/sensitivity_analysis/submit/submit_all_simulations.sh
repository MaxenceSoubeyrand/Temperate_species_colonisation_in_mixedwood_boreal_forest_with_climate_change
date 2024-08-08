#!/bin/bash
#SBATCH --time=0:30:00
#SBATCH --account=def-fgennare

cd "submit/"

rm -r ../out #remove folder out (where the unix outputs go)
rm -r ../output #remove folder output (where the sortie outputs go)
rm -r ../parameter_files #remove folder parameter_files (where the parameter_files create during the simualtion runs go)
rm -r ../output_df #remove folder ouput_df (where the results as data frame create during the simualtion runs go)

cd ../tree_maps
cd ../submit

mkdir ../out #recreate the out folder
mkdir ../output #recreate the output folder
mkdir ../parameter_files #recreate the parameter_files folder
mkdir ../output_df #recreate the output_df folder

module load gcc/9.3.0 r/4.0.2 #load module to run R

Rscript execute_all_simulations.R #Run rscript which run all simulations