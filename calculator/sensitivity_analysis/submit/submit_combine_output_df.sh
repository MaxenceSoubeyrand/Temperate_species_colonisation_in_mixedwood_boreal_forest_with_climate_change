#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --account=def-fgennare
#SBATCH --mem-per-cpu=128G
#SBATCH --output=combine_detailed.out

module load stdEnv/2020

module load gcc/9.3.0 r/4.0.2 

Rscript "combine_output_df.R" 