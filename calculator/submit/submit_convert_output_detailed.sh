#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --account=def-fgennare
#SBATCH --mem-per-cpu=4G
#SBATCH --output=../out/%j.out

module load stdEnv/2020

module load gcc/9.3.0 r/4.0.2 

Rscript "read_output_detailed.R" $parameter_file