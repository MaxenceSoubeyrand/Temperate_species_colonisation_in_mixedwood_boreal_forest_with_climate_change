#!/bin/bash
#SBATCH --time=16:00:00
#SBATCH --account=def-fgennare
#SBATCH --output=../out/%x.out
#SBATCH --mem-per-cpu=4G

module load gcc/9.3.0 r/4.0.2

echo $vars
Rscript execute_one_set_simulation.R $vars