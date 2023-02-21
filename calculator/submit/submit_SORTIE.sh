#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --account=def-fgennare
#SBATCH --output=../out/%x.out
#SBATCH --mem-per-cpu=8G

module load StdEnv/2018.3
wine "/project/6071506/msoubeyr/bin/coremodel.exe" $1
