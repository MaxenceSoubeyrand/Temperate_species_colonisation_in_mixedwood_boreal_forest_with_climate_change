#!/bin/bash
#SBATCH --time=23:59:00
#SBATCH --account=def-fgennare

#We do not do anything until it is finished i.e. the sq is not empty

while [ `sq | wc -l` -ne 2 ]; do
        sleep 5m
done


#On combine tout dans le même fichier
sbatch submit_convert_output.sh