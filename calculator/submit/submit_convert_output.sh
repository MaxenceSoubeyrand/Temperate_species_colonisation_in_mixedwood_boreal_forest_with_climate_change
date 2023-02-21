#!/bin/bash
#SBATCH --time=16:00:00
#SBATCH --account=def-fgennare

#To launch when there is nothing left in the output
#Rendered all the simulations are finished or overtime and we go to the processing of the output

#We launch the Rscript which launches submit_convert_output_summary.sh
#and submit_convert_output_detailed.sh 
#And once the whole thing is finished it launches sbatch submit_combine_output_df.sh

module load gcc/9.3.0 r/4.0.2

Rscript execute_convert_output.R