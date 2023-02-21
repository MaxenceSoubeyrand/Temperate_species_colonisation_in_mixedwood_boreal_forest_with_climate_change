#Script that runs all the result readings and puts them in an array
rm(list = ls())

library(tidyverse)


system(paste0("sbatch submit_convert_output_summary.sh")) #For each of the cond init we launch the simulations

par_files <- dir("../parameter_files/")


#We partition in 4 so that the tail is not too crowded
par_files_1 <- par_files[1:(length(par_files)/4)]
par_files_2 <- par_files[(length(par_files)/4+1):(length(par_files)/4*2)]
par_files_3 <- par_files[(length(par_files)/4*2+1):(length(par_files)/4*3)]
par_files_4 <- par_files[(length(par_files)/4*3+1):(length(par_files)/4*4)]


for(j in par_files_1){ 
  system(paste0("sbatch -J ", j, " --export=parameter_file=../parameter_files/'", j,"' submit_convert_output_detailed.sh")) #For each of the cond init we launch the simulations
} #OK

sq <- system("squeue --format='%.40j' --me", intern=T)

while(length(sq)>200){
  Sys.sleep(180)
  sq <- system("squeue --format='%.40j' --me", intern=T)
}

for(j in par_files_2){ 
  system(paste0("sbatch -J ", j, " --export=parameter_file=../parameter_files/'", j,"' submit_convert_output_detailed.sh")) #For each of the cond init we launch the simulations
} #OK

sq <- system("squeue --format='%.40j' --me", intern=T)

while(length(sq)>200){
  Sys.sleep(180)
  sq <- system("squeue --format='%.40j' --me", intern=T)
}

for(j in par_files_3){ 
  system(paste0("sbatch -J ", j, " --export=parameter_file=../parameter_files/'", j,"' submit_convert_output_detailed.sh")) #For each of the cond init we launch the simulations
} #OK

sq <- system("squeue --format='%.40j' --me", intern=T)

while(length(sq)>200){
  Sys.sleep(180)
  sq <- system("squeue --format='%.40j' --me", intern=T)
}

for(j in par_files_4){ 
  system(paste0("sbatch -J ", j, " --export=parameter_file=../parameter_files/'", j,"' submit_convert_output_detailed.sh")) #For each of the cond init we launch the simulations
} #OK

sq <- system("squeue --format='%.40j' --me", intern=T)

while(length(sq)!=2){
  Sys.sleep(180)
  sq <- system("squeue --format='%.40j' --me", intern=T)
}

system(paste0("sbatch submit_combine_output_df.sh"))