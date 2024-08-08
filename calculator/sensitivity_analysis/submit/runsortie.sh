#!/bin/bash

echo $1

module load StdEnv/2018.3
wine "coremodel.exe"  $1
