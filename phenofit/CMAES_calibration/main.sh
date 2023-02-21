#!/bin/sh
#SBATCH --output=/dev/null

sbatch --output=$1 test.sh $1
