#!/bin/sh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=victor.vandermeersch@cefe.cnrs.fr
#SBATCH --job-name=CMAES3
#SBATCH --output=/scratch/vvandermeersch/phenofit_calibration_3/logs/CMAES_log.txt
#SBATCH --cpus-per-task=75
#SBATCH --mem=110G
#SBATCH --time 3-20:00:00

. /local/env/envconda.sh

conda activate /home/genouest/mnhn_cesco/vvandermeersch/env/env_capsis_cmaes

Rscript calibration.R
