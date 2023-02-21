#!/bin/sh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=victor.vandermeersch@cefe.cnrs.fr
#SBATCH --job-name=args
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time 0-01:00:00

. /local/env/envconda.sh

conda activate /home/genouest/mnhn_cesco/vvandermeersch/env/env_capsis_cmaes

Rscript test.R $1
