#!/usr/bin/env bash
# DIRECTIVES:
#SBATCH --job-name=testing_%J
#SBATCH --output=automated_testing/testing_%J.out
#SBATCH --error=automated_testing/testing_%J.err
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --qos=normal
#SBATCH --wckey=um-rose-stem-spawn
#SBATCH --mem=2048
#SBATCH --time=02:00:00

. /etc/profile
for var in "$@"; do
    $var
done