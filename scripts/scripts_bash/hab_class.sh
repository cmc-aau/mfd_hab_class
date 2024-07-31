#!/usr/bin/bash -l
# Simple SLURM launcher for the snakemake pipeline that downloads and sets the input rasters.
# This script was made using mamba v1.1.0 and conda v22.11.1.

#SBATCH --job-name=hab_class
#SBATCH --output=job_%j_%x.out
#SBATCH --error=job_%j_%x.err
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --time=7-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=frde@bio.aau.dk


# The script will try to detect the folder where it is using the following line. This might fail depending on the cluster configuration. If this happens the path can be hardcoded (absolute path) and stored as "SCRIPT_DIR"
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#SCRIPT_DIR="/home/bio.aau.dk/wz65bi/mfd_hab_class/scripts/scripts_bash" # Harcoded path on the cmc cluster
REPO_DIR=$SCRIPT_DIR/../..

MAMBA_INSTALLATION_DIR=`mamba info | awk '/base environment/{print $4}'`

export ENV_DIR=$REPO_DIR/envs
export INPUT_DIR=$REPO_DIR/data
export CONFIG_DIR=$REPO_DIR/config
export PYTHON_DIR=$REPO_DIR/scripts/scripts_python
export DOWNLOAD_DIR=$REPO_DIR/data/download
export OUTPUT_DIR=$REPO_DIR/analysis
export SCRIPTS_DIR=$REPO_DIR/scripts

source $MAMBA_INSTALLATION_DIR/etc/profile.d/mamba.sh
source $MAMBA_INSTALLATION_DIR/etc/profile.d/conda.sh

echo $ENV_DIR

if [[ ! -e $ENV_DIR/snakemake ]]; then

    	mamba env create -f $ENV_DIR/snakemake.yml -p $ENV_DIR/snakemake

fi

mamba activate "$ENV_DIR/snakemake"

snakemake --version

cd $REPO_DIR

#         --immediate-submit --notemp \

#snakemake --unlock -s $PYTHON_DIR/variable_selection.snakemake

snakemake --cores 1 --unlock --rerun-incomplete \
        -p --jobs 100 \
        --notemp \
        --rerun-triggers mtime \
        --use-conda --conda-prefix $ENV_DIR \
        --configfile $CONFIG_DIR/hab_class.yaml \
        --profile $CONFIG_DIR \
        -s $PYTHON_DIR/hab_class.snakemake

snakemake --cores 1 --rerun-incomplete \
	-p --jobs 100 \
	--notemp \
	--rerun-triggers mtime \
	--use-conda --conda-prefix $ENV_DIR \
	--configfile $CONFIG_DIR/hab_class.yaml \
	--profile $CONFIG_DIR \
        -s $PYTHON_DIR/hab_class.snakemake





