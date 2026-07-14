#!/bin/bash
#SBATCH --account=def-jcaplan
#SBATCH --job-name=harrier27b_embed
#SBATCH --output=/home/devon7y/scratch/devon7y/harrier_semanticfa/logs/%x_%j.out
#SBATCH --error=/home/devon7y/scratch/devon7y/harrier_semanticfa/logs/%x_%j.err
#SBATCH --time=00:45:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=120G
#SBATCH --gpus=h100:1

set -euo pipefail
module load python/3.11

WORK=/home/devon7y/scratch/devon7y/harrier_semanticfa
export HF_HOME=$WORK/hf
export HF_HUB_OFFLINE=1
export OMP_NUM_THREADS=12

# fresh venv on node-local NVMe (Lustre small-file pathology workaround)
python -m venv "$SLURM_TMPDIR/venv"
source "$SLURM_TMPDIR/venv/bin/activate"
pip install --quiet --upgrade pip
# everything from the Alliance wheelhouse (local CVMFS): transformers 5.12.1
# and sentence_transformers 5.2.0 cover gemma3_text; zero network exposure
pip install --quiet --no-index --no-cache-dir torch numpy sentence_transformers transformers requests certifi msgpack pillow accelerate

nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
python "$WORK/embed_harrier.py"
echo "JOB_EXIT:$?"
