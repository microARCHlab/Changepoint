#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=ChangePoint_subset100k # job name
#SBATCH -o ChangePoint_aAMR-HunDC_subset_20250218.out
#SBATCH -t 72:00:00
#SBATCH --mem=100gb
#SBATCH --cpus-per-task=10
#SBATCH --account=lsw132
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cjt5751@psu.edu
############################
# Creating folder and getting files
############################

#change directory to directory where job is submitted
cd $SLURM_SUBMIT_DIR


#copy *fastq files from file location
# cp /storage/home/cjt5751/lsw132_group/04_LabMembersFolders/ChristineTa/2023_AncientResistome/01_SampleRawSequences/China_YellowRiver/2024Dec05_CHDC_AdapterRemovalUpdate/	KEEP_Asia_China_AllProccessedCHDC_Samples/* ./
############################
# Subsample to 100k reads using seqtk
############################

#install seqtk
git clone https://github.com/lh3/seqtk.git;
cd seqtk; make

# Define path for seqtk command if seqtk is not install into bin directory, if you successfully installed seqtk in your main bin folder, can just use command seqtk sample
seqtk=/storage/group/LiberalArts/default/lsw132_collab/04_LabMembersFolders/ChristineTa/2023_AncientResistome/03_Analysis/Aim1_Analysis/01_aDNA_Authentication/01_Changepoint/KEEP_Asia_China_AllProccessedCHDC_Samples/seqtk

for i in *.fastq;

do 

ID=$(basename $i)
NAME=$(echo $ID |cut -d "." -f1)

echo "Running seqtk on" $ID
echo $NAME

$seqtk/seqtk sample -s12345 $i 100000 > ${NAME}_100k.fastq

done
