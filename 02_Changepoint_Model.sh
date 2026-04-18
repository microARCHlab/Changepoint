#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=ChangePoint_script # job name
#SBATCH -o ChangePoint_aAMR-HuDC_20250218.out
#SBATCH -t 72:00:00
#SBATCH --mem=100gb
#SBATCH --cpus-per-task=10
#SBATCH --account=lsw132
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cjt5751@psu.edu

cd $SLURM_SUBMIT_DIR

#Description:This script will run ChangePoint on input files

#Usage:Count the proportion and frequencies of ATCG from 5' and 3' ends of the reads of a fastA or fastQ file

#Number of bases from the sequence termini you want to look at
N_bp=25
#Number of bases to trim at the end
TRIM=1

Count_ATCG(){
  FILE=$1
  N_bp=$2
  TRIM=$3
  
  #SEQ=`cat $FILE | awk 'NR%2==0{print}' ` #fastA file input; use zcat if  data is compressed
  SEQ=`cat $1 | awk 'NR%4==2{print}' ` #fastQ file input; use zcat if  data is compressed
  
  #echo $SEQ | tr " " \\n | cut -c $N_bp | grep -c A 
  
  for ((i=$((TRIM + 1)); i<=${N_bp}; i++)); do 
  A_freq_5=$(echo $SEQ | tr " " \\n | cut -c ${i} | grep -c A)
  T_freq_5=$(echo $SEQ | tr " " \\n | cut -c ${i} | grep -c T)
  C_freq_5=$(echo $SEQ | tr " " \\n | cut -c ${i} | grep -c C)
  G_freq_5=$(echo $SEQ | tr " " \\n | cut -c ${i} | grep -c G)
  Tot_freq_5=$(echo $SEQ | tr " " \\n | cut -c ${i} | wc -l)
  echo -e $i"\t"${A_freq_5}"\t"${T_freq_5}"\t"${C_freq_5}"\t"${G_freq_5}"\t"${Tot_freq_5} >>  ${FILE/\.fastq}_5_end_freq
  
  A_freq_3=$(echo $SEQ | tr " " \\n | rev | cut -c ${i} | grep -c A)
  T_freq_3=$(echo $SEQ | tr " " \\n | rev | cut -c ${i} | grep -c T)
  C_freq_3=$(echo $SEQ | tr " " \\n | rev | cut -c ${i} | grep -c C)
  G_freq_3=$(echo $SEQ | tr " " \\n | rev | cut -c ${i} | grep -c G)
  Tot_freq_3=$(echo $SEQ | tr " " \\n | rev | cut -c ${i} |cut -c ${i} | wc -l)
  echo -e $i"\t"${A_freq_3}"\t"${T_freq_3}"\t"${C_freq_3}"\t"${G_freq_3}"\t"${Tot_freq_3} >>  ${FILE/\.fastq}_3_end_freq
  done
  
  (echo -e Position_from_5end"\t"A_freq"\t"T_freq"\t"C_freq"\t"G_freq"\t"Total && cat ${FILE/\.fastq}_5_end_freq) \
  >  ${FILE/\.fastq}_5_end_freq1 && mv  ${FILE/\.fastq}_5_end_freq1 ${FILE/\.fastq}_5_end_freq #add the header
  
  (echo -e Position_from_3end"\t"A_freq"\t"T_freq"\t"C_freq"\t"G_freq"\t"Total && cat ${FILE/\.fastq}_3_end_freq) \
  >  ${FILE/\.fastq}_3_end_freq1 && mv  ${FILE/\.fastq}_3_end_freq1 ${FILE/\.fastq}_3_end_freq #add the header	
}

export -f Count_ATCG

# parallel version
# for i in `ls *fasta`; do
# 	echo Count_ATCG $i $N_bp $TRIM
# done | 	parallel -j $CORES

#if you dont want to use parallel, use this loop instead
for i in `ls *fastq`; do
Count_ATCG $i $N_bp $TRIM
done 

#make sure either fasta or fastq
wait
for i in `ls *freq`; do \
cat $i | awk 'BEGIN{OFS="\t"}(NR>1){print $1, $2/$6, $3/$6, $4/$6, $5/$6}' > ${i/_freq/_prop} ;\
done

wait
for i in `ls *5_end_prop`; do \
(echo -e Position_from_5end"\t"A"\t"T"\t"C"\t"G && cat $i) > ${i}1 && mv ${i}1 $i ;\
done	

for i in `ls *3_end_prop`; do \
(echo -e Position_from_3end"\t"A"\t"T"\t"C"\t"G && cat $i) > ${i}1 && mv ${i}1 $i ;\
done	


############################
# Group Files
############################
mkdir prop
mkdir freq

mv *_prop prop
mv *_freq freq
