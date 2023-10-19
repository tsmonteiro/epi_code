
for I in {0..50}
do
  let "S = I * 50 +1"
  echo "${I}/50"
  LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/GSE196696_PROCESS_%03d.log" $S)
  Rscript main_intensities.R $S 50 GSE196696 "EpiMatch DNA Methylation Resource" > $LOG_OUT
done