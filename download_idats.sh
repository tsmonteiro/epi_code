#!/bin/bash
for I in {0..10}
do
    let "SA = I * 200 +1"
    let "SB = (I+1) * 200"
    
    GEOID=GSE210255
    GEODESC="Methylation data from stored peripheral blood leukocytes from African American participants in the GENOA study with Infinium HumanMethylationEPIC BeadChip"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE147740
    GEODESC="DNA methylation analysis of human peripheral blood mononuclear cell collected in the AIRWAVE study"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE179325
    GEODESC="COVID-19 DNA methylation analysis"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE197675
    GEODESC="Genome-wide association studies identify novel genetic loci for epigenetic age acceleration among survivors of childhood cancer [Metadata_SJLIFE2_502]"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    
    GEOID=GSE154683
    GEODESC="Epigenomic profiles of African American Transthyretin Val122Ile carriers reveals putatively dysregulated amyloid mechanisms"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
        
    GEOID=GSE164056
    GEODESC="DNA Methylation Differences Associated with Social Anxiety Disorder and Early Life Adversity"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE155426
    GEODESC="DNA Methylation Patterns of Chronic Explosive Breaching in U.S. Military Warfighters"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE123914
    GEODESC="Variation in DNA methylation of human blood over a 1-year period using the Illumina MethylationEPIC array"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE143307
    GEODESC="Epigenomics of vitamin K response to supplementation"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    GEOID=GSE141682
    GEODESC="DNA methylation profiling of whole blood from Han Chinese individuals: identification of age-related CpG sites for forensic use"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT

    GEOID=GSE132203
    GEODESC="DNA Methylation (EPIC) from the Grady Trauma Project"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT

    GEOID=GSE167202
    GEODESC="Host methylation predicts SARS-CoV-2 infection and clinical outcome"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
    
    GEOID=GSE199700
    GEODESC="Fourteen-weeks combined exercise epigenetically modulated 118 genes of menopausal women with prediabetes"
    echo "Downloading ${GEOID} from ${SA} to ${SB}"
    LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_DOWNLOAD_%04d_%04d.log" $SA $SB)
    Rscript donload_idats.R ${SA} ${SB} ${GEOID} $GEODESC > $LOG_OUT
    
done




