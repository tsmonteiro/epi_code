#devtools::install_github("yiluyucheng/dnaMethyAge")
#https://github.com/danbelsky/DunedinPACE
#devtools::install_github("danbelsky/DunedinPACE")
#devtools::install_github("danbelsky/DunedinPoAm38")
library("dnaMethyAge")
#library(DunedinPACE)
library("DunedinPoAm38")
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)

source("data_funcs.R")
usable_clocks <- suppressMessages(availableClock())
data(list = "DunedinPACE", envir = environment())
dunedinProbes <- names(gold_standard_means)

# SMOKING <- Hypo is better
dunedinProbes <- append(dunedinProbes, "cg05575921")
dunedinProbes <- append(dunedinProbes, "cg03636183")
dunedinProbes <- append(dunedinProbes, "cg21566642")
dunedinProbes <- append(dunedinProbes, "cg01940273")
dunedinProbes <- append(dunedinProbes, "cg05951221")
dunedinProbes <- append(dunedinProbes, "cg21161138")
dunedinProbes <- append(dunedinProbes, "cg25648203")
dunedinProbes <- append(dunedinProbes, "cg25189904")
dunedinProbes <- append(dunedinProbes, "cg06126421")
dunedinProbes <- append(dunedinProbes, "cg06644428")

smokeProbes <- c("cg05575921","cg03636183", "cg21566642","cg01940273", "cg05951221",
                 "cg21161138","cg25648203","cg25189904","cg06126421","cg06644428")
smokeProbeTypes <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

#BMI
dunedinProbes <- append(dunedinProbes, "cg00574958") # HIPO
dunedinProbes <- append(dunedinProbes, "cg18181703") # HIPO
dunedinProbes <- append(dunedinProbes, "cg17501210") # HIPO
dunedinProbes <- append(dunedinProbes, "cg06500161") # HIPER
dunedinProbes <- append(dunedinProbes, "cg11024682") # HIPER
dunedinProbes <- append(dunedinProbes, "cg09664445") # HIPER
dunedinProbes <- append(dunedinProbes, "cg13708645") # HIPER
dunedinProbes <- append(dunedinProbes, "cg25178683") # HIPER
dunedinProbes <- append(dunedinProbes, "cg26403843") # HIPER

bmiProbes <- c("cg00574958","cg18181703","cg17501210","cg06500161","cg11024682",
               "cg09664445","cg13708645","cg25178683","cg26403843")
bmiProbeTypes <- c(-1,-1,-1,1,1,1,1,1,1)

# ALCOOL
dunedinProbes <- append(dunedinProbes, "cg26213873") # HIPER
dunedinProbes <- append(dunedinProbes, "cg18634443") # HIPER
dunedinProbes <- append(dunedinProbes, "cg05497240") # HIPER
dunedinProbes <- append(dunedinProbes, "cg09966309") # HIPO
dunedinProbes <- append(dunedinProbes, "cg03461296") # HIPER
dunedinProbes <- append(dunedinProbes, "cg15090909") # HIPER
dunedinProbes <- append(dunedinProbes, "cg07832337") # HIPO
dunedinProbes <- append(dunedinProbes, "cg20258580") # HIPER
dunedinProbes <- append(dunedinProbes, "cg03936229") # HIPER


alcoholProbes <- c("cg26213873","cg18634443","cg05497240","cg09966309","cg03461296",
                   "cg15090909","cg07832337","cg20258580","cg03936229")
alcoholProbeTypes <- c(1,1,1,-1,1,1,-1,1,1)

# CONSUMO DE FRUTA
dunedinProbes <- append(dunedinProbes, "cg05364411") # HIPER
dunedinProbes <- append(dunedinProbes, "cg24423329") # HIPO
dunedinProbes <- append(dunedinProbes, "cg04166618") # HIPER
dunedinProbes <- append(dunedinProbes, "cg13701509") # HIPO
dunedinProbes <- append(dunedinProbes, "cg06801252") # HIPER
dunedinProbes <- append(dunedinProbes, "cg09033857") # HIPER
dunedinProbes <- append(dunedinProbes, "cg13041975") # HIPER
dunedinProbes <- append(dunedinProbes, "cg03768517") # HIPO
dunedinProbes <- append(dunedinProbes, "cg03163184") # HIPO
dunedinProbes <- append(dunedinProbes, "cg08605301") # HIPER

fruitProbes <- c("cg05364411","cg24423329","cg04166618","cg13701509","cg06801252",
                 "cg09033857","cg13041975","cg03768517","cg03163184","cg08605301")
fruitProbeTypes <- c(1,-1,1,-1,1,1,1,-1,-1,1)

#EXERCICIO foi cortado por so ter sondas de musculo e gordura
# Usando capacidade pulmonar como um distante proxy
dunedinProbes <- append(dunedinProbes, "cg05575921") # HIPER
dunedinProbes <- append(dunedinProbes, "cg15342087") # HIPER
dunedinProbes <- append(dunedinProbes, "cg13993467") # HIPO
dunedinProbes <- append(dunedinProbes, "cg18181703") # HIPER
dunedinProbes <- append(dunedinProbes, "cg05951221") # HIPER
dunedinProbes <- append(dunedinProbes, "cg03636183") # HIPER
dunedinProbes <- append(dunedinProbes, "cg23759053") # HIPO
dunedinProbes <- append(dunedinProbes, "cg15909232") # HIPO
dunedinProbes <- append(dunedinProbes, "cg07219303") # HIPER
dunedinProbes <- append(dunedinProbes, "cg03559389") # HIPO


lungProbes <- c("cg05575921","cg15342087","cg13993467","cg18181703","cg05951221",
                 "cg03636183","cg23759053","cg15909232","cg07219303","cg03559389")
lungProbeTypes <- c(1,1,-1,1,1,1,-1,-1,1,-1)

# SONO INSUFICIENTE
dunedinProbes <- append(dunedinProbes, "cg00032756") # HIPO
dunedinProbes <- append(dunedinProbes, "cg18958202") # HIPO
dunedinProbes <- append(dunedinProbes, "cg11701615") # HIPO
dunedinProbes <- append(dunedinProbes, "cg12484487") # HIPO
dunedinProbes <- append(dunedinProbes, "cg10094651") # HIPO
dunedinProbes <- append(dunedinProbes, "cg26091928") # HIPO
dunedinProbes <- append(dunedinProbes, "cg11412248") # HIPO
dunedinProbes <- append(dunedinProbes, "cg05894754") # HIPER
dunedinProbes <- append(dunedinProbes, "cg20135711") # HIPER
dunedinProbes <- append(dunedinProbes, "cg21246271") # HIPO


sleepProbes <- c("cg00032756","cg18958202","cg11701615","cg12484487","cg10094651",
                    "cg26091928","cg11412248","cg05894754","cg20135711","cg21246271")
sleepProbeTypes <- c(-1,-1,-1,-1,-1,-1,-1,1,1,-1)

# NIVEL HDL
dunedinProbes <- append(dunedinProbes, "cg06500161") # HIPO
dunedinProbes <- append(dunedinProbes, "cg17901584") # HIPER
dunedinProbes <- append(dunedinProbes, "cg02650017") # HIPER
dunedinProbes <- append(dunedinProbes, "cg24002003") # HIPER
dunedinProbes <- append(dunedinProbes, "cg09572125") # HIPO
dunedinProbes <- append(dunedinProbes, "cg27243685") # HIPO
dunedinProbes <- append(dunedinProbes, "cg14816825") # HIPO

hdlProbes <- c("cg06500161","cg17901584","cg02650017","cg24002003","cg09572125",
               "cg27243685","cg14816825")
hdlProbeTypes <- c(-1,1,1,1,-1,-1,-1)


dunedinProbes <- unique(dunedinProbes)

#dunedinProbes2 <- getRequiredProbes()[[1]]

pessoasDf <- get_full_pessoas_com_meta(c("AGE", "GENDER"))

# Remove idades em range (> 60, <= 25 etc)
pessoasDf <-
  pessoasDf %>%
  mutate( AGE = as.numeric(AGE) ) %>%
  filter(!is.na(AGE))


listaSondas <- get_sondas()

dunedinBaseSondas <- listaSondas %>%
  filter(name_sonda %in% dunedinProbes) 

#TODO TESTAR AQUI DE NOVO
incCpgs <- c()
datasets <- get_dataset_list()

 
  mclapply(seq(1, nrow(datasets)), function(i){
    betas <- get_betas_sondas( datasets$id[i], dunedinBaseSondas,  normalized=T )
    save("betas", file=paste0("dados/betas_", datasets$id[i], ".rda"))
 }, mc.cores = 3)
 #  
 betasTmp <- NULL
 for(i in seq(1, nrow(datasets))){
   load(paste0("dados/betas_", datasets$id[i], ".rda"))
   if(i == 1){
     betasTmp <- betas
   }else{
     betasTmp <- rbind(betasTmp, betas)
   }
 }


 betas <- betasTmp
 rm(betasTmp)
 save("betas", file=paste0("dados/betas_dunedin.rda"))
load( file=paste0("dados/betas_dunedin.rda"))

pessoasBeta <- rownames(betas)
fullPessoasDf <- get_full_pessoas_com_meta(c("AGE", "GENDER"))

incPids <- intersect(pessoasBeta, fullPessoasDf$GEOID_PESSOA)



incPessoas <- fullPessoasDf %>% filter(GEOID_PESSOA %in% incPids)
dunedin <- methyAge(t(betas), clock='DunedinPACE', do_plot = F, inputation = FALSE )



plot(incPessoas$AGE, dunedin$mAge)
cor.test(as.numeric(incPessoas$AGE), dunedin$mAge)



smokeProbes <- c("cg05575921","cg03636183", "cg21566642","cg01940273", "cg05951221",
                 "cg21161138","cg25648203","cg25189904","cg06126421","cg06644428")
smokeProbeTypes <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
df <- data.frame(IDADE=incPessoas$AGE, DunedinPACE=dunedin$mAge)
# , 
#                  cg05575921=betas %>% pull("cg05575921") <= qtl_smk1[2],
#                  cg03636183=betas %>% pull("cg03636183") <= qtl_smk2[2],
#                  cg21566642=betas %>% pull("cg21566642") <= qtl_smk3[2],
#                  cg00574958=betas %>% pull("cg01940273") <= qtl_smk4[2],
#                  cg18181703=betas %>% pull("cg06644428") <= qtl_smk5[2],
#                  cg06500161=betas %>% pull("cg21161138") <= qtl_smk6[2], 
#                  cg06500162=betas %>% pull("cg25648203") <= qtl_smk7[2], 
#                  cg06500163=betas %>% pull("cg25189904") <= qtl_smk8[2] 
# )

sondasBetas <- colnames(betas)
idxP <- 1
SMOKE_SCORE <- rep(0, nrow(betas))
#TODO Adicionar outras condiçoes
probes <- hdlProbes
probeTypes <- hdlProbeTypes
for( i in seq(1, length(probes))){
  if( probes[i] %in% sondasBetas ){
    qtl <- quantile(betas %>% pull(probes[i]), probs = seq(0,1,0.1))
    xt <- asinh(sqrt(as.numeric(betas %>% pull(probes[i]))))
    
    x <- as.numeric(betas %>% pull(probes[i]))
    km <- kmeans(xt ,2, iter.max = 100, nstart = 10)
    # hist(x[km$cluster==2], col=rgb(0,0,1,0.3))
    # hist(x[km$cluster==1], col=rgb(1,0,0,0.3), add=T)
    
    if( probeTypes[i] == -1 ){
      ki <- 1
      if( mean(x[km$cluster==2]) > mean(x[km$cluster==1])){
        ki<-2
      }
      thr <- mean(x[km$cluster==ki])-sd(x[km$cluster==ki])
      
      SMOKE_SCORE <- SMOKE_SCORE + as.numeric(x <= thr)
    }else{
      ki <- 2
      if( mean(x[km$cluster==2]) > mean(x[km$cluster==1])){
        ki<-1
      }
      thr <- mean(x[km$cluster==ki])+sd(x[km$cluster==ki])
      SMOKE_SCORE <- SMOKE_SCORE + as.numeric(x >= thr)
      
    }
    idxP <-idxP+1
  }
}

#tScore <- table(SMOKE_SCORE)

# ADD to have similar numbers per class... if that makes even sense
# 
 SMOKE_SCORE[SMOKE_SCORE<= 3] <- 0
 SMOKE_SCORE[SMOKE_SCORE> 3 & SMOKE_SCORE <= 6] <- 1
 SMOKE_SCORE[SMOKE_SCORE> 6 ] <- 2

# IMPACTO CIGARRO IDADE
nonNA <- which(!is.na(as.numeric(df$IDADE)))
cigDf <- data.frame(SCORE=as.factor(SMOKE_SCORE[nonNA]), IDADE=as.numeric(df$IDADE[nonNA]), DUNEDIN=df$DunedinPACE[nonNA])
# summary(aov(IDADE ~ SCORE, data = cigDf))
# summary(aov(DUNEDIN ~ SCORE, data = cigDf))

ggplot(cigDf, aes(x=SCORE, y=DUNEDIN)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="IDADE (Anos)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  theme_classic()  



# qtl_smk1 <- quantile(betas %>% pull("cg05575921"))
# qtl_smk2 <- quantile(betas %>% pull("cg03636183"))
# qtl_smk3 <- quantile(betas %>% pull("cg21566642"))
# qtl_smk4 <- quantile(betas %>% pull("cg01940273"))
# qtl_smk5 <- quantile(betas %>% pull("cg06644428"))
# qtl_smk6 <- quantile(betas %>% pull("cg21161138"))
# qtl_smk7 <- quantile(betas %>% pull("cg25648203"))
# qtl_smk8 <- quantile(betas %>% pull("cg25189904"))


# qtl_bmi1 <- quantile(betas %>% pull("cg00574958"))
# qtl_bmi2 <- quantile(betas %>% pull("cg18181703"))
# qtl_bmi3 <- quantile(betas %>% pull("cg17501210"))
# qtl_bmi4 <- quantile(betas %>% pull("cg06500161"))
# qtl_bmi5 <- quantile(betas %>% pull("cg11024682"))
# qtl_bmi6 <- quantile(betas %>% pull("cg09664445"))
# qtl_bmi7 <- quantile(betas %>% pull("cg13708645"))
# qtl_bmi8 <- quantile(betas %>% pull("cg25178683"))
# qtl_bmi9 <- quantile(betas %>% pull("cg26403843"))

library(ggplot2)
library(RColorBrewer)
# plot(incPessoas$AGE, dunedin$mAge)
myPalette <- colorRampPalette(rev(brewer.pal(6, "PuOr")))

# # TABAGISMO
# df <- data.frame(IDADE=incPessoas$AGE, DunedinPACE=dunedin$mAge, 
#                  cg05575921=betas %>% pull("cg05575921") <= qtl_smk1[2],
#                  cg03636183=betas %>% pull("cg03636183") <= qtl_smk2[2],
#                  cg21566642=betas %>% pull("cg21566642") <= qtl_smk3[2],
#                  cg00574958=betas %>% pull("cg01940273") <= qtl_smk4[2],
#                  cg18181703=betas %>% pull("cg06644428") <= qtl_smk5[2],
#                  cg06500161=betas %>% pull("cg21161138") <= qtl_smk6[2], 
#                  cg06500162=betas %>% pull("cg25648203") <= qtl_smk7[2], 
#                  cg06500163=betas %>% pull("cg25189904") <= qtl_smk8[2] 
#                  )
SMOKE_SCORE=df["cg05575921"]+df["cg03636183"]+df["cg00574958"]+df["cg18181703"]+df["cg21566642"] +df["cg06500161"]+df["cg06500162"]+df["cg06500163"]
# IMC
df <- data.frame(IDADE=as.numeric(incPessoas$AGE), DunedinPACE=dunedin$mAge, DSET=incPessoas$ID_DATASET,
                 cg05575921=betas %>% pull("cg00574958") <= qtl_bmi1[2],
                 cg03636183=betas %>% pull("cg18181703") <= qtl_bmi2[2],
                 cg21566642=betas %>% pull("cg17501210") <= qtl_bmi3[2],
                 cg00574958=betas %>% pull("cg06500161") >= qtl_bmi4[4],
                 cg18181703=betas %>% pull("cg11024682") >= qtl_bmi5[4],
                 cg06500161=betas %>% pull("cg09664445") >= qtl_bmi6[4], 
                 cg06500162=betas %>% pull("cg13708645") >= qtl_bmi7[4], 
                 cg06500163=betas %>% pull("cg25178683") >= qtl_bmi8[4], 
                 cg06500164=betas %>% pull("cg26403843") >= qtl_bmi9[4]
)


BMI_SCORE=df["cg05575921"]+df["cg03636183"]+df["cg00574958"]+df["cg18181703"]+df["cg21566642"] +df["cg06500161"]+df["cg06500162"]+df["cg06500163"]+df["cg06500164"]

# IDADE
ggplot(df, aes(x=IDADE)) + 
  geom_histogram(color="black", fill="cyan", binwidth = 5) +
  scale_x_continuous(name ="IDADE (Anos)", 
                     limits=c(0, 105), breaks=seq(5,100,by=10) )+
  scale_y_continuous(name ="# Amostras" )+
  theme_classic()

# DUNEDIN
ggplot(dunedin, aes(x=mAge)) + 
  geom_histogram(color="black", fill="cyan") +
  scale_x_continuous(name ="ACELERAÇÃO DE IDADE (u.a., Dunedin)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  scale_y_continuous(name ="# Amostras" )+
  theme_classic()

# IDADE vs DUNEDIN
ggplot(data.frame(IDADE=df$IDADE, DUNEDIN=df$DunedinPACE, DSET=df$DSET), aes(x=IDADE, y=DUNEDIN)) +
  geom_point() +
  scale_y_continuous(name ="ACELERAÇÃO DE IDADE (u.a., Dunedin)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  geom_smooth(method=lm, se=FALSE) +
  theme_classic()

# IMPACTO CIGARRO IDADE
nonNA <- which(!is.na(as.numeric(df$IDADE)))
cigDf <- data.frame(SCORE=as.factor(SMOKE_SCORE$cg05575921[nonNA]), IDADE=as.numeric(df$IDADE[nonNA]), DUNEDIN=df$DunedinPACE[nonNA])
summary(aov(IDADE ~ SCORE, data = cigDf))
summary(aov(DUNEDIN ~ SCORE, data = cigDf))

ggplot(cigDf, aes(x=SCORE, y=IDADE)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="IDADE (Anos)", 
                     limits=c(0, 105), breaks=seq(5,100,by=10) )+
  theme_classic()  

ggplot(cigDf, aes(x=SCORE, y=DUNEDIN)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="ACELERAÇÃO DE IDADE (u.a., Dunedin)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  theme_classic()


# IMPACTO IMC IDADE
nonNA <- which(!is.na(as.numeric(df$IDADE)))
imcDf <- data.frame(SCORE=as.factor(BMI_SCORE$cg05575921[nonNA]), IDADE=as.numeric(df$IDADE[nonNA]), DUNEDIN=df$DunedinPACE[nonNA])
summary(aov(IDADE ~ SCORE, data = imcDf))
summary(aov(DUNEDIN ~ SCORE, data = imcDf))

ggplot(imcDf, aes(x=SCORE, y=IDADE)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="IDADE (Anos)", 
                     limits=c(0, 105), breaks=seq(5,100,by=10) )+
  theme_classic()  

ggplot(imcDf, aes(x=SCORE, y=DUNEDIN)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="ACELERAÇÃO DE IDADE (u.a., Dunedin)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  theme_classic()

# IMPACTO COMPOSTO IDADE
nonNA <- which(!is.na(as.numeric(df$IDADE)))
imcDf <- data.frame(SCORE=as.factor(BMI_SCORE$cg05575921[nonNA]+SMOKE_SCORE$cg05575921[nonNA]), IDADE=as.numeric(df$IDADE[nonNA]), DUNEDIN=df$DunedinPACE[nonNA])
summary(aov(IDADE ~ SCORE, data = imcDf))
summary(aov(DUNEDIN ~ SCORE, data = imcDf))

ggplot(imcDf, aes(x=SCORE, y=IDADE)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="IDADE (Anos)", 
                     limits=c(0, 105), breaks=seq(5,100,by=10) )+
  theme_classic()  

ggplot(imcDf, aes(x=SCORE, y=DUNEDIN)) +
  geom_violin(fill="cyan") +
  geom_boxplot(width=0.1) +
  scale_y_continuous(name ="ACELERAÇÃO DE IDADE (u.a., Dunedin)", 
                     limits=c(0.4, 2), breaks=seq(0,2,by=0.25) )+
  theme_classic()





ggplot(data.frame(SCORE=as.factor(SMOKE_SCORE$cg05575921), DUNEDIN=df$DunedinPACE), aes(x=SCORE, y=DUNEDIN)) +
  geom_boxplot() +
  theme_classic()  

ggplot(data.frame(SCORE=as.factor(AGE_SCORE$cg05575921), DUNEDIN=as.numeric(df$IDADE)), aes(x=SCORE, y=DUNEDIN)) +
  geom_boxplot() +
  theme_bw()  

ggplot(data.frame(SCORE=as.factor(SMOKE_SCORE$cg05575921+BMI_SCORE$cg05575921), DUNEDIN=df$DunedinPACE), aes(x=SCORE, y=DUNEDIN)) +
  geom_boxplot() +
  theme_classic()  

# df2<-data.frame(SCORE=as.factor(AGE_SCORE$cg05575921), DUNEDIN=df$DunedinPACE)
# 
# df2 %>% group_by(SCORE) %>%  dplyr::summarize(Mean = mean(DUNEDIN, na.rm=TRUE))
#                 
# ggplot(df, aes(x=IDADE, y=DunedinPACE)) +
#   geom_point(aes(color=DSET), size=4) +
#   scale_colour_gradientn(colours = myPalette(100), limits=c(0.2, 0.7)) +
#   theme_bw()  



df %>% filter(cg05575921 >= 0.5)  %>% pull(DunedinPACE) %>% mean()
df %>% filter(cg05575921 < 0.6)  %>% pull(DunedinPACE) %>% mean()

#plot(incPessoas$AGE, dunedin_bt$mAge)

#dunedin <- methyAge(t(betas), clock='DunedinPACE', do_plot = F, inputation = FALSE )

plot(incPessoas$AGE, dunedin$mAge)


save.image(file = "DunedinData.rda")
# TODO Remove samples with too few probes
load("DunedinData.rda")





# for( i in seq(0, length(incPids))){
#   incPids[i]
# }

# 
# miss <- c()
# for(i in seq(1,2776)){
#   if(sum(is.na(betasDf[i,])) >15000){
#     miss <- append(miss, i)
#   }
# }
# 
# mdf <- betasDf
# mdf[!is.na(mdf)] <- 0
# mdf[is.na(mdf)] <- 1
# 
# 
# sum(is.na(betasDf[1,]))
# 
# pct <- seq(1,nrow(mdf))*0
# for(i in seq(1,nrow(mdf))){
#   if( (i %% 10) == 0){
#     print(paste0(i, "/", nrow(mdf)))  
#   }
#   pct[i] = 100 * sum(mdf[i,]) / ncol(mdf)
# }
# 
# betasDf <- betasDf %>% filter(pct<35)
# pessoasDf <- pessoasDf %>% filter(pct<35)

ex <- t(betas)
colnames(betas) <- dunedinBaseSondas$name_sonda
rownames(betas) <- incPids


dunedin <- methyAge(ex, clock='DunedinPACE', do_plot = T, inputation = FALSE, age_info = )

incPessoas <- fullPessoasDf %>% filter(GEOID_PESSOA %in% incPids)



plot(incPessoas$AGE, dunedin$mAge)

save("dunedin", "betasDf", "pessoasDf", "incCpgs", file = "DATA.rda")
# Somente pessoas com betas


plot(pessoasDf$AGE, dunedin$mAge)

data = data.frame(data$AGE*0+1, pessoasDf$AGE, dunedin$mAge)
colnames(data) <- c("INTERCEPT","AGE", "mAGE")

mdl <- lm.fit(as.matrix(data %>% select(-mAGE)), data$mAGE)

data <- cbind(data, data.frame(mdl$residuals))
colnames(data) <- c("INTERCEPT","AGE", "mAGE", "mAGE_Adj")


clienteId <- 25

ggplot(data, aes(x=AGE, y=mAGE)) + 
  geom_point()+
  geom_point(aes(x=AGE[25],y=mAGE[25]),colour="#9955ff", size=5) +
  geom_smooth(method=lm) + 
  theme_classic()



sum((data$mAGE >= data$mAGE[25])/length(data$mAGE))
sum((data$mAGE < data$mAGE[25])/length(data$mAGE))

ggplot(data, aes(x=mAGE)) + 
  geom_density() +
  geom_point(aes(x=mAGE[25], y=1),colour="#9955ff", size=5) +
  geom_vline(xintercept = data$mAGE[25]) +
  theme_classic()
# geom_histogram(color="black", fill="white")

ggplot(data) +
  aes(x = mAGE, y = 100*after_stat(count)/sum(after_stat(count))) +
  geom_density(stat = 'bin', bins=22 ) +
  geom_point(aes(x=mAGE[25], y=1),colour="#9955ff", size=5) +
  geom_vline(xintercept = data$mAGE[25]) +
  theme_classic()



ggplot(data, aes(x=AGE, y=mAGE_Adj)) + 
  geom_point()+
  geom_point(aes(x=AGE[25],y=mAGE_Adj[25]),colour="#9955ff", size=5) +
  theme_classic()
