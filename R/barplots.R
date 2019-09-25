subn_PR_SINAN <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete.csv")


library("ggplot2")

install.packages("zoo")

#############################################################################################
###### subn. quarterYear#################

barplot_subn_PR_SINAN <- select(subn_PR_SINAN, DT_OCOR, ID_PACIENTE)
barplot_subn_PR_SINAN$DT_OCOR <- as.Date(barplot_subn_PR_SINAN$DT_OCOR)
barplot_subn_PR_SINAN$quarterYear  <- as.Date(as.yearqtr(barplot_subn_PR_SINAN$DT_OCOR))
tmp_subn <- head(barplot_subn_PR_SINAN %>% group_by(quarterYear) %>% summarise(Counts = n()), 52)


##### Loess short for Local Regression # smoothing
tmp_subn$index <- 1:nrow(tmp_subn)  # create index variable

loessMod15 <- loess(Counts ~ index, data=tmp_subn, span=0.15) # 10% smoothing span
loessMod25 <- loess(Counts ~ index, data=tmp_subn, span=0.25) # 25% smoothing span
loessMod50 <- loess(Counts ~ index, data=tmp_subn, span=0.50) # 50% smoothing span

# get smoothed output
smoothed15 <- predict(loessMod15) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50)


plot_sm <- plot(tmp_subn$Counts, x=tmp_subn$quarterYear, type="l", main="Loess Smoothing and Prediction", xlab="Quarter/Year", ylab="Count" )
#lines(smoothed15, x=tmp_subn$quarterYear, col="green")
#lines(smoothed25, x=tmp_subn$quarterYear, col="red")
lines(smoothed50, x=tmp_subn$quarterYear, col="red")

#### Get legend
install.packages("ggpubr")

leg <- get_legend(plot_sm)
as_ggplot(leg)

ggplot(tmp_subn , aes(fill=quarterYear, y=Counts, x=quarterYear)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + ggtitle("Count of underreported individuals with Hepatitis") +
  xlab("Quarter") + ylab("Count")  + theme(axis.text.x = element_text(angle = 90))

write.csv(barplot_subn_PR_SINAN, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE2_QUARTER_SUBN.CSV', row.names=FALSE)

write.csv(barplot_subn_PR_SINAN, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE3_LOESS.CSV', row.names=FALSE)

#############################################################################################

############################################################################
########### sexo # faixa
 library(data.table)
 
 labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                 sep = "-"), paste(100, "+", sep = ""))
 
sexo <- subn_PR_SINAN$SEXO
idade <- subn_PR_SINAN$IDADE
uf_res <- subn_PR_SINAN$UF_RES
uf_ocor <- subn_PR_SINAN$UF_OCOR

 sexo_idade_subnot <- cbind.data.frame(sexo,idade, uf_res, uf_ocor)
 sexo_idade_subnot$sexo <- as.character(sexo_idade_subnot$sexo)
 sexo_idade_subnot$idade <- as.numeric(sexo_idade_subnot$idade)
 sexo_idade_subnot$uf_res <- as.character(sexo_idade_subnot$uf_res)
 sexo_idade_subnot$uf_ocor <- as.character(sexo_idade_subnot$uf_ocor)
 
 sexo_idade_subnot$IDADE_faixa <- cut(sexo_idade_subnot$idade, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)
 
 idade <- as.data.frame(idade) 
 sexo <- as.data.frame(sexo) 
 uf_ocor <- as.data.frame(uf_ocor)
 uf_res <- as.data.frame(uf_res)
 
 
  print(pl_SEXO_FAIXA + geom_bar(aes(fill=SEXO_IDADE$IDADE_faixa),position = "fill")) + theme_bw() 
 pl_SEXO_FAIXA <- ggplot(SEXO_IDADE, aes(x=SEXO_IDADE$SEXO)) + labs(x = "Gender", y = "Count", fill = "Age") 
 
 write.csv(sexo_idade_subnot, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE4-9_sexo_faixa_idade.CSV', row.names=FALSE)
 
######################################################################################

###################################################################################### 
 ###### UF_RES

 UF_res <- table(subn_PR_SINAN$UF_RES)
 UF_res <- as.data.frame(UF_res)
 UF_res <- UF_res[-c(26), ] # remove SP #  2839250
 UF_res <- UF_res[order(UF_res$Freq), ] 
 UF_res$Var1 <- factor(UF_res$Var1, levels = UF_res$Var1) 
 
 ggplot(data=UF_res, aes(x=UF_res$Var1, y=UF_res$Freq, fill=UF_res$Var1),) +
   geom_bar(stat="identity", position=position_dodge()) + theme_bw() + labs(x = "State", y = "Count" , fill = "State") 
 
 ################################################################################
 
 ###### UF_ocor

 UF_ocor <- table(subn_PR_SINAN$UF_OCOR)
 UF_ocor <- as.data.frame(UF_ocor)
 UF_ocor <- UF_ocor[-c(26), ] # remove SP # 2843522
 UF_ocor <- UF_ocor[order(UF_ocor$Freq), ] 
 UF_ocor$Var1 <- factor(UF_ocor$Var1, levels = UF_ocor$Var1) 
 
 ggplot(data=UF_ocor, aes(x=UF_ocor$Var1, y=UF_ocor$Freq, fill=UF_ocor$Var1),) +
   geom_bar(stat="identity", position=position_dodge()) + theme_bw() + labs(x = "State", y = "Count" , fill = "State") 
 
 ID <- subn_PR_SINAN$ID_PACIENTE
 ID <- as.data.frame(ID)
 
 uf_ocor_uf_res_ID <- cbind.data.frame(ID, uf_res, uf_ocor)
 
 write.csv(uf_ocor_uf_res_ID, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE11_UF.CSV', row.names=FALSE)
 

 ###########################################################################
 
 #### DB origin 
 BD <- table(subn_PR_SINAN$DB_ORIGEM)
 BD <- as.data.frame(BD)
 BD <- BD[order(BD$Freq), ] 
 BD$Var1 <- factor(BD$Var1, levels = BD$Var1) 
 
 #### DB origin # W/O BPAI -> 4056154; APAC -> 239
 
 BD <- table(subn_PR_SINAN$DB_ORIGEM)
 BD <- as.data.frame(BD)
 BD <- BD[-c(3), ] # remove BPAI
 BD <- BD[-c(2), ] # remove APAC
 BD <- BD[order(BD$Freq), ] 
 BD$Var1 <- factor(BD$Var1, levels = BD$Var1) 
 
 ggplot(data=BD, aes(x=BD$Var1, y=BD$Freq, fill=BD$Var1),) +
   geom_bar(stat="identity", position=position_dodge()) + theme_bw() + labs(x = "Database", y = "Count" , fill = "Database") 
 

subn_PR_SINAN <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete.csv")
 

write.csv(BD, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE12_DB.CSV', row.names=FALSE)


########################################################################
############### HEP types
hep_tipo <- table(subn_PR_SINAN$HEPATITE)
hep <- as.data.frame(hep)
hep_tipo <- hep_tipo[order(hep_tipo$Count), ] 
hep_tipo$HEP_TIPO <- factor(hep_tipo$HEP_TIPO, levels = hep_tipo$HEP_TIPO) 

is.na(hep$Var1) <- hep$Var1==""  
is.na(hep$Var1) <- hep$Var1=="_" 
is.na(hep$Var1) <- hep$Var1=="//"

hep %>% 
  group_by(Var1)  %>%
  arrange(desc(Freq))

#### Build the hep df

HEP_TIPO <- c('A','B','C', 'D', 'I', 'A/B', 'C/D', 'A/D', 'A/C', 'B/I', 'C/I', 'D/I', 'A/I', 'A/B/I')
Count <- c(13631, 5055, 3787234, 151452, 163471, 27, 28, 2, 4, 6, 35, 14, 20, 2)

### W/O HEP C -> 3787269 ; A/B -> 27;  C/D -> 28 ; A/D -> 2; A/C -> 4

HEP_TIPO <- c('A','B', "C", 'D', 'I', "A/B",  "C/D" , "A/D" , "A/C")
Count <- c(13651, 5061 ,3787269, 151466, 163473, 27, 28, 2, 4  )

HEP_TIPO <- c('A','B', 'D', 'I')
Count <- c(13651, 5061 , 151466, 163473)

HEP_TIPO <- as.data.frame(HEP_TIPO)
Count <- as.data.frame(Count)

hep_tipo <-cbind.data.frame(HEP_TIPO, Count)

ggplot(data=hep_tipo, aes(x=hep_tipo$HEP_TIPO, y=hep_tipo$Count, fill=hep_tipo$HEP_TIPO),) +
  geom_bar(stat="identity", position=position_dodge()) + theme_bw() + labs(x = "Hepatitis types", y = "Count" , fill = "Hepatitis types") 

hep_tipo <- as.data.frame(HEP_TIPO, Count)


write.csv(hep_tipo, file = '/Volumes/MIKAEL 1/ProjetoBDBM/Plots_tables/FIGURE10_hep_type.CSV', row.names=FALSE)


#####################################################################################
######Hep_type_year




#####################################################################################
######Overlay
colnames(UF_ocor)[colnames(UF_ocor)=="Hospital"] <- "Ocurrence"

colnames(UF_res)[colnames(UF_res)=="Var1"] <- "Var2"
colnames(UF_res)[colnames(UF_res)=="UF_res"] <- "Residence"

UF <- cbind.data.frame(UF_ocor, UF_res)

UF <- select (UF,-c(Var2))

melted<-melt(UF, id="Var1")

print(ggplot(melted,aes(x=Var1,y=value,fill=variable)) + 
        geom_bar(stat="identity",position = "identity", alpha=.3))+ theme_bw() + labs(x = "State", y = "Count" , fill = "State") 


table(subn_PR_SINAN$UF_RES)


#####################################################################################
###### SUL ### SEXO_IDADE #### OCOR

subn_PR_SINAN$SEXO <- as.character(subn_PR_SINAN$SEXO)
subn_PR_SINAN$IDADE <- as.character(subn_PR_SINAN$IDADE)
subn_PR_SINAN$UF_OCOR <- as.character(subn_PR_SINAN$UF_OCOR)


RS_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="RS" ),c(IDADE, SEXO))
PR_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="PR" ),c(IDADE, SEXO))
SC_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="SC" ),c(IDADE, SEXO))

SUL_OCOR <- bind_rows(list(RS_OCOR, PR_OCOR, SC_OCOR))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


SUL_OCOR$SEXO <- as.character(SUL_OCOR$SEXO)
SUL_OCOR$IDADE <- as.numeric(SUL_OCOR$IDADE)
SUL_OCOR$IDADE_faixa <- cut(SUL_OCOR$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=SUL_OCOR$SEXO),position = position_dodge())) + theme_bw() 
pl_SEXO_FAIXA <- ggplot(SUL_OCOR, aes(x=SUL_OCOR$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 


#####################################################################################
###### SUL ### SEXO_IDADE #### RES

subn_PR_SINAN$SEXO <- as.character(subn_PR_SINAN$SEXO)
subn_PR_SINAN$IDADE <- as.character(subn_PR_SINAN$IDADE)
subn_PR_SINAN$UF_OCOR <- as.character(subn_PR_SINAN$UF_OCOR)


RS_RES <- select(filter(subn_PR_SINAN, UF_RES=="RS" ),c(IDADE, SEXO))
PR_RES <- select(filter(subn_PR_SINAN, UF_RES=="PR" ),c(IDADE, SEXO))
SC_RES <- select(filter(subn_PR_SINAN, UF_RES=="SC" ),c(IDADE, SEXO))

SUL_RES <- bind_rows(list(RS_RES, PR_RES, SC_RES))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


SUL_RES$SEXO <- as.character(SUL_RES$SEXO)
SUL_RES$IDADE <- as.numeric(SUL_RES$IDADE)
SUL_RES$IDADE_faixa <- cut(SUL_RES$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=SUL_RES$SEXO),position = position_dodge())) + theme_bw() 
pl_SEXO_FAIXA <- ggplot(SUL_RES, aes(x=SUL_RES$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 



#####################################################################################
###### SUDESTE ### SEXO_IDADE ### OCOR


SP_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="SP" ),c(IDADE, SEXO))
MG_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="MG" ),c(IDADE, SEXO))
RJ_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="RJ" ),c(IDADE, SEXO))
ES_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="ES" ),c(IDADE, SEXO))


SUDESTE_OCOR <- bind_rows(list(RJ_OCOR, ES_OCOR, SP_OCOR, MG_OCOR))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


SUDESTE_OCOR$SEXO <- as.character(SUDESTE_OCOR$SEXO)
SUDESTE_OCOR$IDADE <- as.numeric(SUDESTE_OCOR$IDADE)
SUDESTE_OCOR$IDADE_faixa <- cut(SUDESTE_OCOR$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=SUDESTE_OCOR$SEXO),position = position_dodge())) + theme_bw() 
pl_SEXO_FAIXA <- ggplot(SUDESTE_OCOR, aes(x=SUDESTE_OCOR$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 


#####################################################################################
###### SUDESTE ### SEXO_IDADE ### RES


SP_RES <- select(filter(subn_PR_SINAN, UF_RES=="SP" ),c(IDADE, SEXO))
MG_RES <- select(filter(subn_PR_SINAN, UF_RES=="MG" ),c(IDADE, SEXO))
RJ_RES <- select(filter(subn_PR_SINAN, UF_RES=="RJ" ),c(IDADE, SEXO))
ES_RES <- select(filter(subn_PR_SINAN, UF_RES=="ES" ),c(IDADE, SEXO))


SUDESTE_RES <- bind_rows(list(RJ_RES, ES_RES, SP_RES, MG_RES))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


SUDESTE_RES$SEXO <- as.character(SUDESTE_RES$SEXO)
SUDESTE_RES$IDADE <- as.numeric(SUDESTE_RES$IDADE)
SUDESTE_RES$IDADE_faixa <- cut(SUDESTE_RES$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=SUDESTE_RES$SEXO),position = position_dodge())) + theme_bw() 
pl_SEXO_FAIXA <- ggplot(SUDESTE_RES, aes(x=SUDESTE_RES$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 








#####################################################################################
###### NORTE ### SEXO_IDADE ## OCOR

AC_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="AC" ),c(IDADE, SEXO))
TO_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="TO" ),c(IDADE, SEXO))
AM_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="AM" ),c(IDADE, SEXO))
PA_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="PA" ),c(IDADE, SEXO))
RO_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="RO" ),c(IDADE, SEXO))
RR_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="RR" ),c(IDADE, SEXO))
AP_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="AP" ),c(IDADE, SEXO))


NORTE_OCOR <- bind_rows(list(AC_OCOR, TO_OCOR, AM_OCOR, PA_OCOR, RO_OCOR, RR_OCOR, AP_OCOR))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


NORTE_OCOR$SEXO <- as.character(NORTE_OCOR$SEXO)
NORTE_OCOR$IDADE <- as.numeric(NORTE_OCOR$IDADE)
NORTE_OCOR$IDADE_faixa <- cut(NORTE_OCOR$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=NORTE_OCOR$SEXO),position = position_dodge())) + theme_bw() + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(NORTE_OCOR, aes(x=NORTE_OCOR$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 



#####################################################################################
###### NORTE ### SEXO_IDADE ## RES

AC_RES <- select(filter(subn_PR_SINAN, UF_RES=="AC" ),c(IDADE, SEXO))
TO_RES <- select(filter(subn_PR_SINAN, UF_RES=="TO" ),c(IDADE, SEXO))
AM_RES <- select(filter(subn_PR_SINAN, UF_RES=="AM" ),c(IDADE, SEXO))
PA_RES <- select(filter(subn_PR_SINAN, UF_RES=="PA" ),c(IDADE, SEXO))
RO_RES <- select(filter(subn_PR_SINAN, UF_RES=="RO" ),c(IDADE, SEXO))
RR_RES <- select(filter(subn_PR_SINAN, UF_RES=="RR" ),c(IDADE, SEXO))
AP_RES <- select(filter(subn_PR_SINAN, UF_RES=="AP" ),c(IDADE, SEXO))


NORTE_RES <- bind_rows(list(AC_RES, TO_RES, AM_RES, PA_RES, RO_RES, RR_RES, AP_RES))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


NORTE_RES$SEXO <- as.character(NORTE_RES$SEXO)
NORTE_RES$IDADE <- as.numeric(NORTE_RES$IDADE)
NORTE_RES$IDADE_faixa <- cut(NORTE_RES$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=NORTE_RES$SEXO),position = position_dodge())) + theme_bw() + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(NORTE_RES, aes(x=NORTE_RES$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 







#####################################################################################
###### CENTRO OESTE ### SEXO_IDADE ### OCOR

DF_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="DF" ),c(IDADE, SEXO))
GO_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="GO" ),c(IDADE, SEXO))
MT_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="MT" ),c(IDADE, SEXO))
MS_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="MS" ),c(IDADE, SEXO))



CENTRO_OESTE_OCOR <- bind_rows(list(DF_OCOR, GO_OCOR, MT_OCOR, MS_OCOR))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


CENTRO_OESTE_OCOR$SEXO <- as.character(CENTRO_OESTE_OCOR$SEXO)
CENTRO_OESTE_OCOR$IDADE <- as.numeric(CENTRO_OESTE_OCOR$IDADE)
CENTRO_OESTE_OCOR$IDADE_faixa <- cut(CENTRO_OESTE_OCOR$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=CENTRO_OESTE_OCOR$SEXO),position = position_dodge())) + theme_bw() + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(CENTRO_OESTE_OCOR, aes(x=CENTRO_OESTE_OCOR$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 



#####################################################################################
###### CENTRO OESTE ### SEXO_IDADE ### RES

DF_RES <- select(filter(subn_PR_SINAN, UF_RES=="DF" ),c(IDADE, SEXO))
GO_RES <- select(filter(subn_PR_SINAN, UF_RES=="GO" ),c(IDADE, SEXO))
MT_RES <- select(filter(subn_PR_SINAN, UF_RES=="MT" ),c(IDADE, SEXO))
MS_RES <- select(filter(subn_PR_SINAN, UF_RES=="MS" ),c(IDADE, SEXO))



CENTRO_OESTE_RES <- bind_rows(list(DF_RES, GO_RES, MT_RES, MS_RES))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


CENTRO_OESTE_RES$SEXO <- as.character(CENTRO_OESTE_RES$SEXO)
CENTRO_OESTE_RES$IDADE <- as.numeric(CENTRO_OESTE_RES$IDADE)
CENTRO_OESTE_RES$IDADE_faixa <- cut(CENTRO_OESTE_RES$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=CENTRO_OESTE_RES$SEXO),position = position_dodge())) + theme_bw() + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(CENTRO_OESTE_RES, aes(x=CENTRO_OESTE_RES$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 




#####################################################################################
###### NORDESTE ### SEXO_IDADE ### OCOR

AL_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="AL" ),c(IDADE, SEXO))
BA_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="BA" ),c(IDADE, SEXO))
CE_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="CE" ),c(IDADE, SEXO))
MA_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="MA" ),c(IDADE, SEXO))
PB_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="PB" ),c(IDADE, SEXO))
PE_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="PE" ),c(IDADE, SEXO))
PI_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="PI" ),c(IDADE, SEXO))
SE_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="SE" ),c(IDADE, SEXO))
RN_OCOR <- select(filter(subn_PR_SINAN, UF_OCOR=="RN" ),c(IDADE, SEXO))


NORDESTE_OCOR <- bind_rows(list(AL_OCOR, BA_OCOR, CE_OCOR, MA_OCOR, PB_OCOR, PE_OCOR, PI_OCOR, SE_OCOR, RN_OCOR))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


NORDESTE_OCOR$SEXO <- as.character(NORDESTE_OCOR$SEXO)
NORDESTE_OCOR$IDADE <- as.numeric(NORDESTE_OCOR$IDADE)
NORDESTE_OCOR$IDADE_faixa <- cut(NORDESTE_OCOR$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=NORDESTE_OCOR$SEXO),position = position_dodge())) + theme_bw()  + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(NORDESTE_OCOR, aes(x=NORDESTE_OCOR$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 

#####################################################################################
###### NORDESTE ### SEXO_IDADE ### RES

AL_RES <- select(filter(subn_PR_SINAN, UF_RES=="AL" ),c(IDADE, SEXO))
BA_RES <- select(filter(subn_PR_SINAN, UF_RES=="BA" ),c(IDADE, SEXO))
CE_RES <- select(filter(subn_PR_SINAN, UF_RES=="CE" ),c(IDADE, SEXO))
MA_RES <- select(filter(subn_PR_SINAN, UF_RES=="MA" ),c(IDADE, SEXO))
PB_RES <- select(filter(subn_PR_SINAN, UF_RES=="PB" ),c(IDADE, SEXO))
PE_RES <- select(filter(subn_PR_SINAN, UF_RES=="PE" ),c(IDADE, SEXO))
PI_RES <- select(filter(subn_PR_SINAN, UF_RES=="PI" ),c(IDADE, SEXO))
SE_RES <- select(filter(subn_PR_SINAN, UF_RES=="SE" ),c(IDADE, SEXO))
RN_RES <- select(filter(subn_PR_SINAN, UF_RES=="RN" ),c(IDADE, SEXO))


NORDESTE_RES <- bind_rows(list(AL_RES, BA_RES, CE_RES, MA_RES, PB_RES, PE_RES, PI_RES, SE_RES, RN_RES))

library(data.table)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))


NORDESTE_RES$SEXO <- as.character(NORDESTE_RES$SEXO)
NORDESTE_RES$IDADE <- as.numeric(NORDESTE_RES$IDADE)
NORDESTE_RES$IDADE_faixa <- cut(NORDESTE_RES$IDADE, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

print(pl_SEXO_FAIXA + geom_bar(aes(fill=NORDESTE_RES$SEXO),position = position_dodge())) + theme_bw()  + scale_fill_manual("Age", values = c("F" = "#17b12b", "M" = "#5086ff"))
pl_SEXO_FAIXA <- ggplot(NORDESTE_RES, aes(x=NORDESTE_RES$IDADE_faixa)) + labs(x = "Gender", y = "Count", fill = "Age") 




