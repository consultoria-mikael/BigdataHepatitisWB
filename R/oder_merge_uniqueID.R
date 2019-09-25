############ New filter - data frame #############
##################################################
###### script developed by Mikael Lemos ##########
###### Version 1.0 - 03.04.2019 ##################
## order, categorize, filter for unique ID?s #####
############## and merge #########################

######
### Loading / installing packages
######

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

#install.packages('Amelia')
library('Amelia')

# install.packages("tidyverse")
library(tidyverse)

# install.packages("lubridate")
library(lubridate)

######
### Loading DATA FRAME
######

##### AIH #####
#### MAC mikael
AIH_PR <- read.csv("/Users/mikaellemos/Projeto_BDBM/csv tables/PR_PO/AIH_PR.csv") 

AIH_PO <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/AIH_PO.csv") 

#### PC PO700
AIH_PR <- read.csv("/Users/alexandre.fonseca/Desktop/samples/") 

AIH_PO <- read.csv("/Users/alexandre.fonseca/Desktop/samples/") 

##### APAC ####
#### MAC mikael
APAC_PR <- read.csv("/Users/mikaellemos/Projeto_BDBM/csv tables/PR_PO/APAC_PR.csv")

APAC_PO <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/APAC_PO.csv")

#### PC PO700
APAC_PR <- read.csv("/Users/alexandre.fonseca/Desktop/samples/CSV_PRPO/APAC_PR.csv")

APAC_PO <- read.csv("/Users/alexandre.fonseca/Desktop/samples/")

#### BPAI #####
#### MAC mikael
BPAI_PR <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/BPAI_PR.csv")

BPAI_PO <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/BPAI_PO.csv")

#### PC PO700
BPAI_PR <- read.csv("/Users/alexandre.fonseca/Desktop/samples/")

BPAI_PO <- read.csv("/Users/alexandre.fonseca/Desktop/samples/")

#### SIM ######
#### MAC mikael
SIM_PR <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/SIM_PR.csv")

SIM_PO <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/SIM_PO.csv")

#### PC PO700
SIM_PR <- read.csv("/Users/alexandre.fonseca/Desktop/samples/")

SIM_PO <- read.csv("/Users/alexandre.fonseca/Desktop/samples/")

##############
#### order and categorize df
#############

#######
## AIH_PR
#######
##### AIH
AIH_PR$`HEPC.AIH$HEPC`<- as.character(AIH_PR$`HEPC.AIH$HEPC`)
AIH_PR$`HEPB.AiH$HEPB` <- as.character(AIH_PR$`HEPB.AiH$HEPB`)  
AIH_PR$`HEPA.AIH$HEPA` <- as.character(AIH_PR$`HEPA.AIH$HEPA`)
AIH_PR$`HEPD.AIH$HEPD` <- as.character(AIH_PR$`HEPD.AIH$HEPD`) 
AIH_PR$`HEPI.AIH$HEPI` <- as.character(AIH_PR$`HEPI.AIH$HEPI`)
AIH_PR$DB_ORIGEM <- as.character(AIH_PR$DB_ORIGEM)

############## Merging columns

AIH_PR <- unite(AIH_PR, "CID", c("CO_CID_PRINCIPAL", "CO_CID_SECUNDARIO_1", "CO_CID_SECUNDARIO_2", "CO_CID_SECUNDARIO_3", "CO_CID_SECUNDARIO_4", "CO_CID_SECUNDARIO_5", "CO_CID_SECUNDARIO_6", "CO_CID_SECUNDARIO_7", "CO_CID_SECUNDARIO_8","CO_CID_SECUNDARIO_9" , "CO_CID_OBITO" )) 

AIH_PR <- unite(AIH_PR,"PROCEDIMENTO", c("CO_PROCEDIMENTO_PRINCIPAL", "COD_PROCEDIMENTO_SECUNDARIO", "CO_PROC_SOLICITADO" )) 

#### Alternaltivelly, it is possible to merge hepatitis types columns into one
AIH_PR_2 <- unite(AIH_PR, "HEPATITE", c( "HEPC.AIH$HEPC","HEPB.AiH$HEPB",  "HEPA.AIH$HEPA", "HEPD.AIH$HEPD", "HEPI.AIH$HEPI" )) 

############### Ordering columns - possibility 1 ###do not unite hepatitis column

#AIH_PR <- select(AIH_PR, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPC.AIH$HEPC, HEPB.AiH$HEPB, HEPA.AIH$HEPA, HEPD.AIH$HEPD, HEPI.AIH$HEPI  )

############### Ordering columns - possibility 2 ### unite hepatitis column

AIH_PR <- select(AIH_PR_2, ID_PACIENTE, CID, PROCEDIMENTO , SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR , DB_ORIGEM, HEPATITE, -QT_PROCEDIMENTO, -DT_CMPT , -QT_DIARIAS_UI)

####################
####### Gender vs Age table
####################

##### calculate age with lubridate
library(lubridate)

#install.packages("data.table")  
#library("data.table")  

###### AIH_PR ######
YEAR1 <- data_frame(year  = ymd(AIH_PR$DT_NASC))
YEAR2 <- data_frame(year = ymd(AIH_PR$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- AIH_PR$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_AIH_PR <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

AIH_PR <- cbind.data.frame(AIH_PR ,IDADE)

AIH_PR$IDADE <- gsub("[S]", "", AIH_PR$IDADE)

IDADE <- gsub("[S]", "", SEXO_IDADE$IDADE)
SEXO_IDADE_AIH_PR$IDADE <- gsub("[S]", "", SEXO_IDADE_AIH_PR$IDADE)

##### UF

UF_RES <- substr(AIH_PR$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(AIH_PR$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

AIH_PR <- cbind.data.frame(AIH_PR, UF_RES)

AIH_PR <- cbind.data.frame(AIH_PR, UF_OCOR)

AIH_PR$UF_RES[AIH_PR$UF_RES== "11"] <- "RO"
AIH_PR$UF_RES[AIH_PR$UF_RES == "12"] <- "AC"
AIH_PR$UF_RES[AIH_PR$UF_RES == "13"] <- "AM"
AIH_PR$UF_RES[AIH_PR$UF_RES == "14"] <- "RR"
AIH_PR$UF_RES[AIH_PR$UF_RES == "15"] <- "PA"
AIH_PR$UF_RES[AIH_PR$UF_RES == "16"] <- "AP"
AIH_PR$UF_RES[AIH_PR$UF_RES == "17"] <- "TO"
AIH_PR$UF_RES[AIH_PR$UF_RES == "21"] <- "MA"
AIH_PR$UF_RES[AIH_PR$UF_RES == "22"] <- "PI"
AIH_PR$UF_RES[AIH_PR$UF_RES == "23"] <- "CE"
AIH_PR$UF_RES[AIH_PR$UF_RES == "24"] <- "RN"
AIH_PR$UF_RES[AIH_PR$UF_RES == "25"] <- "PB"
AIH_PR$UF_RES[AIH_PR$UF_RES == "26"] <- "PE"
AIH_PR$UF_RES[AIH_PR$UF_RES == "27"] <- "AL"
AIH_PR$UF_RES[AIH_PR$UF_RES == "28"] <- "SE"
AIH_PR$UF_RES[AIH_PR$UF_RES == "29"] <- "BA"
AIH_PR$UF_RES[AIH_PR$UF_RES == "31"] <- "MG"
AIH_PR$UF_RES[AIH_PR$UF_RES == "32"] <- "ES"
AIH_PR$UF_RES[AIH_PR$UF_RES == "33"] <- "RJ"
AIH_PR$UF_RES[AIH_PR$UF_RES == "35"] <- "SP"
AIH_PR$UF_RES[AIH_PR$UF_RES == "41"] <- "PR"
AIH_PR$UF_RES[AIH_PR$UF_RES == "42"] <- "SC"
AIH_PR$UF_RES[AIH_PR$UF_RES == "43"] <- "RS"
AIH_PR$UF_RES[AIH_PR$UF_RES == "50"] <- "MS"
AIH_PR$UF_RES[AIH_PR$UF_RES == "51"] <- "MT"
AIH_PR$UF_RES[AIH_PR$UF_RES == "52"] <- "GO"
AIH_PR$UF_RES[AIH_PR$UF_RES == "53"] <- "DF"

AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "11"] <- "RO"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "12"] <- "AC"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "13"] <- "AM"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "14"] <- "RR"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "15"] <- "PA"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "16"] <- "AP"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "17"] <- "TO"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "21"] <- "MA"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "22"] <- "PI"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "23"] <- "CE"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "24"] <- "RN"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "25"] <- "PB"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "26"] <- "PE"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "27"] <- "AL"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "28"] <- "SE"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "29"] <- "BA"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "31"] <- "MG"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "32"] <- "ES"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "33"] <- "RJ"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "35"] <- "SP"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "41"] <- "PR"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "42"] <- "SC"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "43"] <- "RS"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "50"] <- "MS"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "51"] <- "MT"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "52"] <- "GO"
AIH_PR$UF_OCOR[AIH_PR$UF_OCOR == "53"] <- "DF"


### Saving file csv
write.csv(AIH_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/AIH_PR_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_AIH_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/SEXO_IDADE_AIH_PR.csv', row.names=FALSE)

#######
## AIH_PO
#######

##### AIH_PO
AIH_PO$`HEPC.AIH$HEPC`<- as.character(AIH_PO$`HEPC.AIH$HEPC`)
AIH_PO$`HEPB.AiH$HEPB` <- as.character(AIH_PO$`HEPB.AiH$HEPB`)  
AIH_PO$`HEPA.AIH$HEPA` <- as.character(AIH_PO$`HEPA.AIH$HEPA`)
AIH_PO$`HEPD.AIH$HEPD` <- as.character(AIH_PO$`HEPD.AIH$HEPD`) 
AIH_PO$`HEPI.AIH$HEPI` <- as.character(AIH_PO$`HEPI.AIH$HEPI`)
AIH_PO$DB_ORIGEM <- as.character(AIH_PO$DB_ORIGEM)

############## Merging columns

AIH_PO <- unite(AIH_PO, "CID", c("CO_CID_PRINCIPAL", "CO_CID_SECUNDARIO_1", "CO_CID_SECUNDARIO_2", "CO_CID_SECUNDARIO_3", "CO_CID_SECUNDARIO_4", "CO_CID_SECUNDARIO_5", "CO_CID_SECUNDARIO_6", "CO_CID_SECUNDARIO_7", "CO_CID_SECUNDARIO_8","CO_CID_SECUNDARIO_9" , "CO_CID_OBITO" )) 

AIH_PO <- unite(AIH_PO,"PROCEDIMENTO", c("CO_PROCEDIMENTO_PRINCIPAL", "COD_PROCEDIMENTO_SECUNDARIO", "CO_PROC_SOLICITADO" )) 

#### Alternaltivelly, it is possible to merge hepatitis types columns into one
AIH_PO_2 <- unite(AIH_PO, "HEPATITE", c( "HEPC.AIH$HEPC","HEPB.AiH$HEPB",  "HEPA.AIH$HEPA", "HEPD.AIH$HEPD", "HEPI.AIH$HEPI" )) 

############### Ordering columns - possibility 1 ###do not unite hepatitis column

#AIH_PO <- select(AIH_PO, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPC.AIH$HEPC, HEPB.AiH$HEPB, HEPA.AIH$HEPA, HEPD.AIH$HEPD, HEPI.AIH$HEPI  )

############### Ordering columns - possibility 2 ### unite hepatitis column

AIH_PO <- select(AIH_PO_2, ID_PACIENTE, CID, PROCEDIMENTO , SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE , DB_ORIGEM,-QT_PROCEDIMENTO, -DT_CMPT , -QT_DIARIAS_UI)

####################
####### Gender vs Age table
####################

##### calculate age with lubridate
library(lubridate)

#install.packages("data.table")  
#library("data.table")  

###### AIH_PO ######
YEAR1 <- data_frame(year  = ymd(AIH_PO$DT_NASC))
YEAR2 <- data_frame(year = ymd(AIH_PO$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- AIH_PO$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_AIH_PO <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

AIH_PO <- cbind.data.frame(AIH_PO ,IDADE)

AIH_PO$IDADE <- gsub("[S]", "", AIH_PO$IDADE)
SEXO_IDADE_AIH_PO$IDADE <- gsub("[S]", "", SEXO_IDADE_AIH_PO$IDADE)

##### UF

UF_RES <- substr(AIH_PO$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(AIH_PO$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

AIH_PO <- cbind.data.frame(AIH_PO, UF_RES)

AIH_PO <- cbind.data.frame(AIH_PO, UF_OCOR)

AIH_PO$UF_RES <- as.character(AIH_PO$UF_RES)
AIH_PO$UF_OCOR <- as.character(AIH_PO$UF_OCOR)

AIH_PO$UF_RES[AIH_PO$UF_RES== "11"] <- "RO"
AIH_PO$UF_RES[AIH_PO$UF_RES == "12"] <- "AC"
AIH_PO$UF_RES[AIH_PO$UF_RES == "13"] <- "AM"
AIH_PO$UF_RES[AIH_PO$UF_RES == "14"] <- "RR"
AIH_PO$UF_RES[AIH_PO$UF_RES == "15"] <- "PA"
AIH_PO$UF_RES[AIH_PO$UF_RES == "16"] <- "AP"
AIH_PO$UF_RES[AIH_PO$UF_RES == "17"] <- "TO"
AIH_PO$UF_RES[AIH_PO$UF_RES == "21"] <- "MA"
AIH_PO$UF_RES[AIH_PO$UF_RES == "22"] <- "PI"
AIH_PO$UF_RES[AIH_PO$UF_RES == "23"] <- "CE"
AIH_PO$UF_RES[AIH_PO$UF_RES == "24"] <- "RN"
AIH_PO$UF_RES[AIH_PO$UF_RES == "25"] <- "PB"
AIH_PO$UF_RES[AIH_PO$UF_RES == "26"] <- "PE"
AIH_PO$UF_RES[AIH_PO$UF_RES == "27"] <- "AL"
AIH_PO$UF_RES[AIH_PO$UF_RES == "28"] <- "SE"
AIH_PO$UF_RES[AIH_PO$UF_RES == "29"] <- "BA"
AIH_PO$UF_RES[AIH_PO$UF_RES == "31"] <- "MG"
AIH_PO$UF_RES[AIH_PO$UF_RES == "32"] <- "ES"
AIH_PO$UF_RES[AIH_PO$UF_RES == "33"] <- "RJ"
AIH_PO$UF_RES[AIH_PO$UF_RES == "35"] <- "SP"
AIH_PO$UF_RES[AIH_PO$UF_RES == "41"] <- "PR"
AIH_PO$UF_RES[AIH_PO$UF_RES == "42"] <- "SC"
AIH_PO$UF_RES[AIH_PO$UF_RES == "43"] <- "RS"
AIH_PO$UF_RES[AIH_PO$UF_RES == "50"] <- "MS"
AIH_PO$UF_RES[AIH_PO$UF_RES == "51"] <- "MT"
AIH_PO$UF_RES[AIH_PO$UF_RES == "52"] <- "GO"
AIH_PO$UF_RES[AIH_PO$UF_RES == "53"] <- "DF"

AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "11"] <- "RO"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "12"] <- "AC"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "13"] <- "AM"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "14"] <- "RR"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "15"] <- "PA"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "16"] <- "AP"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "17"] <- "TO"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "21"] <- "MA"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "22"] <- "PI"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "23"] <- "CE"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "24"] <- "RN"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "25"] <- "PB"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "26"] <- "PE"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "27"] <- "AL"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "28"] <- "SE"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "29"] <- "BA"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "31"] <- "MG"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "32"] <- "ES"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "33"] <- "RJ"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "35"] <- "SP"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "41"] <- "PR"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "42"] <- "SC"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "43"] <- "RS"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "50"] <- "MS"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "51"] <- "MT"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "52"] <- "GO"
AIH_PO$UF_OCOR[AIH_PO$UF_OCOR == "53"] <- "DF"

### Saving file csv
write.csv(AIH_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/AIH_PO_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_AIH_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_AIH_PO.csv', row.names=FALSE)

#### Factor to chr
APAC_PR$`HEPC.APAC$HEPC` <- as.character(APAC_PR$`HEPC.APAC$HEPC`)  
APAC_PR$`HEPB.APAC$HEPB` <- as.character(APAC_PR$`HEPB.APAC$HEPB`)
APAC_PR$`HEPA.APAC$HEPA` <- as.character(APAC_PR$`HEPA.APAC$HEPA`)
APAC_PR$`HEPD.APAC$HEPD` <- as.character(APAC_PR$`HEPD.APAC$HEPD`)
APAC_PR$`HEPI.APAC$HEPI` <- as.character(APAC_PR$`HEPI.APAC$HEPI`)

##### APAC_PR ######
### MERGING columns 
APAC_PR <- unite(APAC_PR, "CID", c("CO_CID_PRINCIPAL", "CO_CID_SECUNDARIO"))
APAC_PR <- unite(APAC_PR, "PROCEDIMENTO", c("CO_PROCEDIMENTO_PRINCIPAL", "CO_PROCEDIMENTO_SECUNDARIO"))
APAC_PR <- unite(APAC_PR, "HEPATITE", c( "HEPC.APAC$HEPC","HEPB.APAC$HEPB",  "HEPA.APAC$HEPA", "HEPD.APAC$HEPD", "HEPI.APAC$HEPI" )) 


#### ORDERING COLUMNS
APAC_PR <- select(APAC_PR, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(APAC_PR$DT_NASC))
YEAR2 <- data_frame(year = ymd(APAC_PR$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- APAC_PR$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_APAC_PR <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

APAC_PR <- cbind.data.frame(APAC_PR ,IDADE)
APAC_PR$IDADE <- gsub("[S]", "", APAC_PR$IDADE)


IDADE <- gsub("[S]", "", SEXO_IDADE$IDADE)
SEXO_IDADE_APAC_PR$IDADE <- gsub("[S]", "", SEXO_IDADE_APAC_PR$IDADE)


#### UF 

UF_RES <- substr(APAC_PR$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(APAC_PR$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

APAC_PR <- cbind.data.frame(APAC_PR, UF_RES)

APAC_PR <- cbind.data.frame(APAC_PR, UF_OCOR)

APAC_PR$UF_RES <- as.character(APAC_PR$UF_RES)
APAC_PR$UF_OCOR <- as.character(APAC_PR$UF_OCOR)

APAC_PR$UF_RES[APAC_PR$UF_RES == "11"] <- "RO"
APAC_PR$UF_RES[APAC_PR$UF_RES == "12"] <- "AC"
APAC_PR$UF_RES[APAC_PR$UF_RES == "13"] <- "AM"
APAC_PR$UF_RES[APAC_PR$UF_RES == "14"] <- "RR"
APAC_PR$UF_RES[APAC_PR$UF_RES == "15"] <- "PA"
APAC_PR$UF_RES[APAC_PR$UF_RES == "16"] <- "AP"
APAC_PR$UF_RES[APAC_PR$UF_RES == "17"] <- "TO"
APAC_PR$UF_RES[APAC_PR$UF_RES == "21"] <- "MA"
APAC_PR$UF_RES[APAC_PR$UF_RES == "22"] <- "PI"
APAC_PR$UF_RES[APAC_PR$UF_RES == "23"] <- "CE"
APAC_PR$UF_RES[APAC_PR$UF_RES == "24"] <- "RN"
APAC_PR$UF_RES[APAC_PR$UF_RES == "25"] <- "PB"
APAC_PR$UF_RES[APAC_PR$UF_RES == "26"] <- "PE"
APAC_PR$UF_RES[APAC_PR$UF_RES == "27"] <- "AL"
APAC_PR$UF_RES[APAC_PR$UF_RES == "28"] <- "SE"
APAC_PR$UF_RES[APAC_PR$UF_RES == "29"] <- "BA"
APAC_PR$UF_RES[APAC_PR$UF_RES == "31"] <- "MG"
APAC_PR$UF_RES[APAC_PR$UF_RES == "32"] <- "ES"
APAC_PR$UF_RES[APAC_PR$UF_RES == "33"] <- "RJ"
APAC_PR$UF_RES[APAC_PR$UF_RES == "35"] <- "SP"
APAC_PR$UF_RES[APAC_PR$UF_RES == "41"] <- "PR"
APAC_PR$UF_RES[APAC_PR$UF_RES == "42"] <- "SC"
APAC_PR$UF_RES[APAC_PR$UF_RES == "43"] <- "RS"
APAC_PR$UF_RES[APAC_PR$UF_RES == "50"] <- "MS"
APAC_PR$UF_RES[APAC_PR$UF_RES == "51"] <- "MT"
APAC_PR$UF_RES[APAC_PR$UF_RES == "52"] <- "GO"
APAC_PR$UF_RES[APAC_PR$UF_RES == "53"] <- "DF"

APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "11"] <- "RO"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "12"] <- "AC"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "13"] <- "AM"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "14"] <- "RR"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "15"] <- "PA"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "16"] <- "AP"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "17"] <- "TO"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "21"] <- "MA"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "22"] <- "PI"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "23"] <- "CE"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "24"] <- "RN"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "25"] <- "PB"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "26"] <- "PE"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "27"] <- "AL"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "28"] <- "SE"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "29"] <- "BA"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "31"] <- "MG"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "32"] <- "ES"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "33"] <- "RJ"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "35"] <- "SP"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "41"] <- "PR"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "42"] <- "SC"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "43"] <- "RS"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "50"] <- "MS"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "51"] <- "MT"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "52"] <- "GO"
APAC_PR$UF_OCOR[APAC_PR$UF_OCOR == "53"] <- "DF"


### Saving file csv
write.csv(APAC_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/APAC_PR_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_APAC_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_APAC_PR.csv', row.names=FALSE)

##### APAC_PO ######

#### Factor to chr
APAC_PO$`HEPC.APAC$HEPC` <- as.character(APAC_PO$`HEPC.APAC$HEPC`)  
APAC_PO$`HEPB.APAC$HEPB` <- as.character(APAC_PO$`HEPB.APAC$HEPB`)
APAC_PO$`HEPA.APAC$HEPA` <- as.character(APAC_PO$`HEPA.APAC$HEPA`)
APAC_PO$`HEPD.APAC$HEPD` <- as.character(APAC_PO$`HEPD.APAC$HEPD`)
APAC_PO$`HEPI.APAC$HEPI` <- as.character(APAC_PO$`HEPI.APAC$HEPI`)

### MERGING columns 
APAC_PO <- unite(APAC_PO, "CID", c("CO_CID_PRINCIPAL", "CO_CID_SECUNDARIO"))
APAC_PO <- unite(APAC_PO, "PROCEDIMENTO", c("CO_PROCEDIMENTO_PRINCIPAL", "CO_PROCEDIMENTO_SECUNDARIO"))
APAC_PO <- unite(APAC_PO, "HEPATITE", c("HEPB.APAC$HEPB", "HEPA.APAC$HEPA", "HEPD.APAC$HEPD", "HEPI.APAC$HEPI", "HEPC.APAC$HEPC")) 

#### oRDERING COLUMNS
APAC_PO <- select(APAC_PO, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(APAC_PO$DT_NASC))
YEAR2 <- data_frame(year = ymd(APAC_PO$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- APAC_PO$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_APAC_PO <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

APAC_PO <- cbind.data.frame(APAC_PO ,IDADE)

APAC_PO$IDADE <- gsub("[S]", "", APAC_PO$IDADE)
SEXO_IDADE_APAC_PO$IDADE <- gsub("[S]", "", SEXO_IDADE_APAC_PO$IDADE)

#### UF 

UF_RES <- substr(APAC_PO$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(APAC_PO$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

APAC_PO <- cbind.data.frame(APAC_PO, UF_RES)

APAC_PO<- cbind.data.frame(APAC_PO, UF_OCOR)

APAC_PO$UF_RES <- as.character(APAC_PO$UF_RES)
APAC_PO$UF_OCOR <- as.character(APAC_PO$UF_OCOR)

APAC_PO$UF_RES[APAC_PO$UF_RES == "11"] <- "RO"
APAC_PO$UF_RES[APAC_PO$UF_RES == "12"] <- "AC"
APAC_PO$UF_RES[APAC_PO$UF_RES == "13"] <- "AM"
APAC_PO$UF_RES[APAC_PO$UF_RES == "14"] <- "RR"
APAC_PO$UF_RES[APAC_PO$UF_RES == "15"] <- "PA"
APAC_PO$UF_RES[APAC_PO$UF_RES == "16"] <- "AP"
APAC_PO$UF_RES[APAC_PO$UF_RES == "17"] <- "TO"
APAC_PO$UF_RES[APAC_PO$UF_RES == "21"] <- "MA"
APAC_PO$UF_RES[APAC_PO$UF_RES == "22"] <- "PI"
APAC_PO$UF_RES[APAC_PO$UF_RES == "23"] <- "CE"
APAC_PO$UF_RES[APAC_PO$UF_RES == "24"] <- "RN"
APAC_PO$UF_RES[APAC_PO$UF_RES == "25"] <- "PB"
APAC_PO$UF_RES[APAC_PO$UF_RES == "26"] <- "PE"
APAC_PO$UF_RES[APAC_PO$UF_RES == "27"] <- "AL"
APAC_PO$UF_RES[APAC_PO$UF_RES == "28"] <- "SE"
APAC_PO$UF_RES[APAC_PO$UF_RES == "29"] <- "BA"
APAC_PO$UF_RES[APAC_PO$UF_RES == "31"] <- "MG"
APAC_PO$UF_RES[APAC_PO$UF_RES == "32"] <- "ES"
APAC_PO$UF_RES[APAC_PO$UF_RES == "33"] <- "RJ"
APAC_PO$UF_RES[APAC_PO$UF_RES == "35"] <- "SP"
APAC_PO$UF_RES[APAC_PO$UF_RES == "41"] <- "PR"
APAC_PO$UF_RES[APAC_PO$UF_RES == "42"] <- "SC"
APAC_PO$UF_RES[APAC_PO$UF_RES == "43"] <- "RS"
APAC_PO$UF_RES[APAC_PO$UF_RES == "50"] <- "MS"
APAC_PO$UF_RES[APAC_PO$UF_RES == "51"] <- "MT"
APAC_PO$UF_RES[APAC_PO$UF_RES == "52"] <- "GO"
APAC_PO$UF_RES[APAC_PO$UF_RES == "53"] <- "DF"

APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "11"] <- "RO"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "12"] <- "AC"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "13"] <- "AM"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "14"] <- "RR"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "15"] <- "PA"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "16"] <- "AP"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "17"] <- "TO"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "21"] <- "MA"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "22"] <- "PI"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "23"] <- "CE"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "24"] <- "RN"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "25"] <- "PB"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "26"] <- "PE"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "27"] <- "AL"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "28"] <- "SE"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "29"] <- "BA"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "31"] <- "MG"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "32"] <- "ES"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "33"] <- "RJ"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "35"] <- "SP"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "41"] <- "PR"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "42"] <- "SC"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "43"] <- "RS"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "50"] <- "MS"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "51"] <- "MT"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "52"] <- "GO"
APAC_PO$UF_OCOR[APAC_PO$UF_OCOR == "53"] <- "DF"

### Saving file csv
write.csv(APAC_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/APAC_PO_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_APAC_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_APAC_PO.csv', row.names=FALSE)

##### BPAI_PR ######

#### Factor to chr
BPAI_PR$`HEPC.BPAI$HEPC` <- as.character(BPAI_PR$`HEPC.BPAI$HEPC`)  
BPAI_PR$`HEPB.BPAI$HEPB` <- as.character(BPAI_PR$`HEPB.BPAI$HEPB`)
BPAI_PR$`HEPA.BPAI$HEPA` <- as.character(BPAI_PR$`HEPA.BPAI$HEPA`)
BPAI_PR$`HEPD.BPAI$HEPD` <- as.character(BPAI_PR$`HEPD.BPAI$HEPD`)
BPAI_PR$`HEPI.BPAI$HEPI` <- as.character(BPAI_PR$`HEPI.BPAI$HEPI`)

### MERGING columns 
BPAI_PR <- unite(BPAI_PR, "HEPATITE", c("HEPC.BPAI.HEPC","HEPD.BPAI.HEPD" , "HEPB.BPAI.HEPB", "HEPI.BPAI.HEPI", "HEPA.BPAI.HEPA" )) 

#### ORDERING COLUMNS
BPAI_PR <- select(BPAI_PR, ID_PACIENTE, CID = CO_CID_PRINCIPAL, PROCEDIMENTO = CO_PROCEDIMENTO_REALIZADO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(BPAI_PR$DT_NASC))
YEAR2 <- data_frame(year = ymd(BPAI_PR$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- BPAI_PR$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_BPAI_PR <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

BPAI_PR <- cbind.data.frame(BPAI_PR ,IDADE)

BPAI_PR$IDADE <- gsub("[S]", "", BPAI_PR$IDADE)
SEXO_IDADE_BPAI_PR$IDADE <- gsub("[S]", "", SEXO_IDADE_BPAI_PR$IDADE)


#### UF 

UF_RES <- substr(BPAI_PR$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(BPAI_PR$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

BPAI_PR <- cbind.data.frame(BPAI_PR, UF_RES)

BPAI_PR <- cbind.data.frame(BPAI_PR, UF_OCOR)

BPAI_PR$UF_RES <- as.character(BPAI_PR$UF_RES)
BPAI_PR$UF_OCOR <- as.character(BPAI_PR$UF_OCOR)

BPAI_PR$UF_RES[BPAI_PR$UF_RES == "11"] <- "RO"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "12"] <- "AC"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "13"] <- "AM"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "14"] <- "RR"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "15"] <- "PA"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "16"] <- "AP"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "17"] <- "TO"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "21"] <- "MA"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "22"] <- "PI"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "23"] <- "CE"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "24"] <- "RN"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "25"] <- "PB"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "26"] <- "PE"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "27"] <- "AL"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "28"] <- "SE"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "29"] <- "BA"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "31"] <- "MG"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "32"] <- "ES"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "33"] <- "RJ"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "35"] <- "SP"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "41"] <- "PR"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "42"] <- "SC"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "43"] <- "RS"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "50"] <- "MS"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "51"] <- "MT"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "52"] <- "GO"
BPAI_PR$UF_RES[BPAI_PR$UF_RES == "53"] <- "DF"

BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "11"] <- "RO"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "12"] <- "AC"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "13"] <- "AM"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "14"] <- "RR"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "15"] <- "PA"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "16"] <- "AP"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "17"] <- "TO"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "21"] <- "MA"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "22"] <- "PI"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "23"] <- "CE"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "24"] <- "RN"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "25"] <- "PB"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "26"] <- "PE"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "27"] <- "AL"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "28"] <- "SE"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "29"] <- "BA"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "31"] <- "MG"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "32"] <- "ES"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "33"] <- "RJ"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "35"] <- "SP"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "41"] <- "PR"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "42"] <- "SC"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "43"] <- "RS"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "50"] <- "MS"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "51"] <- "MT"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "52"] <- "GO"
BPAI_PR$UF_OCOR[BPAI_PR$UF_OCOR == "53"] <- "DF"


### Saving file csv
write.csv(BPAI_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/BPAI_PR_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_BPAI_PR , file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_BPAI_PR.csv', row.names=FALSE)

##### BPAI_PO ######

#### Factor to chr
BPAI_PO$`HEPC.BPAI$HEPC` <- as.character(BPAI_PO$`HEPC.BPAI$HEPC`)  
BPAI_PO$`HEPB.BPAI$HEPB` <- as.character(BPAI_PO$`HEPB.BPAI$HEPB`)
BPAI_PO$`HEPA.BPAI$HEPA` <- as.character(BPAI_PO$`HEPA.BPAI$HEPA`)
BPAI_PO$`HEPD.BPAI$HEPD` <- as.character(BPAI_PO$`HEPD.BPAI$HEPD`)
BPAI_PO$`HEPI.BPAI$HEPI` <- as.character(BPAI_PO$`HEPI.BPAI$HEPI`)

### MERGING columns 

BPAI_PO <- unite(BPAI_PO, "HEPATITE", c( "HEPC.BPAI.HEPC" , "HEPD.BPAI.HEPD" , "HEPB.BPAI.HEPB",  "HEPI.BPAI.HEPI", "HEPA.BPAI.HEPA")) 

#### ORDERING COLUMNS
BPAI_PO <- select(BPAI_PO, ID_PACIENTE, CID = CO_CID_PRINCIPAL, PROCEDIMENTO = CO_PROCEDIMENTO_REALIZADO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(BPAI_PO$DT_NASC))
YEAR2 <- data_frame(year = ymd(BPAI_PO$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- BPAI_PO$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_BPAI_PO <- cbind.data.frame(SEXO, IDADE)
BPAI_PO <- cbind.data.frame(BPAI_PO ,IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

BPAI_PO$IDADE <- gsub("[S]", "", BPAI_PO$IDADE)
SEXO_IDADE_BPAI_PO$IDADE <- gsub("[S]", "", SEXO_IDADE_BPAI_PO$IDADE)


#### UF 

UF_RES <- substr(BPAI_PO$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)

UF_OCOR <- substr(BPAI_PO$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

BPAI_PO <- cbind.data.frame(BPAI_PO, UF_RES)

BPAI_PO <- cbind.data.frame(BPAI_PO, UF_OCOR)

BPAI_PO$UF_RES <- as.character(BPAI_PO$UF_RES)
BPAI_PO$UF_OCOR <- as.character(BPAI_PO$UF_OCOR)

BPAI_PO$UF_RES[BPAI_PO$UF_RES == "11"] <- "RO"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "12"] <- "AC"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "13"] <- "AM"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "14"] <- "RR"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "15"] <- "PA"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "16"] <- "AP"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "17"] <- "TO"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "21"] <- "MA"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "22"] <- "PI"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "23"] <- "CE"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "24"] <- "RN"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "25"] <- "PB"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "26"] <- "PE"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "27"] <- "AL"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "28"] <- "SE"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "29"] <- "BA"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "31"] <- "MG"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "32"] <- "ES"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "33"] <- "RJ"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "35"] <- "SP"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "41"] <- "PR"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "42"] <- "SC"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "43"] <- "RS"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "50"] <- "MS"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "51"] <- "MT"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "52"] <- "GO"
BPAI_PO$UF_RES[BPAI_PO$UF_RES == "53"] <- "DF"

BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "11"] <- "RO"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "12"] <- "AC"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "13"] <- "AM"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "14"] <- "RR"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "15"] <- "PA"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "16"] <- "AP"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "17"] <- "TO"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "21"] <- "MA"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "22"] <- "PI"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "23"] <- "CE"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "24"] <- "RN"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "25"] <- "PB"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "26"] <- "PE"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "27"] <- "AL"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "28"] <- "SE"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "29"] <- "BA"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "31"] <- "MG"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "32"] <- "ES"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "33"] <- "RJ"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "35"] <- "SP"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "41"] <- "PR"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "42"] <- "SC"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "43"] <- "RS"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "50"] <- "MS"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "51"] <- "MT"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "52"] <- "GO"
BPAI_PO$UF_OCOR[BPAI_PO$UF_OCOR == "53"] <- "DF"

### Saving file csv
write.csv(BPAI_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/BPAI_PO_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_BPAI_PO , file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_BPAI_PO.csv', row.names=FALSE)

##### SIM_PO ######

#### Factor to chr
SIM_PO$`HEPC.SIM$HEPC` <- as.character(SIM_PO$`HEPC.SIM$HEPC`)  
SIM_PO$`HEPB.SIM$HEPB` <- as.character(SIM_PO$`HEPB.SIM$HEPB`)
SIM_PO$`HEPA.SIM$HEPA` <- as.character(SIM_PO$`HEPA.SIM$HEPA`)
SIM_PO$`HEPD.SIM$HEPD` <- as.character(SIM_PO$`HEPD.SIM$HEPD`)
SIM_PO$`HEPI.SIM$HEPI` <- as.character(SIM_PO$`HEPI.SIM$HEPI`)

### MERGING columns 

SIM_PO <- unite(SIM_PO, "HEPATITE", c("HEPB.SIM$HEPB", "HEPA.SIM$HEPA", "HEPD.SIM$HEPD", "HEPC.SIM$HEPC", "HEPI.SIM$HEPI")) 
SIM_PO <- unite(SIM_PO, "CID", c( "ATESTADO" ,"CAUSABAS",  "LINHAA","LINHAB",  "LINHAC",  "LINHAD" , "LINHAII" )) 

##### Create column PROCEDIMENTO
SIM_PO$PROCEDIMENTO <- "999999999"

#### ORDERING COLUMNS
SIM_PO <- select(SIM_PO, ID_PACIENTE, CID , PROCEDIMENTO , SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(SIM_PO$DT_NASC))
YEAR2 <- data_frame(year = ymd(SIM_PO$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- SIM_PO$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_SIM_PO <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)

SIM_PO <- cbind.data.frame(SIM_PO ,IDADE)

SIM_PO$IDADE <- gsub("[S]", "", SIM_PO$IDADE)
SEXO_IDADE_SIM_PO$IDADE <- gsub("[S]", "", SEXO_IDADE_SIM_PO$IDADE)

#### UF 

UF_RES <- substr(SIM_PO$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(SIM_PO$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

SIM_PO <- cbind.data.frame(SIM_PO, UF_RES)

SIM_PO <- cbind.data.frame(SIM_PO, UF_OCOR)

SIM_PO$UF_RES <- as.character(SIM_PO$UF_RES)
SIM_PO$UF_OCOR <- as.character(SIM_PO$UF_OCOR)

SIM_PO$UF_RES[SIM_PO$UF_RES == "11"] <- "RO"
SIM_PO$UF_RES[SIM_PO$UF_RES == "12"] <- "AC"
SIM_PO$UF_RES[SIM_PO$UF_RES == "13"] <- "AM"
SIM_PO$UF_RES[SIM_PO$UF_RES == "14"] <- "RR"
SIM_PO$UF_RES[SIM_PO$UF_RES == "15"] <- "PA"
SIM_PO$UF_RES[SIM_PO$UF_RES == "16"] <- "AP"
SIM_PO$UF_RES[SIM_PO$UF_RES == "17"] <- "TO"
SIM_PO$UF_RES[SIM_PO$UF_RES == "21"] <- "MA"
SIM_PO$UF_RES[SIM_PO$UF_RES == "22"] <- "PI"
SIM_PO$UF_RES[SIM_PO$UF_RES == "23"] <- "CE"
SIM_PO$UF_RES[SIM_PO$UF_RES == "24"] <- "RN"
SIM_PO$UF_RES[SIM_PO$UF_RES == "25"] <- "PB"
SIM_PO$UF_RES[SIM_PO$UF_RES == "26"] <- "PE"
SIM_PO$UF_RES[SIM_PO$UF_RES == "27"] <- "AL"
SIM_PO$UF_RES[SIM_PO$UF_RES == "28"] <- "SE"
SIM_PO$UF_RES[SIM_PO$UF_RES == "29"] <- "BA"
SIM_PO$UF_RES[SIM_PO$UF_RES == "31"] <- "MG"
SIM_PO$UF_RES[SIM_PO$UF_RES == "32"] <- "ES"
SIM_PO$UF_RES[SIM_PO$UF_RES == "33"] <- "RJ"
SIM_PO$UF_RES[SIM_PO$UF_RES == "35"] <- "SP"
SIM_PO$UF_RES[SIM_PO$UF_RES == "41"] <- "PR"
SIM_PO$UF_RES[SIM_PO$UF_RES == "42"] <- "SC"
SIM_PO$UF_RES[SIM_PO$UF_RES == "43"] <- "RS"
SIM_PO$UF_RES[SIM_PO$UF_RES == "50"] <- "MS"
SIM_PO$UF_RES[SIM_PO$UF_RES == "51"] <- "MT"
SIM_PO$UF_RES[SIM_PO$UF_RES == "52"] <- "GO"
SIM_PO$UF_RES[SIM_PO$UF_RES == "53"] <- "DF"

SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "11"] <- "RO"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "12"] <- "AC"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "13"] <- "AM"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "14"] <- "RR"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "15"] <- "PA"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "16"] <- "AP"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "17"] <- "TO"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "21"] <- "MA"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "22"] <- "PI"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "23"] <- "CE"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "24"] <- "RN"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "25"] <- "PB"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "26"] <- "PE"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "27"] <- "AL"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "28"] <- "SE"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "29"] <- "BA"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "31"] <- "MG"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "32"] <- "ES"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "33"] <- "RJ"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "35"] <- "SP"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "41"] <- "PR"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "42"] <- "SC"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "43"] <- "RS"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "50"] <- "MS"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "51"] <- "MT"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "52"] <- "GO"
SIM_PO$UF_OCOR[SIM_PO$UF_OCOR == "53"] <- "DF"

## Saving file csv
write.csv(SIM_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SIM_PO_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_SIM_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_SIM_PO.csv', row.names=FALSE)

##### SIM_PR ######
#### Factor to chr
SIM_PR$`HEPC.SIM$HEPC` <- as.character(SIM_PR$`HEPC.SIM$HEPC`)  
SIM_PR$`HEPB.SIM$HEPB` <- as.character(SIM_PR$`HEPB.SIM$HEPB`)
SIM_PR$`HEPA.SIM$HEPA` <- as.character(SIM_PR$`HEPA.SIM$HEPA`)
SIM_PR$`HEPD.SIM$HEPD` <- as.character(SIM_PR$`HEPD.SIM$HEPD`)
SIM_PR$`HEPI.SIM$HEPI` <- as.character(SIM_PR$`HEPI.SIM$HEPI`)

### MERGING columns 
SIM_PR <- unite(SIM_PR, "HEPATITE", c("HEPA.SIM.HEPA", "HEPD.SIM.HEPD", "HEPI.SIM.HEPI", "HEPC.SIM.HEPC","HEPB.SIM.HEPB" )) 
SIM_PR <- unite(SIM_PR, "CID", c(  "ATESTADO" ,"CAUSABAS",  "LINHAA","LINHAB",  "LINHAC",  "LINHAD" , "LINHAII"  )) 

##### Create column PROCEDIMENTO
SIM_PR$PROCEDIMENTO <- "999999999"

#### ORDERING COLUMNS
SIM_PR <- select(SIM_PR, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, HEPATITE, DB_ORIGEM) 

##### IDADE
YEAR1 <- data_frame(year  = ymd(SIM_PR$DT_NASC))
YEAR2 <- data_frame(year = ymd(SIM_PR$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- SIM_PR$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_SIM_PR <- cbind.data.frame(SEXO, IDADE)

SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)


SEXO_IDADE_SIM_PR$IDADE <- gsub("[S]", "", SEXO_IDADE_SIM_PR$IDADE)

SIM_PR <- cbind.data.frame(SIM_PR ,IDADE)
SIM_PR$IDADE <- gsub("[S]", "", SIM_PR$IDADE)


#### UF 

UF_RES <- substr(SIM_PR$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(SIM_PR$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

SIM_PR <- cbind.data.frame(SIM_PR, UF_RES)

SIM_PR <- cbind.data.frame(SIM_PR, UF_OCOR)

SIM_PR$UF_RES <- as.character(SIM_PR$UF_RES)
SIM_PR$UF_OCOR <- as.character(SIM_PR$UF_OCOR)

SIM_PR$UF_RES[SIM_PR$UF_RES == "11"] <- "RO"
SIM_PR$UF_RES[SIM_PR$UF_RES == "12"] <- "AC"
SIM_PR$UF_RES[SIM_PR$UF_RES == "13"] <- "AM"
SIM_PR$UF_RES[SIM_PR$UF_RES == "14"] <- "RR"
SIM_PR$UF_RES[SIM_PR$UF_RES == "15"] <- "PA"
SIM_PR$UF_RES[SIM_PR$UF_RES == "16"] <- "AP"
SIM_PR$UF_RES[SIM_PR$UF_RES == "17"] <- "TO"
SIM_PR$UF_RES[SIM_PR$UF_RES == "21"] <- "MA"
SIM_PR$UF_RES[SIM_PR$UF_RES == "22"] <- "PI"
SIM_PR$UF_RES[SIM_PR$UF_RES == "23"] <- "CE"
SIM_PR$UF_RES[SIM_PR$UF_RES == "24"] <- "RN"
SIM_PR$UF_RES[SIM_PR$UF_RES == "25"] <- "PB"
SIM_PR$UF_RES[SIM_PR$UF_RES == "26"] <- "PE"
SIM_PR$UF_RES[SIM_PR$UF_RES == "27"] <- "AL"
SIM_PR$UF_RES[SIM_PR$UF_RES == "28"] <- "SE"
SIM_PR$UF_RES[SIM_PR$UF_RES == "29"] <- "BA"
SIM_PR$UF_RES[SIM_PR$UF_RES == "31"] <- "MG"
SIM_PR$UF_RES[SIM_PR$UF_RES == "32"] <- "ES"
SIM_PR$UF_RES[SIM_PR$UF_RES == "33"] <- "RJ"
SIM_PR$UF_RES[SIM_PR$UF_RES == "35"] <- "SP"
SIM_PR$UF_RES[SIM_PR$UF_RES == "41"] <- "PR"
SIM_PR$UF_RES[SIM_PR$UF_RES == "42"] <- "SC"
SIM_PR$UF_RES[SIM_PR$UF_RES == "43"] <- "RS"
SIM_PR$UF_RES[SIM_PR$UF_RES == "50"] <- "MS"
SIM_PR$UF_RES[SIM_PR$UF_RES == "51"] <- "MT"
SIM_PR$UF_RES[SIM_PR$UF_RES == "52"] <- "GO"
SIM_PR$UF_RES[SIM_PR$UF_RES == "53"] <- "DF"

SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "11"] <- "RO"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "12"] <- "AC"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "13"] <- "AM"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "14"] <- "RR"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "15"] <- "PA"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "16"] <- "AP"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "17"] <- "TO"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "21"] <- "MA"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "22"] <- "PI"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "23"] <- "CE"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "24"] <- "RN"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "25"] <- "PB"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "26"] <- "PE"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "27"] <- "AL"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "28"] <- "SE"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "29"] <- "BA"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "31"] <- "MG"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "32"] <- "ES"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "33"] <- "RJ"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "35"] <- "SP"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "41"] <- "PR"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "42"] <- "SC"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "43"] <- "RS"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "50"] <- "MS"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "51"] <- "MT"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "52"] <- "GO"
SIM_PR$UF_OCOR[SIM_PR$UF_OCOR == "53"] <- "DF"

## Saving file csv
write.csv(SIM_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SIM_PR_BDcompleto.csv', row.names=FALSE)

write.csv(SEXO_IDADE_SIM_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SEXO_IDADE_SIM_PR.csv', row.names=FALSE)

#######################
########### Merge df
#######################

########
### Loading files
########

AIH_PR_BDcompleto <- read_csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/AIH_PR/AIH_PR_BDcompleto.csv")

APAC_PR_BDcompleto <- read_csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/APAC_PR/APAC_PR_BDcompleto.csv")

BPAI_PR_BDcompleto <- read_csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/BPAI_PR/BPAI_PR_BDcompleto.csv")

SIM_PR_BDcompleto <- read_csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_BDCOMPLETO/SIM_PR/SIM_PR_BDcompleto.csv")

##########
### Preparing files
##########

#### AIH_PR_BDcompleto

AIH_PR_BDcompleto$ID_PACIENTE <- as.character(AIH_PR_BDcompleto$ID_PACIENTE)
AIH_PR_BDcompleto$MUN_RES <- as.character(AIH_PR_BDcompleto$MUN_RES)
AIH_PR_BDcompleto$MUN_OCOR <- as.character(AIH_PR_BDcompleto$MUN_OCOR)
AIH_PR_BDcompleto$IDADE <- as.character(AIH_PR_BDcompleto$IDADE)
AIH_PR_BDcompleto$PROCEDIMENTO <- as.character(AIH_PR_BDcompleto$PROCEDIMENTO)

#### BPAI_PR_BDcompleto

BPAI_PR_BDcompleto$ID_PACIENTE <- as.character(BPAI_PR_BDcompleto$ID_PACIENTE)
BPAI_PR_BDcompleto$MUN_RES <- as.character(BPAI_PR_BDcompleto$MUN_RES)
BPAI_PR_BDcompleto$MUN_OCOR <- as.character(BPAI_PR_BDcompleto$MUN_OCOR)
BPAI_PR_BDcompleto$IDADE <- as.character(BPAI_PR_BDcompleto$IDADE)
BPAI_PR_BDcompleto$PROCEDIMENTO <- as.character(BPAI_PR_BDcompleto$PROCEDIMENTO)

#### SIM_PR_BDcompleto

SIM_PR_BDcompleto$ID_PACIENTE <- as.character(SIM_PR_BDcompleto$ID_PACIENTE)
SIM_PR_BDcompleto$MUN_RES <- as.character(SIM_PR_BDcompleto$MUN_RES)
SIM_PR_BDcompleto$MUN_OCOR <- as.character(SIM_PR_BDcompleto$MUN_OCOR)
SIM_PR_BDcompleto$IDADE <- as.character(SIM_PR_BDcompleto$IDADE)
SIM_PR_BDcompleto$PROCEDIMENTO <- as.character(SIM_PR_BDcompleto$PROCEDIMENTO)

#### APAC_PR_BDcompleto

APAC_PR_BDcompleto$ID_PACIENTE <- as.character(APAC_PR_BDcompleto$ID_PACIENTE)
APAC_PR_BDcompleto$MUN_RES <- as.character(APAC_PR_BDcompleto$MUN_RES)
APAC_PR_BDcompleto$MUN_OCOR <- as.character(APAC_PR_BDcompleto$MUN_OCOR)
APAC_PR_BDcompleto$IDADE <- as.character(APAC_PR_BDcompleto$IDADE)
APAC_PR_BDcompleto$PROCEDIMENTO <- as.character(APAC_PR_BDcompleto$PROCEDIMENTO)

############
##### Bind rows
############
#### Create a list of all tables
list_PR <- list(AIH_PR_BDcompleto, APAC_PR_BDcompleto, BPAI_PR_BDcompleto, SIM_PR_BDcompleto)

####### Bind rows f al tables
bind_rows_PR_merged <- bind_rows(list_PR)

###### Check the DB origin in the table containing all DB tables 
table(bind_rows_PR_merged$DB_ORIGEM)

###### Select the distinct IDs 
distinct_bind_rows_PR_merged = bind_rows_PR_merged %>% distinct(bind_rows_PR_merged$ID_PACIENTE, .keep_all = TRUE)

###### Check the DB origin in the final tabe 
table(distinct_bind_rows_PR_merged$DB_ORIGEM)

#### Save final tables ## Distinct and Complete DB table
write.csv(bind_rows_PR_merged, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/bind_rows_PR_merged.csv', row.names=FALSE)
write.csv(distinct_bind_rows_PR_merged, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/distinct_bind_rows_PR_merged.csv', row.names=FALSE)

######################
####### Intersect and setdiff #### Merged df vs SINAM (not notified individuals)
#####################
PR_distinct <- select(distinct_bind_rows_PR_merged, ID_PACIENTE , CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC , DT_OCOR, DB_ORIGEM, HEPATITE, IDADE,  UF_RES, UF_OCOR )

###### Intersect SINAN - distinct PR

intersect_SINAN_distinct_PR <- data.frame(C= intersect(SINAN[,1], PR_distinct[,1] ))

##### OR #####

intersect_SINAN_distinct_PR <- data.frame(intersect(SINAN$ID_PACIENTE, PR_distinct$ID_PACIENTE))

##############

intersect_distinct_PR_SINAN <- data.frame(C= intersect(PR_distinct[,1], SINAN[,1] ))

####### OR #####

intersect_distinct_PR_SINAN <- data.frame(intersect(PR_distinct$ID_PACIENTE, SINAN$ID_PACIENTE))


###### Undereported individual(setdiff) SINAN - distinct PR

setdiff_SINAN_distinct_PR <- data.frame(C= setdiff(SINAN[,1], PR_distinct[,1] ))

##### OR #####

setdiff_SINAN_distinct_PR <- data.frame(setdiff(SINAN$ID_PACIENTE, PR_distinct$ID_PACIENTE))

##############

setdiff_distinct_PR_SINAN <- data.frame(C= setdiff(PR_distinct[,1], SINAN[,1] ))

###### OR #####

setdiff_distinct_PR_SINAN <- data.frame(setdiff(PR_distinct$ID_PACIENTE, SINAN$ID_PACIENTE))

##### HOW to recover the information based on the ID column from intersect or setdiff

###### SETDIFF - undereported individuals
#### 1st rename the setdiff column
setdiff_distinct_PR_SINAN <- select(setdiff_distinct_PR_SINAN, ID_PACIENTE = setdiff.PR_distinct.ID_PACIENTE..SINAN.ID_PACIENTE.)

##### 2nd change to chr
setdiff_distinct_PR_SINAN$ID_PACIENTE <- as.character(setdiff_distinct_PR_SINAN$ID_PACIENTE)

#### 3rd bring the information from a df containing all IDs
setdiff_distinct_PR_SINAN_BDcomplete <- inner_join(setdiff_distinct_PR_SINAN, bindrows_PR_distinct_SINAN,by="ID_PACIENTE")

##### Save file

write.csv(setdiff_distinct_PR_SINAN_BDcomplete, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete.csv', row.names=FALSE)

###### INTERSECT/INNER_JOIN -  individuals present in SINAN and DB
#### 1st rename the intersect column
intersect_distinct_PR_SINAN <- select(intersect_distinct_PR_SINAN, ID_PACIENTE = intersect.PR_distinct.ID_PACIENTE..SINAN.ID_PACIENTE.)

##### 2nd change to chr
intersect_distinct_PR_SINAN$ID_PACIENTE <- as.character(intersect_distinct_PR_SINAN$ID_PACIENTE)

#### 3rd bring the information from a df containing all IDs
inner_join_distinct_PR_SINAN_BDcomplete <- inner_join(PR_distinct, SINAN,by="ID_PACIENTE")

#### organize table and merge columns from inner_join

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "CID", c("CID.x" , "CID.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "PROCEDIMENTO", c("PROCEDIMENTO.x" , "PROCEDIMENTO.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "SEXO", c("SEXO.x" , "SEXO.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "MUN_RES", c("MUN_RES.x" , "MUN_RES.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "MUN_OCOR", c("MUN_OCOR.x" , "MUN_OCOR.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "DT_NASC", c("DT_NASC.x" , "DT_NASC.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "DT_OCOR", c("DT_OCOR.x" , "DT_OCOR.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "DB_ORIGEM", c("DB_ORIGEM.x" , "DB_ORIGEM.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "HEPATITE", c("HEPATITE.x" , "HEPATITE.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "IDADE", c("IDADE.x" , "IDADE.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "UF_RES", c("UF_RES.x" , "UF_RES.y" ))

inner_join_distinct_PR_SINAN_BDcomplete <- unite(inner_join_distinct_PR_SINAN_BDcomplete, "UF_OCOR", c("UF_OCOR.x" , "UF_OCOR.y" ))

##### Save file

write.csv(inner_join_distinct_PR_SINAN_BDcomplete, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/intersect_interseccao/inner_join_distinct_PR_SINAN_BDcomplete.csv', row.names=FALSE)

##### remove NAs
inner_join_distinct_PR_SINAN_BDcomplete <- na.omit(inner_join_distinct_PR_SINAN_BDcomplete)

###### remove duplicates
inner_join_distinct_PR_SINAN_BDcomplete = inner_join_distinct_PR_SINAN_BDcomplete %>% distinct(inner_join_distinct_PR_SINAN_BDcomplete$ID_PACIENTE, .keep_all = TRUE)

#### Order
inner_join_distinct_PR_SINAN_BDcomplete <- select(inner_join_distinct_PR_SINAN_BDcomplete, ID_PACIENTE, CID, PROCEDIMENTO, SEXO, MUN_RES, MUN_OCOR, DT_NASC, DT_OCOR, DT_NASC, DT_OCOR, DB_ORIGEM,  HEPATITE, IDADE, UF_RES, UF_OCOR )

#######################
##### Summary Stats ### Count by Hepatitis types, Count by Gender , Count by UF and Count by city
#######################

##### SETDIFF - "subnotificacao"
#### Count by Hepatitis type
setdiff_distinct_PR_SINAN_BDcomplete_HEP_SUMMARY <- table(setdiff_distinct_PR_SINAN_BDcomplete$HEPATITE, useNA = "always")

setdiff_distinct_PR_SINAN_BDcomplete_HEP_SUMMARY <- as.data.frame(setdiff_distinct_PR_SINAN_BDcomplete_HEP_SUMMARY)

#### Count by Gender
setdiff_distinct_PR_SINAN_BDcomplete_GENDER_SUMMARY <- table(setdiff_distinct_PR_SINAN_BDcomplete$SEXO, useNA = "always")

setdiff_distinct_PR_SINAN_BDcomplete_GENDER_SUMMARY <- as.data.frame(setdiff_distinct_PR_SINAN_BDcomplete_GENDER_SUMMARY)

##### Count by UF and Count UF residence
setdiff_distinct_PR_SINAN_BDcomplete_UF_RES <- table(setdiff_distinct_PR_SINAN_BDcomplete$UF_RES, useNA = "always")

setdiff_distinct_PR_SINAN_BDcomplete_UF_RES <- as.data.frame(setdiff_distinct_PR_SINAN_BDcomplete_UF_RES)

##### Count by UF and Count by UF hospital
setdiff_distinct_PR_SINAN_BDcomplete_UF_OCOR <- table(setdiff_distinct_PR_SINAN_BDcomplete$UF_OCOR, useNA = "always")

setdiff_distinct_PR_SINAN_BDcomplete_UF_OCOR <- as.data.frame(setdiff_distinct_PR_SINAN_BDcomplete_UF_OCOR)


write.csv(setdiff_distinct_PR_SINAN_BDcomplete_HEP_SUMMARY, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete_HEP_SUMMARY.csv', row.names=FALSE)
write.csv(setdiff_distinct_PR_SINAN_BDcomplete_GENDER_SUMMARY, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete_GENDER_SUMMARY.csv', row.names=FALSE)
write.csv(setdiff_distinct_PR_SINAN_BDcomplete_UF_RES, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete_UF_RES_SUMMARY.csv', row.names=FALSE)
write.csv(setdiff_distinct_PR_SINAN_BDcomplete_UF_OCOR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/BIND_ROW_and_DISTINCT/setdiff_subnotificacao/setdiff_distinct_PR_SINAN_BDcomplete_UF_OCOR_SUMMARY.csv', row.names=FALSE)

