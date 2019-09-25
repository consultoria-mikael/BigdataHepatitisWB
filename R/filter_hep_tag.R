############ New filter - data frame #############
##################################################
###### script developed by Mikael Lemos ##########
###### Version 1.0 - 02.04.2019 ##################
## Filtering, classification of Hepatitis types ##
# and tag with DB origin #########################

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
df.AIH <- read.csv("/Users/mikaellemos/Projeto_BDBM/SEMFILTROTEMPO/DBAIH_AIH_FILTRADO.csv") 

#### PC PO700
df.AIH <- read.csv("/Users/alexandre.fonseca/Desktop/samples/sem_filtro_tempo/DBAIH_AIH_FILTRADO.csv") 

##### APAC ####
#### MAC mikael
df.APAC <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/SEMFILTROTEMPO/DBSIA_APAC_FILTRADO.csv")

#### PC PO700
df.APAC <- read.csv("/Users/alexandre.fonseca/Desktop/samples/sem_filtro_tempo/DBSIA_APAC_FILTRADO.csv")

#### BPAI #####
#### MAC mikael
df.BPAI <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/SEMFILTROTEMPO/DBSIA_BPAI_FILTRADO.csv")

#### PC PO700
df.BPAI <- read.csv("/Users/alexandre.fonseca/Desktop/samples/sem_filtro_tempo/DBSIA_BPAI_FILTRADO.csv")

#### SIM ######
#### MAC mikael
df.SIM <- read.csv("/Users/mikaellemos/Projeto_BDBM/SEMFILTROTEMPO/DBSIM_FILTRADO.csv")

#### PC PO700
df.SIM <- read.csv("/Users/alexandre.fonseca/Desktop/samples/sem_filtro_tempo/DBSIM_FILTRADO.csv")

#### Adding BD origin

##### AIH #####
df.AIH <- mutate(df.AIH, DB_ORIGEM = 'AIH')

##### APAC ####
df.APAC <- mutate(df.APAC, DB_ORIGEM = 'APAC')

##### BPAI ####
df.BPAI <- mutate(df.BPAI, DB_ORIGEM = 'BPAI')

#### SIM #####
df.SIM <- mutate(df.SIM, DB_ORIGEM = 'SIM')

#### SINAN #####
SINAN <- mutate(SINAN, DB_ORIGEM = 'SINAN')

#### SINAN - PREPARING SINAN TABLE FROM PRE-FIXED DB
SINAN <- read_csv("/Volumes/Untitled/df_SINAN_fixvar.csv")

SINAN$ID_PACIENTE <- as.character(SINAN$ID_PACIENTE)
SINAN$PROCEDIMENTO <- as.character(SINAN$PROCEDIMENTO)
SINAN$MUN_RES <- as.character(SINAN$MUN_RES)
SINAN$MUN_OCOR <- as.character(SINAN$MUN_OCOR)

SINAN$DT_NASC <- ymd(SINAN$DT_NASC)

SINAN$DT_OCOR <- ymd(SINAN$DT_OCOR)

##### Create column HEPATITE
SINAN$HEPATITE <- "I"

##### Column "IDADE"
YEAR1 <- data_frame(year  = ymd(SINAN$DT_NASC))
YEAR2 <- data_frame(year = ymd(SINAN$DT_OCOR))

IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

SEXO <- SINAN$SEXO

SEXO <- data.table(SEXO)

IDADE <- data.table(IDADE)

SEXO_IDADE_SINAN <- cbind.data.frame(SEXO, IDADE)

#SEXO_IDADE$SEXO <- as.character(SEXO_IDADE$SEXO)
#SEXO_IDADE$IDADE <- as.numeric(SEXO_IDADE$IDADE)


SEXO_IDADE_SINAN$IDADE <- gsub("[S]", "", SEXO_IDADE_SINAN$IDADE)

SINAN <- cbind.data.frame(SINAN ,IDADE)
SINAN$IDADE <- gsub("[S]", "", SINAN$IDADE)

#### Columns UF_RES and UF_OCOR #### SINAN 

UF_RES <- substr(SINAN$MUN_RES, 0, 2)
UF_RES <- as.data.frame(UF_RES)


UF_OCOR <- substr(SINAN$MUN_OCOR, 0, 2)
UF_OCOR <- as.data.frame(UF_OCOR)

SINAN <- cbind.data.frame(SINAN, UF_RES)

SINAN <- cbind.data.frame(SINAN, UF_OCOR)

SINAN$UF_RES <- as.character(SINAN$UF_RES)
SINAN$UF_OCOR <- as.character(SINAN$UF_OCOR)

SINAN$UF_RES[SINAN$UF_RES == "11"] <- "RO"
SINAN$UF_RES[SINAN$UF_RES == "12"] <- "AC"
SINAN$UF_RES[SINAN$UF_RES == "13"] <- "AM"
SINAN$UF_RES[SINAN$UF_RES == "14"] <- "RR"
SINAN$UF_RES[SINAN$UF_RES == "15"] <- "PA"
SINAN$UF_RES[SINAN$UF_RES == "16"] <- "AP"
SINAN$UF_RES[SINAN$UF_RES == "17"] <- "TO"
SINAN$UF_RES[SINAN$UF_RES == "21"] <- "MA"
SINAN$UF_RES[SINAN$UF_RES == "22"] <- "PI"
SINAN$UF_RES[SINAN$UF_RES == "23"] <- "CE"
SINAN$UF_RES[SINAN$UF_RES == "24"] <- "RN"
SINAN$UF_RES[SINAN$UF_RES == "25"] <- "PB"
SINAN$UF_RES[SINAN$UF_RES == "26"] <- "PE"
SINAN$UF_RES[SINAN$UF_RES == "27"] <- "AL"
SINAN$UF_RES[SINAN$UF_RES == "28"] <- "SE"
SINAN$UF_RES[SINAN$UF_RES == "29"] <- "BA"
SINAN$UF_RES[SINAN$UF_RES == "31"] <- "MG"
SINAN$UF_RES[SINAN$UF_RES == "32"] <- "ES"
SINAN$UF_RES[SINAN$UF_RES == "33"] <- "RJ"
SINAN$UF_RES[SINAN$UF_RES == "35"] <- "SP"
SINAN$UF_RES[SINAN$UF_RES == "41"] <- "PR"
SINAN$UF_RES[SINAN$UF_RES == "42"] <- "SC"
SINAN$UF_RES[SINAN$UF_RES == "43"] <- "RS"
SINAN$UF_RES[SINAN$UF_RES == "50"] <- "MS"
SINAN$UF_RES[SINAN$UF_RES == "51"] <- "MT"
SINAN$UF_RES[SINAN$UF_RES == "52"] <- "GO"
SINAN$UF_RES[SINAN$UF_RES == "53"] <- "DF"

SINAN$UF_OCOR[SINAN$UF_OCOR == "11"] <- "RO"
SINAN$UF_OCOR[SINAN$UF_OCOR == "12"] <- "AC"
SINAN$UF_OCOR[SINAN$UF_OCOR == "13"] <- "AM"
SINAN$UF_OCOR[SINAN$UF_OCOR == "14"] <- "RR"
SINAN$UF_OCOR[SINAN$UF_OCOR == "15"] <- "PA"
SINAN$UF_OCOR[SINAN$UF_OCOR == "16"] <- "AP"
SINAN$UF_OCOR[SINAN$UF_OCOR == "17"] <- "TO"
SINAN$UF_OCOR[SINAN$UF_OCOR == "21"] <- "MA"
SINAN$UF_OCOR[SINAN$UF_OCOR == "22"] <- "PI"
SINAN$UF_OCOR[SINAN$UF_OCOR == "23"] <- "CE"
SINAN$UF_OCOR[SINAN$UF_OCOR == "24"] <- "RN"
SINAN$UF_OCOR[SINAN$UF_OCOR == "25"] <- "PB"
SINAN$UF_OCOR[SINAN$UF_OCOR == "26"] <- "PE"
SINAN$UF_OCOR[SINAN$UF_OCOR == "27"] <- "AL"
SINAN$UF_OCOR[SINAN$UF_OCOR == "28"] <- "SE"
SINAN$UF_OCOR[SINAN$UF_OCOR == "29"] <- "BA"
SINAN$UF_OCOR[SINAN$UF_OCOR == "31"] <- "MG"
SINAN$UF_OCOR[SINAN$UF_OCOR == "32"] <- "ES"
SINAN$UF_OCOR[SINAN$UF_OCOR == "33"] <- "RJ"
SINAN$UF_OCOR[SINAN$UF_OCOR == "35"] <- "SP"
SINAN$UF_OCOR[SINAN$UF_OCOR == "41"] <- "PR"
SINAN$UF_OCOR[SINAN$UF_OCOR == "42"] <- "SC"
SINAN$UF_OCOR[SINAN$UF_OCOR == "43"] <- "RS"
SINAN$UF_OCOR[SINAN$UF_OCOR == "50"] <- "MS"
SINAN$UF_OCOR[SINAN$UF_OCOR == "51"] <- "MT"
SINAN$UF_OCOR[SINAN$UF_OCOR == "52"] <- "GO"
SINAN$UF_OCOR[SINAN$UF_OCOR == "53"] <- "DF"


#### Dealing with NA's ###### AIH #######
## Changing Blank and "*" to "NA"  
#df.AIH[] <- lapply(df.AIH, str_trim) 
is.na(df.AIH) <- df.AIH==''  
is.na(df.AIH) <- df.AIH=='*' 
is.na(df.AIH) <- df.AIH=='//'

#### Dealing with NA's ###### APAC #######
## Changing Blank and "*" to "NA"  
#df.AIH[] <- lapply(df.AIH, str_trim) 
is.na(df.APAC) <- df.APAC==''  
is.na(df.APAC) <- df.APAC=='*' 
is.na(df.APAC) <- df.APAC=='//'

#### Dealing with NA's ###### BPAI #######
## Changing Blank and "*" to "NA"  
#df.AIH[] <- lapply(df.AIH, str_trim) 
is.na(df.BPAI) <- df.BPAI==''  
is.na(df.BPAI) <- df.BPAI=='*' 
is.na(df.BPAI) <- df.BPAI=='//'

#### Dealing with NA's ###### SIM #######
## Changing Blank and "*" to "NA"  
#df.AIH[] <- lapply(df.AIH, str_trim) 
is.na(df.SIM) <- df.SIM==''  
is.na(df.SIM) <- df.SIM=='*' 
is.na(df.SIM) <- df.SIM=='//'

##### Checking DB

str(df.AIH)

str(df.APAC)

str(df.BPAI)

str(df.SIM)

##### Preprocessing df #### AIH

df.AIH$CO_CID_PRINCIPAL <- as.character(df.AIH$CO_CID_PRINCIPAL)
df.AIH$CO_CID_SECUNDARIO_1 <- as.character(df.AIH$CO_CID_SECUNDARIO_1)
df.AIH$CO_CID_SECUNDARIO_2 <- as.character(df.AIH$CO_CID_SECUNDARIO_2)
df.AIH$CO_CID_SECUNDARIO_3 <- as.character(df.AIH$CO_CID_SECUNDARIO_3)
df.AIH$CO_CID_SECUNDARIO_4 <- as.character(df.AIH$CO_CID_SECUNDARIO_4)
df.AIH$CO_CID_SECUNDARIO_5 <- as.character(df.AIH$CO_CID_SECUNDARIO_5)
df.AIH$CO_CID_SECUNDARIO_6 <- as.character(df.AIH$CO_CID_SECUNDARIO_6)
df.AIH$CO_CID_SECUNDARIO_7 <- as.character(df.AIH$CO_CID_SECUNDARIO_7)
df.AIH$CO_CID_SECUNDARIO_8 <- as.character(df.AIH$CO_CID_SECUNDARIO_8)
df.AIH$CO_CID_SECUNDARIO_9 <- as.character(df.AIH$CO_CID_SECUNDARIO_9)
df.AIH$CO_CID_OBITO <- as.character(df.AIH$CO_CID_OBITO)

df.AIH$ID_PACIENTE <- as.character(df.AIH$ID_PACIENTE) 
df.AIH$CO_PROCEDIMENTO_PRINCIPAL <- as.character(df.AIH$CO_PROCEDIMENTO_PRINCIPAL) 
df.AIH$CO_PACIENTE_SEXO <- as.character(df.AIH$CO_PACIENTE_SEXO) 
df.AIH$ID_PACIENTE <- as.character(df.AIH$ID_PACIENTE) 

df.AIH$DT_INTERNACAO <- dmy(df.AIH$DT_INTERNACAO)
df.AIH$DT_PACIENTE_NASCIMENTO <- dmy(df.AIH$DT_PACIENTE_NASCIMENTO)

##### Preprocessing df #### APAC

df.APAC$CO_CID_PRINCIPAL <- as.character(df.APAC$CO_CID_PRINCIPAL)
df.APAC$CO_CID_SECUNDARIO <- as.character(df.APAC$CO_CID_SECUNDARIO)

df.APAC$ID_PACIENTE <- as.character(df.APAC$ID_PACIENTE) 

df.APAC$CO_PROCEDIMENTO_PRINCIPAL <- as.character(df.APAC$CO_PROCEDIMENTO_PRINCIPAL) 
df.APAC$CO_PROCEDIMENTO_SECUNDARIO <- as.character(df.APAC$CO_PROCEDIMENTO_SECUNDARIO) 

df.APAC$SEXO <- as.character(df.APAC$SEXO) 

df.APAC$DT_NASCIMENTO <- dmy(df.APAC$DT_NASCIMENTO)
df.APAC$DATA_INICIO <- dmy(df.APAC$DATA_INICIO)


##### Preprocessing df #### BPAI

df.BPAI$CO_CID_PRINCIPAL <- as.character(df.BPAI$CO_CID_PRINCIPAL)

df.BPAI$ID_PACIENTE <- as.character(df.BPAI$ID_PACIENTE) 

df.BPAI$MUNICIPIO_PACIENTE <- as.character(df.BPAI$MUNICIPIO_PACIENTE) 
df.BPAI$CO_MUNICIPIO <- as.character(df.BPAI$CO_MUNICIPIO) 

df.BPAI$CO_PROCEDIMENTO_REALIZADO <- as.character(df.BPAI$CO_PROCEDIMENTO_REALIZADO) 

df.BPAI$SEXO <- as.character(df.BPAI$SEXO) 

df.BPAI$DT_NASCIMENTO <- ymd(df.BPAI$DT_NASCIMENTO)

df.BPAI$DT_ATENDIMENTO <- ymd(df.BPAI$DT_ATENDIMENTO)

##### Preprocessing df #### SIM

df.SIM$ID_PACIENTE <- as.character(df.SIM$ID_PACIENTE)

df.SIM$ATESTADO <- as.character(df.SIM$ATESTADO)
df.SIM$TIPOBITOE <- as.character(df.SIM$TIPOBITO)
df.SIM$SEXO <- as.character(df.SIM$SEXO)
df.SIM$CAUSABAS <- as.character(df.SIM$CAUSABAS)
df.SIM$LINHAA <- as.character(df.SIM$LINHAA)
df.SIM$LINHAB<- as.character(df.SIM$LINHAB)
df.SIM$LINHAC<- as.character(df.SIM$LINHAC)
df.SIM$LINHAD <- as.character(df.SIM$LINHAD)
df.SIM$LINHAII <- as.character(df.SIM$LINHAII)

df.SIM$DTNASC <- dmy(df.SIM$DTNASC)

df.SIM$DTOBITO <- dmy(df.SIM$DTOBITO)

##### Checking DB

str(df.AIH)

str(df.APAC)

str(df.BPAI)

str(df.SIM)

#### Filter 1 -  ICD (CID) or medical procedures (Procedimentos) 
#### oRGANAZING COLUMNS AND VARIABLES NAMES IN THE TABLE

########## AIH ########

########## PR ##########
AIH_PR <- select(df.AIH,ID_PACIENTE, DT_CMPT, DT_NASC = DT_PACIENTE_NASCIMENTO, -VL_PACIENTE_IDADE, -VL_VALOR, QT_PROCEDIMENTO, -QT_DIARIAS, QT_DIARIAS_UI, -QT_DIARIAS_UTI, SEXO = CO_PACIENTE_SEXO, -RACA, MUN_RES = NU_PACIENTE_LOGR_MUNICIPIO, MUN_OCOR = NU_MUN_HOSP, CO_CID_OBITO, DT_OCOR = DT_INTERNACAO, -DT_SAIDA, CO_CID_PRINCIPAL, CO_CID_SECUNDARIO_1, CO_CID_SECUNDARIO_2, CO_CID_SECUNDARIO_3, CO_CID_SECUNDARIO_4, CO_CID_SECUNDARIO_5, CO_CID_SECUNDARIO_6, CO_CID_SECUNDARIO_7, CO_CID_SECUNDARIO_8, CO_CID_SECUNDARIO_9, CO_CID_OBITO, CO_PROC_SOLICITADO, CO_PROCEDIMENTO_PRINCIPAL, COD_PROCEDIMENTO_SECUNDARIO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='B170' | .=='B171' | .=='B18' | .=='B180' | .=='B181' | .=='B182' | .=='B188' | .=='B189' | .=='B19' | .=='B190' | .=='B199' | .=='B942' | .=='O984' | .=='P353' | .=='Z225' | .=='B15' | .=='B150' | .=='B159' | .=='B160' | .=='0202030059' | .=='0202030210' | .=='0202031080' | .=='0213010542' | .=='0303010118' | .=='0604760019' | .=='0604760027' | .=='0604760035' | .=='0604640030' | .=='0604760043' | .=='0604390041' | .=='0604390050' | .=='0604390068' | .=='0604390076' | .=='0604450010' | .=='0604390017' | .=='0604460023' | .=='0604460058' | .=='0604460040' | .=='0604390025' | .=='0604390033')) 
                                      #DT_CMPT     

########## PO ##########
AIH_PO <-  select(df.AIH,ID_PACIENTE, DT_CMPT, DT_NASC = DT_PACIENTE_NASCIMENTO, -VL_PACIENTE_IDADE, -VL_VALOR, QT_PROCEDIMENTO, -QT_DIARIAS, QT_DIARIAS_UI, -QT_DIARIAS_UTI, SEXO = CO_PACIENTE_SEXO, -RACA, MUN_RES = NU_PACIENTE_LOGR_MUNICIPIO, MUN_OCOR = NU_MUN_HOSP, CO_CID_OBITO, DT_OCOR = DT_INTERNACAO, -DT_SAIDA, CO_CID_PRINCIPAL, CO_CID_SECUNDARIO_1, CO_CID_SECUNDARIO_2, CO_CID_SECUNDARIO_3, CO_CID_SECUNDARIO_4, CO_CID_SECUNDARIO_5, CO_CID_SECUNDARIO_6, CO_CID_SECUNDARIO_7, CO_CID_SECUNDARIO_8, CO_CID_SECUNDARIO_9, CO_CID_OBITO, CO_PROC_SOLICITADO, CO_PROCEDIMENTO_PRINCIPAL, COD_PROCEDIMENTO_SECUNDARIO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='K73' | .=='K730' | .=='K731' | .=='K732' | .=='K738' | .=='K739' | .=='K752' | .=='Z205' | .=='Z944' | .=='C220' | .=='C221' | .=='C222' | .=='C223' | .=='C224' | .=='C227' | .=='C229' | .=='R18' | .=='K74' | .=='K743' | .=='K744' | .=='K745' | .=='K746' | .=='0501070052' | .=='0505020050' | .=='0505020068' | .=='0506020096' | .=='0702050580' | .=='0202030644' | .=='0202030679' | .=='0202030784' | .=='0202030890' | .=='0202030970' | .=='0202030989' | .=='0214010090)' | .=='0213010208' | .=='0305010115' | .=='0305010123' | .=='0601210018' | .=='0601210026' | .=='0601210034' | .=='0604300018' | .=='0604300026' | .=='0604300034' | .=='0213010356' | .=='0213010135' | .=='0213010143' | .=='0213010178')) 
                                      #DT_CMPT

########## APAC ########

########## PR ##########
APAC_PR <- select(df.APAC, ID_PACIENTE, -COMPETENCIA, DT_NASC = DT_NASCIMENTO, SEXO , MUN_RES = CO_MUNICIPIO_ENDERECO, MUN_OCOR = CO_MUNICIPIO_HOSPITAL, DT_OCOR = DATA_INICIO, -DATA_FIM, CO_CID_PRINCIPAL, CO_CID_SECUNDARIO, CO_PROCEDIMENTO_PRINCIPAL, CO_PROCEDIMENTO_SECUNDARIO, -DATA_SOLIC, -DATA_GERACAO, -VALOR, -QT_PROCEDIMENTO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='B170' | .=='B171' | .=='B18' | .=='B180' | .=='B181' | .=='B182' | .=='B188' | .=='B189' | .=='B19' | .=='B190' | .=='B199' | .=='B942' | .=='O984' | .=='P353' | .=='Z225' | .=='B15' | .=='B150' | .=='B159' | .=='B160' | .=='0202030059' | .=='0202030210' | .=='0202031080' | .=='0213010542' | .=='0303010118' | .=='0604760019' | .=='0604760027' | .=='0604760035' | .=='0604640030' | .=='0604760043' | .=='0604390041' | .=='0604390050' | .=='0604390068' | .=='0604390076' | .=='0604450010' | .=='0604390017' | .=='0604460023' | .=='0604460058' | .=='0604460040' | .=='0604390025' | .=='0604390033')) 
                                        #COMPETENCIA
########## PO ##########
APAC_PO <- select(df.APAC, ID_PACIENTE, -COMPETENCIA, DT_NASC = DT_NASCIMENTO, SEXO , MUN_RES = CO_MUNICIPIO_ENDERECO, MUN_OCOR = CO_MUNICIPIO_HOSPITAL, DT_OCOR = DATA_INICIO, -DATA_FIM, CO_CID_PRINCIPAL, CO_CID_SECUNDARIO, CO_PROCEDIMENTO_PRINCIPAL, CO_PROCEDIMENTO_SECUNDARIO, -DATA_SOLIC, -DATA_GERACAO, -VALOR, -QT_PROCEDIMENTO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='K73' | .=='K730' | .=='K731' | .=='K732' | .=='K738' | .=='K739' | .=='K752' | .=='Z205' | .=='Z944' | .=='C220' | .=='C221' | .=='C222' | .=='C223' | .=='C224' | .=='C227' | .=='C229' | .=='R18' | .=='K74' | .=='K743' | .=='K744' | .=='K745' | .=='K746' | .=='0501070052' | .=='0505020050' | .=='0505020068' | .=='0506020096' | .=='0702050580' | .=='0202030644' | .=='0202030679' | .=='0202030784' | .=='0202030890' | .=='0202030970' | .=='0202030989' | .=='0214010090)' | .=='0213010208' | .=='0305010115' | .=='0305010123' | .=='0601210018' | .=='0601210026' | .=='0601210034' | .=='0604300018' | .=='0604300026' | .=='0604300034' | .=='0213010356' | .=='0213010135' | .=='0213010143' | .=='0213010178')) 
                                        #COMPETENCIA
######## Change gender label ### APAC_PR
APAC_PR$SEXO[APAC_PR$SEXO == "MASCULINO"] <- "M"

APAC_PR$SEXO[APAC_PR$SEXO == "FEMININO"] <- "F"

######## Change gender label ### APAC_PO
APAC_PO$SEXO <- as.character(APAC_PO$SEXO)

APAC_PO$SEXO[APAC_PO$SEXO == "MASCULINO"] <- "M"

APAC_PO$SEXO[APAC_PO$SEXO == "FEMININO"] <- "F"

########## BPAI ########

########## PR ##########
BPAI_PR <- select(df.BPAI, ID_PACIENTE, -COMPETENCIA, DT_NASC = DT_NASCIMENTO, SEXO , MUN_RES = MUNICIPIO_PACIENTE, MUN_OCOR = CO_MUNICIPIO, DT_OCOR = DT_ATENDIMENTO, CO_CID_PRINCIPAL, CO_PROCEDIMENTO_REALIZADO, -VALOR, -QT_REALIZADO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='B170' | .=='B171' | .=='B18' | .=='B180' | .=='B181' | .=='B182' | .=='B188' | .=='B189' | .=='B19' | .=='B190' | .=='B199' | .=='B942' | .=='O984' | .=='P353' | .=='Z225' | .=='B15' | .=='B150' | .=='B159' | .=='B160' | .=='0202030059' | .=='0202030210' | .=='0202031080' | .=='0213010542' | .=='0303010118' | .=='0604760019' | .=='0604760027' | .=='0604760035' | .=='0604640030' | .=='0604760043' | .=='0604390041' | .=='0604390050' | .=='0604390068' | .=='0604390076' | .=='0604450010' | .=='0604390017' | .=='0604460023' | .=='0604460058' | .=='0604460040' | .=='0604390025' | .=='0604390033')) 
                                          #COMPETENCIA
########## PO ##########
BPAI_PO <- select(df.BPAI, ID_PACIENTE, -COMPETENCIA, DT_NASC = DT_NASCIMENTO, SEXO , MUN_RES = MUNICIPIO_PACIENTE, MUN_OCOR = CO_MUNICIPIO, DT_OCOR = DT_ATENDIMENTO, CO_CID_PRINCIPAL, CO_PROCEDIMENTO_REALIZADO, -VALOR, -QT_REALIZADO, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='K73' | .=='K730' | .=='K731' | .=='K732' | .=='K738' | .=='K739' | .=='K752' | .=='Z205' | .=='Z944' | .=='C220' | .=='C221' | .=='C222' | .=='C223' | .=='C224' | .=='C227' | .=='C229' | .=='R18' | .=='K74' | .=='K743' | .=='K744' | .=='K745' | .=='K746' | .=='0501070052' | .=='0505020050' | .=='0505020068' | .=='0506020096' | .=='0702050580' | .=='0202030644' | .=='0202030679' | .=='0202030784' | .=='0202030890' | .=='0202030970' | .=='0202030989' | .=='0214010090)' | .=='0213010208' | .=='0305010115' | .=='0305010123' | .=='0601210018' | .=='0601210026' | .=='0601210034' | .=='0604300018' | .=='0604300026' | .=='0604300034' | .=='0213010356' | .=='0213010135' | .=='0213010143' | .=='0213010178')) 
                                       #COMPETENCIA

########## SIM ########

########## PR ##########   
SIM_PR <- select(df.SIM, ID_PACIENTE ,  DT_NASC = DTNASC, SEXO , MUN_RES = CODMUNRES, MUN_OCOR = CODMUNOCOR, DT_OCOR = DTOBITO, ATESTADO, -TIPOBITO, CAUSABAS, LINHAA, LINHAB, LINHAC,LINHAD,LINHAII, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='B170' | .=='B171' | .=='B18' | .=='B180' | .=='B181' | .=='B182' | .=='B188' | .=='B189' | .=='B19' | .=='B190' | .=='B199' | .=='B942' | .=='O984' | .=='P353' | .=='Z225' | .=='B15' | .=='B150' | .=='B159' | .=='B160' | .=='0202030059' | .=='0202030210' | .=='0202031080' | .=='0213010542' | .=='0303010118' | .=='0604760019' | .=='0604760027' | .=='0604760035' | .=='0604640030' | .=='0604760043' | .=='0604390041' | .=='0604390050' | .=='0604390068' | .=='0604390076' | .=='0604450010' | .=='0604390017' | .=='0604460023' | .=='0604460058' | .=='0604460040' | .=='0604390025' | .=='0604390033')) 
                        #ID_PACIENTE
########## PO ##########
SIM_PO <- select(df.SIM, ID_PACIENTE  , DT_NASC = DTNASC, SEXO , MUN_RES = CODMUNRES, MUN_OCOR = CODMUNOCOR, DT_OCOR = DTOBITO, ATESTADO, -TIPOBITO, CAUSABAS, LINHAA, LINHAB, LINHAC,LINHAD,LINHAII, DB_ORIGEM) %>% filter_if(is.character, any_vars(.=='K73' | .=='K730' | .=='K731' | .=='K732' | .=='K738' | .=='K739' | .=='K752' | .=='Z205' | .=='Z944' | .=='C220' | .=='C221' | .=='C222' | .=='C223' | .=='C224' | .=='C227' | .=='C229' | .=='R18' | .=='K74' | .=='K743' | .=='K744' | .=='K745' | .=='K746' | .=='0501070052' | .=='0505020050' | .=='0505020068' | .=='0506020096' | .=='0702050580' | .=='0202030644' | .=='0202030679' | .=='0202030784' | .=='0202030890' | .=='0202030970' | .=='0202030989' | .=='0214010090)' | .=='0213010208' | .=='0305010115' | .=='0305010123' | .=='0601210018' | .=='0601210026' | .=='0601210034' | .=='0604300018' | .=='0604300026' | .=='0604300034' | .=='0213010356' | .=='0213010135' | .=='0213010143' | .=='0213010178')) 
                        #ID_PACIENTE
### Change gender label ### SIM_PR

SIM_PR$SEXO[SIM_PR$SEXO == "1"] <- "M"

SIM_PR$SEXO[SIM_PR$SEXO == "2"] <- "F"

### Change gender label ### SIM_PO

SIM_PO$SEXO[SIM_PO$SEXO == "1"] <- "M"

SIM_PO$SEXO[SIM_PO$SEXO == "2"] <- "F"

##### Checking DB

str(AIH_PR)

str(APAC_PR)

str(BPAI_PR)

str(SIM_PR)


#####################################
#### Classificarion - Hepatitis types
#####################################

####### Preparing vectors in each DB

#### AIH_PR

procedimento <-  AIH_PR$CO_CID_PRINCIPAL
procedimento.2 <- AIH_PR$COD_PROCEDIMENTO_SECUNDARIO

cid <- AIH_PR$CO_CID_PRINCIPAL
cid.1 <-AIH_PR$CO_CID_SECUNDARIO_1
cid.2 <-AIH_PR$CO_CID_SECUNDARIO_2
cid.3 <-AIH_PR$CO_CID_SECUNDARIO_3
cid.4 <-AIH_PR$CO_CID_SECUNDARIO_4
cid.5 <- AIH_PR$CO_CID_SECUNDARIO_5
cid.6 <- AIH_PR$CO_CID_SECUNDARIO_6
cid.7 <- AIH_PR$CO_CID_SECUNDARIO_7
cid.8 <- AIH_PR$CO_CID_SECUNDARIO_8
cid.9 <- AIH_PR$CO_CID_SECUNDARIO_9
cid.x <- as.character(AIH_PR$CO_CID_OBITO)

#### AIH_PO

procedimento <-  AIH_PO$CO_CID_PRINCIPAL
procedimento.2 <- AIH_PO$COD_PROCEDIMENTO_SECUNDARIO

cid <- AIH_PO$CO_CID_PRINCIPAL
cid.1 <-AIH_PO$CO_CID_SECUNDARIO_1
cid.2 <-AIH_PO$CO_CID_SECUNDARIO_2
cid.3 <-AIH_PO$CO_CID_SECUNDARIO_3
cid.4 <-AIH_PO$CO_CID_SECUNDARIO_4
cid.5 <- AIH_PO$CO_CID_SECUNDARIO_5
cid.6 <- AIH_PO$CO_CID_SECUNDARIO_6
cid.7 <- AIH_PO$CO_CID_SECUNDARIO_7
cid.8 <- AIH_PO$CO_CID_SECUNDARIO_8
cid.9 <- AIH_PO$CO_CID_SECUNDARIO_9
cid.x <- as.character(AIH_PO$CO_CID_OBITO)

### APAC_PR

cid.APAC1 <- APAC_PR$CO_CID_PRINCIPAL
cid.APAC2 <- APAC_PR$CO_CID_SECUNDARIO

procedimento.APAC1 <- APAC_PR$CO_PROCEDIMENTO_PRINCIPAL
procedimento.APAC2 <- APAC_PR$CO_PROCEDIMENTO_PRINCIPAL


### APAC_PO

cid.APAC1 <- APAC_PO$CO_CID_PRINCIPAL
cid.APAC2 <- APAC_PO$CO_CID_SECUNDARIO

procedimento.APAC1 <- APAC_PO$CO_PROCEDIMENTO_PRINCIPAL
procedimento.APAC2 <- APAC_PO$CO_PROCEDIMENTO_PRINCIPAL

### BPAI_PR

cid.BPAI <- BPAI_PR$CO_CID_PRINCIPAL

procedimento.BPAI <- BPAI_PR$CO_PROCEDIMENTO_REALIZADO

### BPAI_PO

cid.BPAI <- BPAI_PO$CO_CID_PRINCIPAL

procedimento.BPAI <- BPAI_PO$CO_PROCEDIMENTO_REALIZADO

### SIM_PR

cid.SIM1  <- SIM_PR$ATESTADO
cid.SIM2 <- SIM_PR$CAUSABAS
cid.SIM3 <- SIM_PR$LINHAA
cid.SIM4 <- SIM_PR$LINHAB
cid.SIM5 <- SIM_PR$LINHAC
cid.SIM6 <- SIM_PR$LINHAD
cid.SIM7 <- SIM_PR$LINHAII

### SIM_PO

cid.SIM1  <- SIM_PO$ATESTADO
cid.SIM2 <- SIM_PO$CAUSABAS
cid.SIM3 <- SIM_PO$LINHAA
cid.SIM4 <- SIM_PO$LINHAB
cid.SIM5 <- SIM_PO$LINHAC
cid.SIM6 <- SIM_PO$LINHAD
cid.SIM7 <- SIM_PO$LINHAII

####### CLassification of HEP types #################

###### AIH_PR ####

HEPC.AIH <- mutate(AIH_PR, HEPC = ifelse (cid == "B171" | cid.1  == "B171" | cid.2  == "B171" | cid.3  == "B171" | cid.4  == "B171" | cid.5  == "B171" | cid.6  == "B171" | cid.7  == "B171" | cid.8  == "B171"| cid.9  == "B171" | cid.x  == "B171"  | cid == "B182" | cid.1  == "B182" | cid.2  == "B182" | cid.3  == "B182" | cid.4  == "B182" | cid.5  == "B182" | cid.6  == "B182" | cid.7  == "B182" | cid.8  == "B182" | cid.8  == "B182"| cid.9  == "B182" | cid.x  == "B182" | procedimento== "0202030059" | procedimento== "0202030210" | procedimento== "0202031080" | procedimento== "0213010542" | procedimento== "0604760027" | procedimento== "0604760035" | procedimento== "0604640030" | procedimento== "0604760043" | procedimento== "0604390041" | procedimento== "0604390017" | procedimento== "0604460023" | procedimento== "0604460058" | procedimento== "0604460040" | procedimento== "0604390050"  | procedimento== "0604390068" | procedimento== "0604390025" | procedimento== "0604390033"  | procedimento== "0604390076" | procedimento== "0604450010"| procedimento== "0303010118" | procedimento.2== "0202030059" | procedimento.2== "0202030210" | procedimento.2== "0202031080" | procedimento.2== "0213010542" | procedimento.2== "0604760027" | procedimento.2== "0604760035" | procedimento.2== "0604640030" | procedimento.2== "0604760043" | procedimento.2== "0604390041" | procedimento.2== "0604390017" | procedimento.2== "0604460023" | procedimento.2== "0604460058" | procedimento.2== "0604460040" | procedimento.2== "0604390050"  | procedimento.2== "0604390068" | procedimento.2== "0604390025" | procedimento.2== "0604390033"  | procedimento.2== "0604390076" | procedimento.2== "0604450010" | procedimento.2== "0303010118" | procedimento.2== "0604760019", "C" , "I")) 
HEPB.AiH <- mutate(AIH_PR, HEPB = ifelse (cid == "B170" | cid.1  == "B170" | cid.2  == "B170" | cid.3  == "B170" | cid.4  == "B170" | cid.5  == "B170" | cid.6  == "B170" | cid.7  == "B170"| cid.8  == "B170"| cid.9  == "B170" | cid.x  == "B170" | procedimento== "0604390041" | procedimento== "0604390050" | procedimento== "0604390068" | procedimento== "0604390076" | procedimento== "0604390017" | procedimento== "0604460023" | procedimento== "0604460058" | procedimento== "0604460040" | procedimento== "0604390025" | procedimento== "0604390033" | procedimento== "0604450010", "B" , "I"))
HEPA.AIH <- mutate(AIH_PR, HEPA = ifelse (cid == "B15" | cid.1  == "B15" | cid.2  == "B15" | cid.3  == "B15" | cid.4  == "B15" | cid.5  == "B15" | cid.6  == "B15" | cid.7  == "B15" | cid.8  == "B15"| cid.9  == "B15" | cid.x  == "B15"  | cid == "B150" | cid.1  == "B150" | cid.2  == "B150" | cid.3  == "B150" | cid.4  == "B150" | cid.5  == "B150" | cid.6  == "B150" | cid.7  == "B150" | cid.8  == "B150" | cid.8  == "B150"| cid.9  == "B150" | cid.x  == "B150" | cid == "B159" | cid.1  == "B159" | cid.2  == "B159" | cid.3  == "B159" | cid.4  == "B159" | cid.5  == "B159" | cid.6  == "B159" | cid.7  == "B159" | cid.8  == "B159" | cid.8  == "B159"| cid.9  == "B159" | cid.x  == "B159", "A" , "I")) 
HEPD.AIH <- mutate(AIH_PR, HEPD = ifelse (cid == "B180" | cid.1  == "B180" | cid.2  == "B180" | cid.3  == "B180" | cid.4  == "B180" | cid.5  == "B180" | cid.6  == "B180" | cid.7  == "B180" | cid.8  == "B180"| cid.9  == "B180" | cid.x  == "B180"  | cid == "B181" | cid.1  == "B181" | cid.2  == "B181" | cid.3  == "B181" | cid.4  == "B181" | cid.5  == "B181" | cid.6  == "B181" | cid.7  == "B181" | cid.8  == "B181" | cid.9  == "B181"| cid.x  == "B181" | cid  == "B160" | cid.1  == "B160" | cid.2  == "B160" | cid.3  == "B160" | cid.4  == "B160" | cid.5  == "B160" | cid.6  == "B160" | cid.7  == "B160" | cid.8  == "B160" | cid.9  == "B160" | cid.x  == "B160", "D" , "I")) 
HEPI.AIH <- mutate(AIH_PR, HEPI = ifelse (cid == "B188" | cid.1  == "B188" | cid.2  == "B188" | cid.3  == "B188" | cid.4  == "B188" | cid.5  == "B188" | cid.6  == "B188" | cid.7  == "B188" | cid.8  == "B188" | cid.9  == "B188" | cid.x  == "B188"  | cid == "B189" | cid.1  == "B189" | cid.2  == "B189" | cid.3  == "B189" | cid.4  == "B189" | cid.5  == "B189" | cid.6  == "B189" | cid.7  == "B189" | cid.8  == "B189"| cid.9  == "B189" | cid.x  == "B189" | cid == "B190" | cid.1  == "B190" | cid.2  == "B190" | cid.3  == "B190" | cid.4  == "B190" | cid.5  == "B190" | cid.6  == "B190" | cid.7  == "B190" | cid.8  == "B190" | cid.9  == "B190" | cid.x  == "B190" | cid == "B199" | cid.1  == "B199" | cid.2  == "B199" | cid.3  == "B199" | cid.4  == "B199" | cid.5  == "B199" | cid.6  == "B199" | cid.7  == "B199" | cid.8  == "B199"| cid.9  == "B199" | cid.x  == "B199" | cid == "B942" | cid.1  == "B942" | cid.2  == "B942" | cid.3  == "B942" | cid.4  == "B942" | cid.5  == "B942" | cid.6  == "B942" | cid.7  == "B942" | cid.8  == "B942" | cid.9  == "B942" | cid.x  == "B942" | cid == "O984" | cid.1  == "O984" | cid.2  == "O984" | cid.3  == "O984" | cid.4  == "O984" | cid.5  == "O984" | cid.6  == "O984" | cid.7  == "O984" | cid.8  == "O984" | cid.9  == "O984" | cid.x  == "O984" | cid == "P353" | cid.1  == "P353" | cid.2  == "P353" | cid.3  == "P353" | cid.4  == "P353" | cid.5  == "P353" | cid.6  == "P353" | cid.7  == "P353" | cid.8  == "P353" | cid.9  == "P353" | cid.x  == "P353"  | cid == "Z225" | cid.1  == "Z225" | cid.2  == "Z225" | cid.3  == "Z225" | cid.4  == "Z225" | cid.5  == "Z225" | cid.6  == "Z225" | cid.7  == "Z225" | cid.8  == "Z225"| cid.9  == "Z225" | cid.x  == "Z225" | cid == "B19" | cid.1  == "B19" | cid.2  == "B19" | cid.3  == "B19" | cid.4  == "B19" | cid.5  == "B19" | cid.6  == "B19" | cid.7  == "B19" | cid.8  == "B19" | cid.9  == "B19"| cid.x  == "B19" | procedimento== "0303010118" | procedimento.2== "0303010118", "I" , "")) 

####### cbind ##### AIH ######  put columns from hep classification in the final table for each DB #########

AIH_PR <- cbind.data.frame(AIH_PR, HEPC.AIH$HEPC)
AIH_PR <- cbind.data.frame(AIH_PR, HEPB.AiH$HEPB)
AIH_PR <- cbind.data.frame(AIH_PR, HEPA.AIH$HEPA)
AIH_PR <- cbind.data.frame(AIH_PR, HEPD.AIH$HEPD)
AIH_PR <- cbind.data.frame(AIH_PR, HEPI.AIH$HEPI)

###### AIH_PO ####

HEPC.AIH <- mutate(AIH_PO, HEPC = ifelse (cid == "B171" | cid.1  == "B171" | cid.2  == "B171" | cid.3  == "B171" | cid.4  == "B171" | cid.5  == "B171" | cid.6  == "B171" | cid.7  == "B171" | cid.8  == "B171"| cid.9  == "B171" | cid.x  == "B171"  | cid == "B182" | cid.1  == "B182" | cid.2  == "B182" | cid.3  == "B182" | cid.4  == "B182" | cid.5  == "B182" | cid.6  == "B182" | cid.7  == "B182" | cid.8  == "B182" | cid.8  == "B182"| cid.9  == "B182" | cid.x  == "B182" | procedimento== "0202030059" | procedimento== "0202030210" | procedimento== "0202031080" | procedimento== "0213010542" | procedimento== "0604760027" | procedimento== "0604760035" | procedimento== "0604640030" | procedimento== "0604760043" | procedimento== "0604390041" | procedimento== "0604390017" | procedimento== "0604460023" | procedimento== "0604460058" | procedimento== "0604460040" | procedimento== "0604390050"  | procedimento== "0604390068" | procedimento== "0604390025" | procedimento== "0604390033"  | procedimento== "0604390076" | procedimento== "0604450010"| procedimento== "0303010118" | procedimento.2== "0202030059" | procedimento.2== "0202030210" | procedimento.2== "0202031080" | procedimento.2== "0213010542" | procedimento.2== "0604760027" | procedimento.2== "0604760035" | procedimento.2== "0604640030" | procedimento.2== "0604760043" | procedimento.2== "0604390041" | procedimento.2== "0604390017" | procedimento.2== "0604460023" | procedimento.2== "0604460058" | procedimento.2== "0604460040" | procedimento.2== "0604390050"  | procedimento.2== "0604390068" | procedimento.2== "0604390025" | procedimento.2== "0604390033"  | procedimento.2== "0604390076" | procedimento.2== "0604450010" | procedimento.2== "0303010118" | procedimento.2== "0604760019", "C" , "I")) 
HEPB.AiH <- mutate(AIH_PO, HEPB = ifelse (cid == "B170" | cid.1  == "B170" | cid.2  == "B170" | cid.3  == "B170" | cid.4  == "B170" | cid.5  == "B170" | cid.6  == "B170" | cid.7  == "B170"| cid.8  == "B170"| cid.9  == "B170" | cid.x  == "B170" | procedimento== "0604390041" | procedimento== "0604390050" | procedimento== "0604390068" | procedimento== "0604390076" | procedimento== "0604390017" | procedimento== "0604460023" | procedimento== "0604460058" | procedimento== "0604460040" | procedimento== "0604390025" | procedimento== "0604390033" | procedimento== "0604450010", "B" , "I"))
HEPA.AIH <- mutate(AIH_PO, HEPA = ifelse (cid == "B15" | cid.1  == "B15" | cid.2  == "B15" | cid.3  == "B15" | cid.4  == "B15" | cid.5  == "B15" | cid.6  == "B15" | cid.7  == "B15" | cid.8  == "B15"| cid.9  == "B15" | cid.x  == "B15"  | cid == "B150" | cid.1  == "B150" | cid.2  == "B150" | cid.3  == "B150" | cid.4  == "B150" | cid.5  == "B150" | cid.6  == "B150" | cid.7  == "B150" | cid.8  == "B150" | cid.8  == "B150"| cid.9  == "B150" | cid.x  == "B150" | cid == "B159" | cid.1  == "B159" | cid.2  == "B159" | cid.3  == "B159" | cid.4  == "B159" | cid.5  == "B159" | cid.6  == "B159" | cid.7  == "B159" | cid.8  == "B159" | cid.8  == "B159"| cid.9  == "B159" | cid.x  == "B159", "A" , "I")) 
HEPD.AIH <- mutate(AIH_PO, HEPD = ifelse (cid == "B180" | cid.1  == "B180" | cid.2  == "B180" | cid.3  == "B180" | cid.4  == "B180" | cid.5  == "B180" | cid.6  == "B180" | cid.7  == "B180" | cid.8  == "B180"| cid.9  == "B180" | cid.x  == "B180"  | cid == "B181" | cid.1  == "B181" | cid.2  == "B181" | cid.3  == "B181" | cid.4  == "B181" | cid.5  == "B181" | cid.6  == "B181" | cid.7  == "B181" | cid.8  == "B181" | cid.9  == "B181"| cid.x  == "B181" | cid  == "B160" | cid.1  == "B160" | cid.2  == "B160" | cid.3  == "B160" | cid.4  == "B160" | cid.5  == "B160" | cid.6  == "B160" | cid.7  == "B160" | cid.8  == "B160" | cid.9  == "B160" | cid.x  == "B160", "D" , "I")) 
HEPI.AIH <- mutate(AIH_PO, HEPI = ifelse (cid == "B188" | cid.1  == "B188" | cid.2  == "B188" | cid.3  == "B188" | cid.4  == "B188" | cid.5  == "B188" | cid.6  == "B188" | cid.7  == "B188" | cid.8  == "B188" | cid.9  == "B188" | cid.x  == "B188"  | cid == "B189" | cid.1  == "B189" | cid.2  == "B189" | cid.3  == "B189" | cid.4  == "B189" | cid.5  == "B189" | cid.6  == "B189" | cid.7  == "B189" | cid.8  == "B189"| cid.9  == "B189" | cid.x  == "B189" | cid == "B190" | cid.1  == "B190" | cid.2  == "B190" | cid.3  == "B190" | cid.4  == "B190" | cid.5  == "B190" | cid.6  == "B190" | cid.7  == "B190" | cid.8  == "B190" | cid.9  == "B190" | cid.x  == "B190" | cid == "B199" | cid.1  == "B199" | cid.2  == "B199" | cid.3  == "B199" | cid.4  == "B199" | cid.5  == "B199" | cid.6  == "B199" | cid.7  == "B199" | cid.8  == "B199"| cid.9  == "B199" | cid.x  == "B199" | cid == "B942" | cid.1  == "B942" | cid.2  == "B942" | cid.3  == "B942" | cid.4  == "B942" | cid.5  == "B942" | cid.6  == "B942" | cid.7  == "B942" | cid.8  == "B942" | cid.9  == "B942" | cid.x  == "B942" | cid == "O984" | cid.1  == "O984" | cid.2  == "O984" | cid.3  == "O984" | cid.4  == "O984" | cid.5  == "O984" | cid.6  == "O984" | cid.7  == "O984" | cid.8  == "O984" | cid.9  == "O984" | cid.x  == "O984" | cid == "P353" | cid.1  == "P353" | cid.2  == "P353" | cid.3  == "P353" | cid.4  == "P353" | cid.5  == "P353" | cid.6  == "P353" | cid.7  == "P353" | cid.8  == "P353" | cid.9  == "P353" | cid.x  == "P353"  | cid == "Z225" | cid.1  == "Z225" | cid.2  == "Z225" | cid.3  == "Z225" | cid.4  == "Z225" | cid.5  == "Z225" | cid.6  == "Z225" | cid.7  == "Z225" | cid.8  == "Z225"| cid.9  == "Z225" | cid.x  == "Z225" | cid == "B19" | cid.1  == "B19" | cid.2  == "B19" | cid.3  == "B19" | cid.4  == "B19" | cid.5  == "B19" | cid.6  == "B19" | cid.7  == "B19" | cid.8  == "B19" | cid.9  == "B19"| cid.x  == "B19" | procedimento== "0303010118" | procedimento.2== "0303010118", "I" , "")) 

####### cbind ##### AIH ######  put columns from hep classification in the final table for each DB #########

AIH_PO <- cbind.data.frame(AIH_PO, HEPC.AIH$HEPC)
AIH_PO <- cbind.data.frame(AIH_PO, HEPB.AiH$HEPB)
AIH_PO <- cbind.data.frame(AIH_PO, HEPA.AIH$HEPA)
AIH_PO <- cbind.data.frame(AIH_PO, HEPD.AIH$HEPD)
AIH_PO <- cbind.data.frame(AIH_PO, HEPI.AIH$HEPI)


###### APAC_PR  ####

HEPC.APAC <- mutate(APAC_PR, HEPC = ifelse (cid.APAC1 == "B171" | cid.APAC2  == "B171" | cid.APAC1 == "B182" | cid.APAC2  == "B182" | procedimento.APAC1== "0202030059" | procedimento.APAC1== "0202030210" | procedimento.APAC1== "0202031080" | procedimento.APAC1== "0213010542" | procedimento.APAC1== "0604760027" | procedimento.APAC1== "0604760035" | procedimento.APAC1== "0604640030" | procedimento.APAC1== "0604760043" | procedimento.APAC1== "0604390041" | procedimento.APAC1== "0604390017" | procedimento.APAC1== "0604460023" | procedimento.APAC1== "0604460058" | procedimento.APAC1== "0604460040" | procedimento.APAC1== "0604390050"  | procedimento.APAC1== "0604390068" | procedimento.APAC1== "0604390025" | procedimento.APAC1== "0604390033"  | procedimento.APAC1== "0604390076" | procedimento.APAC1== "0604450010"| procedimento.APAC1== "0303010118" | procedimento.APAC2== "0202030059" | procedimento.APAC2== "0202030210" | procedimento.APAC2== "0202031080" | procedimento.APAC2== "0213010542" | procedimento.APAC2== "0604760027" | procedimento.APAC2== "0604760035" | procedimento.APAC2== "0604640030" | procedimento.APAC2== "0604760043" | procedimento.APAC2== "0604390041" | procedimento.APAC2== "0604390017" | procedimento.APAC2== "0604460023" | procedimento.APAC2== "0604460058" | procedimento.APAC2== "0604460040" | procedimento.APAC2== "0604390050"  | procedimento.APAC2== "0604390068" | procedimento.APAC2== "0604390025" | procedimento.APAC2== "0604390033"  | procedimento.APAC2== "0604390076" | procedimento.APAC2== "0604450010" | procedimento.APAC2== "0303010118" | procedimento.APAC2== "0604760019", "C" , "")) 
HEPB.APAC <- mutate(APAC_PR, HEPB = ifelse (cid.APAC1 == "B170" | cid.APAC2  == "B170" | procedimento.APAC1== "0604390041" | procedimento.APAC1== "0604390050" | procedimento.APAC1== "0604390068" | procedimento.APAC1== "0604390076" | procedimento.APAC1== "0604390017" | procedimento.APAC1== "0604460023" | procedimento.APAC1== "0604460058" | procedimento.APAC1== "0604460040" | procedimento.APAC1== "0604390025" | procedimento.APAC1== "0604390033" | procedimento.APAC1== "0604450010" | procedimento.APAC2== "0604390041" | procedimento.APAC2== "0604390050" | procedimento.APAC2== "0604390068" | procedimento.APAC2== "0604390076" | procedimento.APAC2== "0604390017" | procedimento.APAC2== "0604460023" | procedimento.APAC2== "0604460058" | procedimento.APAC2== "0604460040" | procedimento.APAC2== "0604390025" | procedimento.APAC2== "0604390033" | procedimento.APAC2== "0604450010"   , "B" , ""))
HEPA.APAC <- mutate(APAC_PR, HEPA = ifelse (cid.APAC1 == "B15" | cid.APAC2  == "B15" | cid.APAC1 == "B150" | cid.APAC2  == "B150" | cid.APAC1 == "B159" | cid.APAC2  == "B159", "A" , "")) 
HEPD.APAC <- mutate(APAC_PR, HEPD = ifelse (cid.APAC1 == "B180" | cid.APAC2  == "B180" | cid.APAC1 == "B181" | cid.APAC2  == "B181" | cid.APAC1  == "B160" | cid.APAC2  == "B160" , "D" , "")) 
HEPI.APAC <- mutate(APAC_PR, HEPI = ifelse (cid.APAC1 == "B188" | cid.APAC2  == "B188" | cid.APAC1 == "B189" | cid.APAC2  == "B189" | cid.APAC1 == "B190" | cid.APAC2  == "B190" | cid.APAC1 == "B199" | cid.APAC2  == "B199" | cid.APAC1 == "B942" | cid.APAC2  == "B942" | cid.APAC1 == "O984" | cid.APAC2  == "O984" | cid.APAC1== "P353" | cid.APAC2  == "P353" | cid.APAC1 == "Z225" | cid.APAC2  == "Z225" | cid.APAC1 == "B19" | cid.APAC2  == "B19" | procedimento.APAC1== "0303010118" | procedimento.APAC2== "0303010118", "I" , "")) 


####### cbind ##### APAC ###### put columns from hep classification in the final table for each DB #########

APAC_PR <- cbind.data.frame(APAC_PR, HEPC.APAC$HEPC)
APAC_PR <- cbind.data.frame(APAC_PR, HEPB.APAC$HEPB)
APAC_PR <- cbind.data.frame(APAC_PR, HEPA.APAC$HEPA)
APAC_PR <- cbind.data.frame(APAC_PR, HEPD.APAC$HEPD)
APAC_PR <- cbind.data.frame(APAC_PR, HEPI.APAC$HEPI)


###### APAC_PO  ####

HEPC.APAC <- mutate(APAC_PO, HEPC = ifelse (cid.APAC1 == "B171" | cid.APAC2  == "B171" | cid.APAC1 == "B182" | cid.APAC2  == "B182" | procedimento.APAC1== "0202030059" | procedimento.APAC1== "0202030210" | procedimento.APAC1== "0202031080" | procedimento.APAC1== "0213010542" | procedimento.APAC1== "0604760027" | procedimento.APAC1== "0604760035" | procedimento.APAC1== "0604640030" | procedimento.APAC1== "0604760043" | procedimento.APAC1== "0604390041" | procedimento.APAC1== "0604390017" | procedimento.APAC1== "0604460023" | procedimento.APAC1== "0604460058" | procedimento.APAC1== "0604460040" | procedimento.APAC1== "0604390050"  | procedimento.APAC1== "0604390068" | procedimento.APAC1== "0604390025" | procedimento.APAC1== "0604390033"  | procedimento.APAC1== "0604390076" | procedimento.APAC1== "0604450010"| procedimento.APAC1== "0303010118" | procedimento.APAC2== "0202030059" | procedimento.APAC2== "0202030210" | procedimento.APAC2== "0202031080" | procedimento.APAC2== "0213010542" | procedimento.APAC2== "0604760027" | procedimento.APAC2== "0604760035" | procedimento.APAC2== "0604640030" | procedimento.APAC2== "0604760043" | procedimento.APAC2== "0604390041" | procedimento.APAC2== "0604390017" | procedimento.APAC2== "0604460023" | procedimento.APAC2== "0604460058" | procedimento.APAC2== "0604460040" | procedimento.APAC2== "0604390050"  | procedimento.APAC2== "0604390068" | procedimento.APAC2== "0604390025" | procedimento.APAC2== "0604390033"  | procedimento.APAC2== "0604390076" | procedimento.APAC2== "0604450010" | procedimento.APAC2== "0303010118" | procedimento.APAC2== "0604760019", "C" , "")) 
HEPB.APAC <- mutate(APAC_PO, HEPB = ifelse (cid.APAC1 == "B170" | cid.APAC2  == "B170" | procedimento.APAC1== "0604390041" | procedimento.APAC1== "0604390050" | procedimento.APAC1== "0604390068" | procedimento.APAC1== "0604390076" | procedimento.APAC1== "0604390017" | procedimento.APAC1== "0604460023" | procedimento.APAC1== "0604460058" | procedimento.APAC1== "0604460040" | procedimento.APAC1== "0604390025" | procedimento.APAC1== "0604390033" | procedimento.APAC1== "0604450010" | procedimento.APAC2== "0604390041" | procedimento.APAC2== "0604390050" | procedimento.APAC2== "0604390068" | procedimento.APAC2== "0604390076" | procedimento.APAC2== "0604390017" | procedimento.APAC2== "0604460023" | procedimento.APAC2== "0604460058" | procedimento.APAC2== "0604460040" | procedimento.APAC2== "0604390025" | procedimento.APAC2== "0604390033" | procedimento.APAC2== "0604450010"   , "B" , ""))
HEPA.APAC <- mutate(APAC_PO, HEPA = ifelse (cid.APAC1 == "B15" | cid.APAC2  == "B15" | cid.APAC1 == "B150" | cid.APAC2  == "B150" | cid.APAC1 == "B159" | cid.APAC2  == "B159", "A" , "")) 
HEPD.APAC <- mutate(APAC_PO, HEPD = ifelse (cid.APAC1 == "B180" | cid.APAC2  == "B180" | cid.APAC1 == "B181" | cid.APAC2  == "B181" | cid.APAC1  == "B160" | cid.APAC2  == "B160" , "D" , "")) 
HEPI.APAC <- mutate(APAC_PO, HEPI = ifelse (cid.APAC1 == "B188" | cid.APAC2  == "B188" | cid.APAC1 == "B189" | cid.APAC2  == "B189" | cid.APAC1 == "B190" | cid.APAC2  == "B190" | cid.APAC1 == "B199" | cid.APAC2  == "B199" | cid.APAC1 == "B942" | cid.APAC2  == "B942" | cid.APAC1 == "O984" | cid.APAC2  == "O984" | cid.APAC1== "P353" | cid.APAC2  == "P353" | cid.APAC1 == "Z225" | cid.APAC2  == "Z225" | cid.APAC1 == "B19" | cid.APAC2  == "B19" | procedimento.APAC1== "0303010118" | procedimento.APAC2== "0303010118", "I" , "")) 


####### cbind ##### APAC ###### put columns from hep classification in the final table for each DB #########

APAC_PO <- cbind.data.frame(APAC_PO, HEPC.APAC$HEPC)
APAC_PO <- cbind.data.frame(APAC_PO, HEPB.APAC$HEPB)
APAC_PO <- cbind.data.frame(APAC_PO, HEPA.APAC$HEPA)
APAC_PO <- cbind.data.frame(APAC_PO, HEPD.APAC$HEPD)
APAC_PO <- cbind.data.frame(APAC_PO, HEPI.APAC$HEPI)


###### SIM_PR ####

HEPC.SIM <- mutate(SIM_PR, HEPC = ifelse (cid.SIM1 == "B171" | cid.SIM2  == "B171" | cid.SIM3  == "B171" | cid.SIM4  == "B171" | cid.SIM5  == "B171" | cid.SIM6  == "B171" | cid.SIM7  == "B171" | cid.SIM1 == "B182" | cid.SIM2  == "B182" | cid.SIM3  == "B182" | cid.SIM4  == "B182" | cid.SIM5  == "B182" | cid.SIM6  == "B182" | cid.SIM7  == "B182" |cid.SIM1 == "*B171" | cid.SIM2  == "*B171" | cid.SIM3  == "*B171" | cid.SIM4  == "*B171" | cid.SIM5  == "*B171" | cid.SIM6  == "*B171" | cid.SIM7  == "*B171" | cid.SIM1 == "*B182" | cid.SIM2  == "*B182" | cid.SIM3  == "*B182" | cid.SIM4  == "*B182" | cid.SIM5  == "*B182" | cid.SIM6  == "*B182" | cid.SIM7  == "*B182" , "C" , "")) 
HEPB.SIM <- mutate(SIM_PR, HEPB = ifelse (cid.SIM1 == "B170" | cid.SIM2  == "B170" | cid.SIM3  == "B170" | cid.SIM4  == "B170" | cid.SIM5  == "B170" | cid.SIM6  == "B170" | cid.SIM7  == "B170"  | cid.SIM1 == "*B170" | cid.SIM2  == "*B170" | cid.SIM3  == "*B170" | cid.SIM4  == "*B170" | cid.SIM5  == "*B170" | cid.SIM6  == "*B170" | cid.SIM7  == "*B170", "B" , ""))
HEPA.SIM <- mutate(SIM_PR, HEPA = ifelse (cid.SIM1 == "B15" | cid.SIM2  == "B15" | cid.SIM3  == "B15" | cid.SIM4  == "B15" | cid.SIM5  == "B15" | cid.SIM6  == "B15" | cid.SIM7  == "B15" | cid.SIM1 == "B150" | cid.SIM2  == "B150" | cid.SIM3  == "B150" | cid.SIM4  == "B150" | cid.SIM5  == "B150" | cid.SIM6  == "B150" | cid.SIM7  == "B150" | cid.SIM1 == "B159" | cid.SIM2  == "B159" | cid.SIM3  == "B159" | cid.SIM4  == "B159" | cid.SIM5  == "B159" | cid.SIM6  == "B159" | cid.SIM7  == "B159"  | cid.SIM1 == "*B15" | cid.SIM2  == "*B15" | cid.SIM3  == "*B15" | cid.SIM4  == "*B15" | cid.SIM5  == "*B15" | cid.SIM6  == "*B15" | cid.SIM7  == "*B15" | cid.SIM1 == "*B150" | cid.SIM2  == "*B150" | cid.SIM3  == "*B150" | cid.SIM4  == "*B150" | cid.SIM5  == "*B150" | cid.SIM6  == "*B150" | cid.SIM7  == "*B150" | cid.SIM1 == "*B159" | cid.SIM2  == "*B159" | cid.SIM3  == "*B159" | cid.SIM4  == "*B159" | cid.SIM5  == "*B159" | cid.SIM6  == "*B159" | cid.SIM7  == "*B159", "A" , "")) 
HEPD.SIM <- mutate(SIM_PR, HEPD = ifelse (cid.SIM1 == "B180" | cid.SIM2  == "B180" | cid.SIM3  == "B180" | cid.SIM4  == "B180" | cid.SIM5  == "B180" | cid.SIM6  == "B180" | cid.SIM7  == "B180" | cid.SIM1 == "B181" | cid.SIM2  == "B181" | cid.SIM3  == "B181" | cid.SIM4  == "B181" | cid.SIM5  == "B181" | cid.SIM6  == "B181" | cid.SIM7  == "B181" | cid.SIM1  == "B160" | cid.SIM2  == "B160" | cid.SIM3  == "B160" | cid.SIM4  == "B160" | cid.SIM5  == "B160" | cid.SIM6  == "B160" | cid.SIM7  == "B160" |  cid.SIM1 == "*B180" | cid.SIM2  == "*B180" | cid.SIM3  == "*B180" | cid.SIM4  == "*B180" | cid.SIM5  == "*B180" | cid.SIM6  == "*B180" | cid.SIM7  == "*B180" | cid.SIM1 == "*B181" | cid.SIM2  == "*B181" | cid.SIM3  == "*B181" | cid.SIM4  == "*B181" | cid.SIM5  == "*B181" | cid.SIM6  == "*B181" | cid.SIM7  == "*B181" | cid.SIM1  == "*B160" | cid.SIM2  == "*B160" | cid.SIM3  == "*B160" | cid.SIM4  == "*B160" | cid.SIM5  == "*B160" | cid.SIM6  == "*B160" | cid.SIM7  == "*B160" , "D" , "")) 
HEPI.SIM <- mutate(SIM_PR, HEPI = ifelse (cid.SIM1 == "B188" | cid.SIM2  == "B188" | cid.SIM3  == "B188" | cid.SIM4  == "B188" | cid.SIM5  == "B188" | cid.SIM6  == "B188" | cid.SIM7  == "B188" | cid.SIM1 == "B189" | cid.SIM2  == "B189" | cid.SIM3  == "B189" | cid.SIM4  == "B189" | cid.SIM5  == "B189" | cid.SIM6  == "B189" | cid.SIM7  == "B189" | cid.SIM1 == "B190" | cid.SIM2  == "B190" | cid.SIM3  == "B190" | cid.SIM4  == "B190" | cid.SIM5  == "B190" | cid.SIM6  == "B190" | cid.SIM7  == "B190" | cid.SIM1 == "B199" | cid.SIM2  == "B199" | cid.SIM3  == "B199" | cid.SIM4  == "B199" | cid.SIM5  == "B199" | cid.SIM6  == "B199" | cid.SIM7  == "B199" | cid.SIM1 == "B942" | cid.SIM2  == "B942" | cid.SIM3  == "B942" | cid.SIM4  == "B942" | cid.SIM5  == "B942" | cid.SIM6  == "B942" | cid.SIM7  == "B942" | cid.SIM1 == "O984" | cid.SIM2  == "O984" | cid.SIM3  == "O984" | cid.SIM4  == "O984" | cid.SIM5  == "O984" | cid.SIM6  == "O984" | cid.SIM7  == "O984" | cid.SIM1 == "P353" | cid.SIM2  == "P353" | cid.SIM3  == "P353" | cid.SIM4  == "P353" | cid.SIM5  == "P353" | cid.SIM6  == "P353" | cid.SIM7  == "P353" | cid.SIM1 == "Z225" | cid.SIM2  == "Z225" | cid.SIM3  == "Z225" | cid.SIM4  == "Z225" | cid.SIM5  == "Z225" | cid.SIM6  == "Z225" | cid.SIM7  == "Z225" | cid.SIM1 == "B19" | cid.SIM2  == "B19" | cid.SIM3  == "B19" | cid.SIM4  == "B19" | cid.SIM5  == "B19" | cid.SIM6  == "B19" | cid.SIM7  == "B19" | cid.SIM1 == "*B188" | cid.SIM2  == "*B188" | cid.SIM3  == "*B188" | cid.SIM4  == "*B188" | cid.SIM5  == "*B188" | cid.SIM6  == "*B188" | cid.SIM7  == "*B188" | cid.SIM1 == "*B189" | cid.SIM2  == "*B189" | cid.SIM3  == "*B189" | cid.SIM4  == "*B189" | cid.SIM5  == "*B189" | cid.SIM6  == "*B189" | cid.SIM7  == "*B189" | cid.SIM1 == "*B190" | cid.SIM2  == "*B190" | cid.SIM3  == "*B190" | cid.SIM4  == "*B190" | cid.SIM5  == "*B190" | cid.SIM6  == "*B190" | cid.SIM7  == "*B190" | cid.SIM1 == "*B199" | cid.SIM2  == "*B199" | cid.SIM3  == "*B199" | cid.SIM4  == "*B199" | cid.SIM5  == "*B199" | cid.SIM6  == "*B199" | cid.SIM7  == "*B199" | cid.SIM1 == "*B942" | cid.SIM2  == "*B942" | cid.SIM3  == "*B942" | cid.SIM4  == "*B942" | cid.SIM5  == "*B942" | cid.SIM6  == "*B942" | cid.SIM7  == "*B942" | cid.SIM1 == "*O984" | cid.SIM2  == "*O984" | cid.SIM3  == "*O984" | cid.SIM4  == "*O984" | cid.SIM5  == "*O984" | cid.SIM6  == "*O984" | cid.SIM7  == "*O984" | cid.SIM1 == "*P353" | cid.SIM2  == "*P353" | cid.SIM3  == "*P353" | cid.SIM4  == "*P353" | cid.SIM5  == "*P353" | cid.SIM6  == "*P353" | cid.SIM7  == "*P353" | cid.SIM1 == "*Z225" | cid.SIM2  == "*Z225" | cid.SIM3  == "*Z225" | cid.SIM4  == "*Z225" | cid.SIM5  == "*Z225" | cid.SIM6  == "*Z225" | cid.SIM7  == "*Z225" | cid.SIM1 == "*B19" | cid.SIM2  == "*B19" | cid.SIM3  == "*B19" | cid.SIM4  == "*B19" | cid.SIM5  == "*B19" | cid.SIM6  == "*B19" | cid.SIM7  == "*B19"  , "I" , "")) 

####### cbind ##### SIM ###### put columns from hep classification in the final table for each DB #########

SIM_PR <- cbind.data.frame(SIM_PR, HEPC.SIM$HEPC)
SIM_PR <- cbind.data.frame(SIM_PR, HEPB.SIM$HEPB)
SIM_PR <- cbind.data.frame(SIM_PR, HEPA.SIM$HEPA)
SIM_PR <- cbind.data.frame(SIM_PR, HEPD.SIM$HEPD)
SIM_PR <- cbind.data.frame(SIM_PR, HEPI.SIM$HEPI)


###### SIM_PO ####

HEPC.SIM <- mutate(SIM_PO, HEPC = ifelse (cid.SIM1 == "B171" | cid.SIM2  == "B171" | cid.SIM3  == "B171" | cid.SIM4  == "B171" | cid.SIM5  == "B171" | cid.SIM6  == "B171" | cid.SIM7  == "B171" | cid.SIM1 == "B182" | cid.SIM2  == "B182" | cid.SIM3  == "B182" | cid.SIM4  == "B182" | cid.SIM5  == "B182" | cid.SIM6  == "B182" | cid.SIM7  == "B182" |cid.SIM1 == "*B171" | cid.SIM2  == "*B171" | cid.SIM3  == "*B171" | cid.SIM4  == "*B171" | cid.SIM5  == "*B171" | cid.SIM6  == "*B171" | cid.SIM7  == "*B171" | cid.SIM1 == "*B182" | cid.SIM2  == "*B182" | cid.SIM3  == "*B182" | cid.SIM4  == "*B182" | cid.SIM5  == "*B182" | cid.SIM6  == "*B182" | cid.SIM7  == "*B182" , "C" , "")) 
HEPB.SIM <- mutate(SIM_PO, HEPB = ifelse (cid.SIM1 == "B170" | cid.SIM2  == "B170" | cid.SIM3  == "B170" | cid.SIM4  == "B170" | cid.SIM5  == "B170" | cid.SIM6  == "B170" | cid.SIM7  == "B170"  | cid.SIM1 == "*B170" | cid.SIM2  == "*B170" | cid.SIM3  == "*B170" | cid.SIM4  == "*B170" | cid.SIM5  == "*B170" | cid.SIM6  == "*B170" | cid.SIM7  == "*B170", "B" , ""))
HEPA.SIM <- mutate(SIM_PO, HEPA = ifelse (cid.SIM1 == "B15" | cid.SIM2  == "B15" | cid.SIM3  == "B15" | cid.SIM4  == "B15" | cid.SIM5  == "B15" | cid.SIM6  == "B15" | cid.SIM7  == "B15" | cid.SIM1 == "B150" | cid.SIM2  == "B150" | cid.SIM3  == "B150" | cid.SIM4  == "B150" | cid.SIM5  == "B150" | cid.SIM6  == "B150" | cid.SIM7  == "B150" | cid.SIM1 == "B159" | cid.SIM2  == "B159" | cid.SIM3  == "B159" | cid.SIM4  == "B159" | cid.SIM5  == "B159" | cid.SIM6  == "B159" | cid.SIM7  == "B159"  | cid.SIM1 == "*B15" | cid.SIM2  == "*B15" | cid.SIM3  == "*B15" | cid.SIM4  == "*B15" | cid.SIM5  == "*B15" | cid.SIM6  == "*B15" | cid.SIM7  == "*B15" | cid.SIM1 == "*B150" | cid.SIM2  == "*B150" | cid.SIM3  == "*B150" | cid.SIM4  == "*B150" | cid.SIM5  == "*B150" | cid.SIM6  == "*B150" | cid.SIM7  == "*B150" | cid.SIM1 == "*B159" | cid.SIM2  == "*B159" | cid.SIM3  == "*B159" | cid.SIM4  == "*B159" | cid.SIM5  == "*B159" | cid.SIM6  == "*B159" | cid.SIM7  == "*B159", "A" , "")) 
HEPD.SIM <- mutate(SIM_PO, HEPD = ifelse (cid.SIM1 == "B180" | cid.SIM2  == "B180" | cid.SIM3  == "B180" | cid.SIM4  == "B180" | cid.SIM5  == "B180" | cid.SIM6  == "B180" | cid.SIM7  == "B180" | cid.SIM1 == "B181" | cid.SIM2  == "B181" | cid.SIM3  == "B181" | cid.SIM4  == "B181" | cid.SIM5  == "B181" | cid.SIM6  == "B181" | cid.SIM7  == "B181" | cid.SIM1  == "B160" | cid.SIM2  == "B160" | cid.SIM3  == "B160" | cid.SIM4  == "B160" | cid.SIM5  == "B160" | cid.SIM6  == "B160" | cid.SIM7  == "B160" |  cid.SIM1 == "*B180" | cid.SIM2  == "*B180" | cid.SIM3  == "*B180" | cid.SIM4  == "*B180" | cid.SIM5  == "*B180" | cid.SIM6  == "*B180" | cid.SIM7  == "*B180" | cid.SIM1 == "*B181" | cid.SIM2  == "*B181" | cid.SIM3  == "*B181" | cid.SIM4  == "*B181" | cid.SIM5  == "*B181" | cid.SIM6  == "*B181" | cid.SIM7  == "*B181" | cid.SIM1  == "*B160" | cid.SIM2  == "*B160" | cid.SIM3  == "*B160" | cid.SIM4  == "*B160" | cid.SIM5  == "*B160" | cid.SIM6  == "*B160" | cid.SIM7  == "*B160" , "D" , "")) 
HEPI.SIM <- mutate(SIM_PO, HEPI = ifelse (cid.SIM1 == "B188" | cid.SIM2  == "B188" | cid.SIM3  == "B188" | cid.SIM4  == "B188" | cid.SIM5  == "B188" | cid.SIM6  == "B188" | cid.SIM7  == "B188" | cid.SIM1 == "B189" | cid.SIM2  == "B189" | cid.SIM3  == "B189" | cid.SIM4  == "B189" | cid.SIM5  == "B189" | cid.SIM6  == "B189" | cid.SIM7  == "B189" | cid.SIM1 == "B190" | cid.SIM2  == "B190" | cid.SIM3  == "B190" | cid.SIM4  == "B190" | cid.SIM5  == "B190" | cid.SIM6  == "B190" | cid.SIM7  == "B190" | cid.SIM1 == "B199" | cid.SIM2  == "B199" | cid.SIM3  == "B199" | cid.SIM4  == "B199" | cid.SIM5  == "B199" | cid.SIM6  == "B199" | cid.SIM7  == "B199" | cid.SIM1 == "B942" | cid.SIM2  == "B942" | cid.SIM3  == "B942" | cid.SIM4  == "B942" | cid.SIM5  == "B942" | cid.SIM6  == "B942" | cid.SIM7  == "B942" | cid.SIM1 == "O984" | cid.SIM2  == "O984" | cid.SIM3  == "O984" | cid.SIM4  == "O984" | cid.SIM5  == "O984" | cid.SIM6  == "O984" | cid.SIM7  == "O984" | cid.SIM1 == "P353" | cid.SIM2  == "P353" | cid.SIM3  == "P353" | cid.SIM4  == "P353" | cid.SIM5  == "P353" | cid.SIM6  == "P353" | cid.SIM7  == "P353" | cid.SIM1 == "Z225" | cid.SIM2  == "Z225" | cid.SIM3  == "Z225" | cid.SIM4  == "Z225" | cid.SIM5  == "Z225" | cid.SIM6  == "Z225" | cid.SIM7  == "Z225" | cid.SIM1 == "B19" | cid.SIM2  == "B19" | cid.SIM3  == "B19" | cid.SIM4  == "B19" | cid.SIM5  == "B19" | cid.SIM6  == "B19" | cid.SIM7  == "B19" | cid.SIM1 == "*B188" | cid.SIM2  == "*B188" | cid.SIM3  == "*B188" | cid.SIM4  == "*B188" | cid.SIM5  == "*B188" | cid.SIM6  == "*B188" | cid.SIM7  == "*B188" | cid.SIM1 == "*B189" | cid.SIM2  == "*B189" | cid.SIM3  == "*B189" | cid.SIM4  == "*B189" | cid.SIM5  == "*B189" | cid.SIM6  == "*B189" | cid.SIM7  == "*B189" | cid.SIM1 == "*B190" | cid.SIM2  == "*B190" | cid.SIM3  == "*B190" | cid.SIM4  == "*B190" | cid.SIM5  == "*B190" | cid.SIM6  == "*B190" | cid.SIM7  == "*B190" | cid.SIM1 == "*B199" | cid.SIM2  == "*B199" | cid.SIM3  == "*B199" | cid.SIM4  == "*B199" | cid.SIM5  == "*B199" | cid.SIM6  == "*B199" | cid.SIM7  == "*B199" | cid.SIM1 == "*B942" | cid.SIM2  == "*B942" | cid.SIM3  == "*B942" | cid.SIM4  == "*B942" | cid.SIM5  == "*B942" | cid.SIM6  == "*B942" | cid.SIM7  == "*B942" | cid.SIM1 == "*O984" | cid.SIM2  == "*O984" | cid.SIM3  == "*O984" | cid.SIM4  == "*O984" | cid.SIM5  == "*O984" | cid.SIM6  == "*O984" | cid.SIM7  == "*O984" | cid.SIM1 == "*P353" | cid.SIM2  == "*P353" | cid.SIM3  == "*P353" | cid.SIM4  == "*P353" | cid.SIM5  == "*P353" | cid.SIM6  == "*P353" | cid.SIM7  == "*P353" | cid.SIM1 == "*Z225" | cid.SIM2  == "*Z225" | cid.SIM3  == "*Z225" | cid.SIM4  == "*Z225" | cid.SIM5  == "*Z225" | cid.SIM6  == "*Z225" | cid.SIM7  == "*Z225" | cid.SIM1 == "*B19" | cid.SIM2  == "*B19" | cid.SIM3  == "*B19" | cid.SIM4  == "*B19" | cid.SIM5  == "*B19" | cid.SIM6  == "*B19" | cid.SIM7  == "*B19"  , "I" , "")) 

####### cbind ##### SIM ###### put columns from hep classification in the final table for each DB #########

SIM_PO <- cbind.data.frame(SIM_PO, HEPC.SIM$HEPC)
SIM_PO <- cbind.data.frame(SIM_PO, HEPB.SIM$HEPB)
SIM_PO <- cbind.data.frame(SIM_PO, HEPA.SIM$HEPA)
SIM_PO <- cbind.data.frame(SIM_PO, HEPD.SIM$HEPD)
SIM_PO <- cbind.data.frame(SIM_PO, HEPI.SIM$HEPI)


######  BPAI_PR  #####

HEPC.BPAI <- mutate(BPAI_PR, HEPC = ifelse (cid.BPAI == "B171" | cid.BPAI == "B182" | procedimento.BPAI== "0202030059" | procedimento.BPAI== "0202030210" | procedimento.BPAI== "0202031080" | procedimento.BPAI== "0213010542" | procedimento.BPAI== "0604760027" | procedimento.BPAI== "0604760035" | procedimento.BPAI== "0604640030" | procedimento.BPAI== "0604760043" | procedimento.BPAI== "0604390041" | procedimento.BPAI== "0604390017" | procedimento.BPAI== "0604460023" | procedimento.BPAI== "0604460058" | procedimento.BPAI== "0604460040" | procedimento.BPAI== "0604390050"  | procedimento.BPAI== "0604390068" | procedimento.BPAI== "0604390025" | procedimento.BPAI== "0604390033"  | procedimento.BPAI== "0604390076" | procedimento.BPAI== "0604450010", "C" , "")) 
HEPB.BPAI <- mutate(BPAI_PR, HEPB = ifelse (cid.BPAI == "B170" | procedimento.BPAI== "0604390041" | procedimento.BPAI== "0604390050" | procedimento.BPAI== "0604390068" | procedimento.BPAI== "0604390076" | procedimento.BPAI== "0604390017" | procedimento.BPAI== "0604460023" | procedimento.BPAI== "0604460058" | procedimento.BPAI== "0604460040" | procedimento.BPAI== "0604390025" | procedimento.BPAI== "0604390033" | procedimento.BPAI== "0604450010", "B" , ""))
HEPA.BPAI <- mutate(BPAI_PR, HEPA = ifelse (cid.BPAI == "B15" | cid.BPAI == "B150" | cid.BPAI == "B159", "A" , "")) 
HEPD.BPAI <- mutate(BPAI_PR, HEPD = ifelse (cid.BPAI == "B180" | cid.BPAI == "B181" | cid.BPAI  == "B160", "D" , "")) 
HEPI.BPAI <- mutate(BPAI_PR, HEPI = ifelse (cid.BPAI == "B188" | cid.BPAI == "B189" | cid.BPAI == "B190" | cid.BPAI == "B199" | cid.BPAI == "B942" | cid.BPAI == "O984" | cid.BPAI == "P353" | cid.BPAI == "Z225" | cid.BPAI == "B19" | procedimento.BPAI== "0303010118", "I" , "")) 

####### cbind ##### BPAI ###### put columns from hep classification in the final table for each DB #########

BPAI_PR <- cbind.data.frame(BPAI_PR, HEPC.BPAI$HEPC)
BPAI_PR <- cbind.data.frame(BPAI_PR, HEPB.BPAI$HEPB)
BPAI_PR <- cbind.data.frame(BPAI_PR, HEPA.BPAI$HEPA)
BPAI_PR <- cbind.data.frame(BPAI_PR, HEPD.BPAI$HEPD)
BPAI_PR <- cbind.data.frame(BPAI_PR, HEPI.BPAI$HEPI)

######  BPAI_PO  #####

HEPC.BPAI <- mutate(BPAI_PO, HEPC = ifelse (cid.BPAI == "B171" | cid.BPAI == "B182" | procedimento.BPAI== "0202030059" | procedimento.BPAI== "0202030210" | procedimento.BPAI== "0202031080" | procedimento.BPAI== "0213010542" | procedimento.BPAI== "0604760027" | procedimento.BPAI== "0604760035" | procedimento.BPAI== "0604640030" | procedimento.BPAI== "0604760043" | procedimento.BPAI== "0604390041" | procedimento.BPAI== "0604390017" | procedimento.BPAI== "0604460023" | procedimento.BPAI== "0604460058" | procedimento.BPAI== "0604460040" | procedimento.BPAI== "0604390050"  | procedimento.BPAI== "0604390068" | procedimento.BPAI== "0604390025" | procedimento.BPAI== "0604390033"  | procedimento.BPAI== "0604390076" | procedimento.BPAI== "0604450010", "C" , "")) 
HEPB.BPAI <- mutate(BPAI_PO, HEPB = ifelse (cid.BPAI == "B170" | procedimento.BPAI== "0604390041" | procedimento.BPAI== "0604390050" | procedimento.BPAI== "0604390068" | procedimento.BPAI== "0604390076" | procedimento.BPAI== "0604390017" | procedimento.BPAI== "0604460023" | procedimento.BPAI== "0604460058" | procedimento.BPAI== "0604460040" | procedimento.BPAI== "0604390025" | procedimento.BPAI== "0604390033" | procedimento.BPAI== "0604450010", "B" , ""))
HEPA.BPAI <- mutate(BPAI_PO, HEPA = ifelse (cid.BPAI == "B15" | cid.BPAI == "B150" | cid.BPAI == "B159", "A" , "")) 
HEPD.BPAI <- mutate(BPAI_PO, HEPD = ifelse (cid.BPAI == "B180" | cid.BPAI == "B181" | cid.BPAI  == "B160", "D" , "")) 
HEPI.BPAI <- mutate(BPAI_PO, HEPI = ifelse (cid.BPAI == "B188" | cid.BPAI == "B189" | cid.BPAI == "B190" | cid.BPAI == "B199" | cid.BPAI == "B942" | cid.BPAI == "O984" | cid.BPAI == "P353" | cid.BPAI == "Z225" | cid.BPAI == "B19" | procedimento.BPAI== "0303010118", "I" , "")) 

####### cbind ##### BPAI ###### put columns from hep classification in the final table for each DB #########

BPAI_PO <- cbind.data.frame(BPAI_PO, HEPC.BPAI$HEPC)
BPAI_PO <- cbind.data.frame(BPAI_PO, HEPB.BPAI$HEPB)
BPAI_PO <- cbind.data.frame(BPAI_PO, HEPA.BPAI$HEPA)
BPAI_PO <- cbind.data.frame(BPAI_PO, HEPD.BPAI$HEPD)
BPAI_PO <- cbind.data.frame(BPAI_PO, HEPI.BPAI$HEPI)

############## Saving tables

write.csv(AIH_PR, file = '/Users/mikaellemos/Projeto_BDBM/AIH_PR.csv', row.names=FALSE)

write.csv(APAC_PR, file = '/Users/mikaellemos/Projeto_BDBM/APAC_PR.csv', row.names=FALSE)

write.csv(BPAI_PR, file = '/Users/mikaellemos/Projeto_BDBM/BPAI_PR.csv', row.names=FALSE)

write.csv(SIM_PR, file = '/Users/mikaellemos/Projeto_BDBM/SIM_PR.csv', row.names=FALSE)



write.csv(AIH_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/AIH_PO.csv', row.names=FALSE)

write.csv(APAC_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/APAC_PO.csv', row.names=FALSE)

write.csv(BPAI_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/BPAI_PO.csv', row.names=FALSE)

write.csv(SIM_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/SIM_PO.csv', row.names=FALSE)



write.csv(AIH_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/AIH_PR.csv', row.names=FALSE)

write.csv(APAC_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/APAC_PR.csv', row.names=FALSE)

write.csv(BPAI_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/BPAI_PR.csv', row.names=FALSE)

write.csv(SIM_PR, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/SIM_PR.csv', row.names=FALSE)


### PO 700
write.csv(AIH_PR, file = '/Users/alexandre.fonseca/Desktop/samples/AIH_PR.csv', row.names=FALSE)

write.csv(APAC_PR, file = '/Users/alexandre.fonseca/Desktop/samples/APAC_PR.csv', row.names=FALSE)

write.csv(BPAI_PR, file = '/Users/alexandre.fonseca/Desktop/samples/BPAI_PR.csv', row.names=FALSE)

write.csv(SIM_PR, file = '/Users/alexandre.fonseca/Desktop/samples/SIM_PR.csv', row.names=FALSE)


write.csv(AIH_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/AIH_PO.csv', row.names=FALSE)

write.csv(APAC_PO, file = '/Users/mikaellemos/Projeto_BDBM/APAC_PO.csv', row.names=FALSE)

write.csv(BPAI_PO, file = '/Users/mikaellemos/Projeto_BDBM/BPAI_PO.csv', row.names=FALSE)

write.csv(SIM_PO, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/SIM_PO.csv.csv', row.names=FALSE)

### PO 700
write.csv(AIH_PO, file = '/Users/alexandre.fonseca/Desktop/samples/AIH_PO.csv', row.names=FALSE)

write.csv(APAC_PO, file = '/Users/alexandre.fonseca/Desktop/samples/APAC_PO.csv', row.names=FALSE)

write.csv(BPAI_PO, file = '/Users/alexandre.fonseca/Desktop/samples/BPAI_PO.csv', row.names=FALSE)

write.csv(SIM_PO, file = '/Users/alexandre.fonseca/Desktop/samples/SIM_PO.csv', row.names=FALSE)


##### SINAN complete and fixed

write.csv(SINAN, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_PO_FILTRADO_HEP_TYPE_BD/SINAN.csv ', row.names=FALSE)


############ Simple count per column - Hepatitis types ######## AIH
table(AIH_PR$`HEPC.AIH$HEPC`)
table(AIH_PR$`HEPB.AiH$HEPB`)
table(AIH_PR$`HEPA.AIH$HEPA`)
table(AIH_PR$`HEPD.AIH$HEPD`)
table(AIH_PR$`HEPI.AIH$HEPI`)

############ Simple count per column - Hepatitis types ######## APAC
table(APAC_PR$`HEPC.APAC$HEPC`)
table(APAC_PR$`HEPB.APAC$HEPB`)
table(APAC_PR$`HEPA.APAC$HEPA`)
table(APAC_PR$`HEPD.APAC$HEPD`) 
table(APAC_PR$`HEPI.APAC$HEPI`)


############ Simple count per column - Hepatitis types ######## BPAI
table(BPAI_PR$`HEPC.BPAI$HEPC`)
table(BPAI_PR$`HEPB.BPAI$HEPB`)
table(BPAI_PR$`HEPA.BPAI$HEPA`)
table(BPAI_PR$`HEPD.BPAI$HEPD`)
table(BPAI_PR$`HEPI.BPAI$HEPI`)

############ Simple count per column - Hepatitis types ######## SIM
table(SIM_PR$`HEPC.SIM$HEPC`) 
table(SIM_PR$`HEPB.SIM$HEPB`)
table(SIM_PR$`HEPA.SIM$HEPA`)
table(SIM_PR$`HEPD.SIM$HEPD`)
table(SIM_PR$`HEPI.SIM$HEPI`)


#### obs: The count can be made using either table(HEPC.APAC$HEPC) or table(APAC_PR$`HEPB.APAC$HEPB`)
