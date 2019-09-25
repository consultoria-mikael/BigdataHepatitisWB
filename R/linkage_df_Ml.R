############ Linkage - data frame #############
##################################################
###### script developed by Mikael Lemos ##########
###### Version 1.0 -24.04.2019 ##################
## Linkage Data Frames for machine learning #####
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

install.packages("devtools")
library(devtools)

######
### Loading DATA FRAME
######

##### AIH/SIM ML #####
#### MAC mikael
AIH_SIM_ML <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/AIH_SIM_ML.csv") 

##### Socieconomic Variables #####
#### MAC mikael
socieconomic_variables <- read.csv2("/Users/mikaellemos/Downloads/Data_set_socioeconomic_characteristics.csv") 

########################
###### LINKAGE OF DF ####
ML_TABELA_SOC <- merge(AIH_SIM_ML, socieconomic_variables, by.x = "NU_PACIENTE_LOGR_MUNICIPIO"  , by.y = "Municipality_code_2" , all = T)

Municipality_code_2 <- substr(socieconomic_variables$Municipality_code, 1, 6)

Municipality_code_2 <- as.data.frame(Municipality_code_2)

socieconomic_variables <- cbind(Municipality_code_2, socieconomic_variables)

write.csv(ML_TABELA_SOC, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/ML_TABELA_SOC.csv', row.names=FALSE)
ML_TABELA_SOC <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/ML_TABELA_SOC.csv") 
