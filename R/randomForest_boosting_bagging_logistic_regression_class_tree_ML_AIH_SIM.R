########Tree Models################
###################################


### Installing Packages

### ISLR : test data for Random Forest 
install.packages("ISLR")
install.packages("randomForest")
install.packages("tree")
install.packages("Matrix")

##Loading packages

library("ISLR")
library("ff")
library("Amelia")
library("caTools")
library("dplyr")
library("ggplot2")
library("randomForest")
library('tree')

####
# Loading df
####

ML_TABELA_SOC <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/ML_TABELA_SOC.csv") 

####
# adding classification about outcome
####

##### Types of treatment outcome ###

table(ML_TABELA_SOC$DS_DESCRICAO, useNA = "always")
table(ML_TABELA_SOC$CO_MOT_SAIDA, useNA = "always")


### Outcome = Death ####

# morte/death -> yes

# alta/treated -> no

#### asigning binary classification : yes/no ####

ML_TABELA_SOC$DS_DESCRICAO <- as.character(ML_TABELA_SOC$DS_DESCRICAO) 
ML_TABELA_SOC$CO_MOT_SAIDA <- as.character(ML_TABELA_SOC$CO_MOT_SAIDA)

### Duplicate column CO_MOT_SAIDA

ML_TABELA_SOC$Class_ML = ML_TABELA_SOC$CO_MOT_SAIDA
ML_TABELA_SOC$Class_ML <- as.character(ML_TABELA_SOC$Class_ML)

#### Binary Classification ### yes = Death / no = Treated
#### yes
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "42"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "41"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "43"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "65"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "66"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "67"] <- "yes"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "25"] <- "yes"

#### no
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "11"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "12"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "13"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "14"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "15"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "16"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "17"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "18"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "19"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "21"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "22"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "23"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "24"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "26"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "27"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "28"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "29"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "31"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "32"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "51"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "61"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "62"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "63"] <- "no"
ML_TABELA_SOC$Class_ML[ML_TABELA_SOC$Class_ML == "64"] <- "no"


write.csv(ML_TABELA_SOC, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/ML_TABELA_SOC.csv', row.names=FALSE)

############# Define factor, integer and numeric (all variables)

ML_TABELA_SOC.sample_var$CO_CAR_INTERNACAO <- as.factor(ML_TABELA_SOC.sample_var$CO_CAR_INTERNACAO)

ML_TABELA_SOC.sample_var_2$CO_CAR_INTERNACAO <- as.factor(ML_TABELA_SOC.sample_var_2$CO_CAR_INTERNACAO)



########
## Classification trees : bagging, random forest, boosting
########


##Random Sampling of a df 
ML_TABELA_SOC.sample_var <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 10000), ]    ## 10000 lines, random

ML_TABELA_SOC.sample_var <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 20000), ]    ## 20000 lines, random

ML_TABELA_SOC.sample_var <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 40000), ]    ## 40000 lines, random

ML_TABELA_SOC.sample_var <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 80000), ]    ## 80000 lines, random

ML_TABELA_SOC.sample_var_2 <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 80000), ]    ## 80000 lines, random

##### Select variables

ML_TABELA_SOC <- select(ML_TABELA_SOC, -ID_PACIENTE, -DT_CMPT, -CO_CID_OBITO, -ID_PACIENTE.1, -DT_INTERNACAO, -DT_SAIDA, -CO_MOT_SAIDA, -DS_DESCRICAO, -DTOBITO, -ATESTADO, -DT_PACIENTE_NASCIMENTO, -CID, -CO_CNES, -RACACOR, -Municipality_code, -NU_MUN_HOSP, -NU_PACIENTE_LOGR_MUNICIPIO, -CO_PROC_SOLICITADO, -CO_PROCEDIMENTO_PRINCIPAL, -COD_PROCEDIMENTO_SECUNDARIO, -DS_CARATER)


### ML_TABELA_SOC <- unite(ML_TABELA_SOC, "CID", c("CO_CID_PRINCIPAL", "CO_CID_SECUNDARIO_1", "CO_CID_SECUNDARIO_2", "CO_CID_SECUNDARIO_3", "CO_CID_SECUNDARIO_4", "CO_CID_SECUNDARIO_5", "CO_CID_SECUNDARIO_6", "CO_CID_SECUNDARIO_7", "CO_CID_SECUNDARIO_8","CO_CID_SECUNDARIO_9" ))

######
### Bagging
######

set.seed(101)
train = sample(1:nrow(ML_TABELA_SOC.sample_var), 8000)

bag.ML=randomForest(Class_ML~. , data = ML_TABELA_SOC.sample_var,subset=train, mtry=46,importance=TRUE, prOximity=TRUE, na.action=na.roughfix) 


ML_TABELA_SOC.sample_var.test=ML_TABELA_SOC.sample_var[-train ,"Class_ML"]

yhat.bag = predict(bag.ML ,newdata=ML_TABELA_SOC.sample_var[-train ,])

plot(yhat.bag, ML_TABELA_SOC.sample_var.test)
abline(0,1)

mean((yhat.bag - ML_TABELA_SOC.sample_var.test)^2)



######
### Random Forest : model 1
######

set.seed (1)

rf.ML_TABELA_SOC.sample_var=randomForest(Class_ML~.,data=ML_TABELA_SOC.sample_var,subset=train, mtry=46,importance =TRUE, prOximity=TRUE, na.action=na.roughfix)

yhat.rf = predict(rf.ML_TABELA_SOC.sample_var ,newdata=ML_TABELA_SOC.sample_var[-train ,])

mean((yhat.rf-ML_TABELA_SOC.sample_var.test)^2)

importance (rf.ML_TABELA_SOC.sample_var)
varImpPlot(rf.ML_TABELA_SOC.sample_var)

ML_TABELA_SOC.sample_var$Class_ML <- as.character(ML_TABELA_SOC.sample_var$Class_ML)
ML_TABELA_SOC.sample_var$Class_ML <- as.factor(ML_TABELA_SOC.sample_var$Class_ML)

## ML_TABELA_SOC.sample_var$Class_ML[ML_TABELA_SOC.sample_var$Class_ML == "yes"] <- "1"
## ML_TABELA_SOC.sample_var$Class_ML[ML_TABELA_SOC.sample_var$Class_ML == "no"] <- "2"

ML_TABELA_SOC.sample_var$Class_ML <- as.numeric(ML_TABELA_SOC.sample_var$Class_ML)

#### Dealing with NA's ###### SIM #######
## Changing Blank and "*" to "NA"  
#df.AIH[] <- lapply(df.AIH, str_trim) 
is.na(ML_TABELA_SOC.sample_var) <- ML_TABELA_SOC.sample_var==''  
is.na(ML_TABELA_SOC.sample_var) <- ML_TABELA_SOC.sample_var=='*' 
is.na(ML_TABELA_SOC.sample_var) <- ML_TABELA_SOC.sample_var=='//'

is.na(ML_TABELA_SOC.sample_var_2) <- ML_TABELA_SOC.sample_var_2==''  
is.na(ML_TABELA_SOC.sample_var_2) <- ML_TABELA_SOC.sample_var_2=='*' 
is.na(ML_TABELA_SOC.sample_var_2) <- ML_TABELA_SOC.sample_var_2=='//'


######
## Classification trees
######

tree.ML =tree(Class_ML~. ,ML_TABELA_SOC.sample_var)

summary(tree.ML)

##### Simple tree plot
plot(tree.ML)
text(tree.ML ,pretty = 0)




plot(tree.ML)
###### Fancy tree plot
install.packages("rattle")
install.packages("data.tree")

install.packages("rpart.plot")

library(rattle)					# Fancy tree plot
library(rpart.plot)
library(data.tree)



tree.ML

set.seed (2)

train=sample(1:nrow(ML_TABELA_SOC.sample_var), 8000)

test=ML_TABELA_SOC.sample_var [-train ,]

Class_ML.test=ML_TABELA_SOC.sample_var$Class_ML[-train]

tree.ML=tree(Class_ML~. ,ML_TABELA_SOC.sample_var)

tree.pred=predict(tree.ML, ML.test,type="class")

table(tree.pred, Class_ML.test)

## accurcy

(648+31)/800

(609+58)/800

## For this case acutacy of 85%

#####
## Prune tree 
#####

set.seed (3)

cv.ML =cv.tree(tree.ML ,FUN=prune.misclass )

names(cv.ML)

cv.ML

### Plot error rate as a function of both size and k.

par(mfrow=c(1,2))


plot(cv.ML$size ,cv.ML$dev ,type="b")
plot(cv.ML$k ,cv.ML$dev ,type="b")


prune.ML=prune.misclass(tree.ML,best=5)

plot(prune.ML )
text(prune.ML,pretty=0)

### Accuracy prune tree

tree.pred=predict(prune.ML,ML.test,type="class")

table(tree.pred ,Class_ML.test)

(651+27)/800


######
### Classification tree : rpart package
######

library("rpart")

class_ML_rpart <- rpart(Class_ML~. ,method="class", data=ML_TABELA_SOC.sample_var)


printcp(class_ML_rpart) # display the results 
plotcp(class_ML_rpart) # visualize cross-validation results 
summary(class_ML_rpart) # detailed summary of splits


  predict_unseen <- predict(class_ML_rpart, test, type = 'class')
  table_mat <- table(test$Class_ML, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test

  fancyRpartPlot(class_ML_rpart)
  
# plot tree 
plot(class_ML_rpart, uniform=TRUE, main= "Classification Tree for medical outcome")
text(class_ML_rpart, use.n=TRUE, all=TRUE, cex=.8)


post(class_ML_rpart, file = "/Volumes/Mikael_backup3/PROJETO_BDBM/PLOTS/tree_rpart.ps", 
     title = "Classification Tree for hepatitis medical outcome")

install.packages("party")

#### prune tree

# prune the tree 
pclass_ML_rpart<- prune(class_ML_rpart, cp=   class_ML_rpart$cptable[which.min(class_ML_rpart$cptable[,"xerror"]),"CP"])

printcp(pclass_ML_rpart) # display the results 
plotcp(pclass_ML_rpart) # visualize cross-validation results 
summary(pclass_ML_rpart) # detailed summary of splits

predict_unseen <- predict(pclass_ML_rpart, test, type = 'class')
table_mat <- table(test$Class_ML, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

precision = table_mat[1,1]/sum(table_mat[,1])
recall = table_mat[1,1]/sum(table_mat[1,])
f = 2 * (precision * recall) / (precision + recall)

precision
recall
f

# plot the pruned tree 
plot(pclass_ML_rpart, uniform=TRUE, 
     main="Pruned Classification Tree for hepatitis medical outcome")
text(pclass_ML_rpart, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(pclass_ML_rpart)


post(pclass_ML_rpart, file = "/Volumes/Mikael_backup3/PROJETO_BDBM/PLOTS/prunedtree_rpart.ps", 
     title = "Pruned Classification Tree for hepatitis medical outcome")

# Conditional Inference Tree for hepatitis
library(party)
library(modeltools)

install.packages("modeltools")
install.packages("stats4")


class_ML_rpart_party <- ctree(Class_ML ~ CO_MOT_SAIDA+CO_PROCEDIMENTO_PRINCIPAL, 
             data=ML_TABELA_SOC.sample_var)
plot(class_ML_rpart_party, main="Conditional Inference Tree for hepatitis medical outcome")



####
# Checking df
####

# head(df)

print(head(df.BPAI))


# str(df)

print(str(df.BPAI))

####
# Data overview ggplot2
####

##scatterplot


ggplot(df.BPAI.sample, aes(MUNICIPIO_PACIENTE,QT_REALIZADO)) + geom_point(aes(color=SEXO), size=4, alpha=0.5)


##histogram

ggplot(df.BPAI.sample, aes(VALOR)) + geom_histogram((aes(fill=SEXO)), color="black", bins=50, alpha=0.5) + theme_bw()

## Checking incorrect entries in the data frame using dplyr

# subset(df, columnname > 6000)  # shows values greater than 6000

## Substituting incorrect/invalid values in the df

# df["rowname", "columnname" ] <- 100 # atributing a new value


########
##Train test split  
########

library("caTools")

set.seed(101)

sample <- sample.split(ML_TABELA_SOC.sample_var$Class_ML, SplitRatio = 0.8) ## obs: here add variable to be predicted

sample2 <- sample.split(ML_TABELA_SOC.sample_var_2$Class_ML, SplitRatio = 0.8) ## obs: here add variable to be predicted


######
## Train
#####

train <- subset(ML_TABELA_SOC.sample_var, sample == T)

train2 <- subset(ML_TABELA_SOC.sample_var_2, sample == T)

######
## Test
######

test <- subset(ML_TABELA_SOC.sample_var, sample == F)

test2 <- subset(ML_TABELA_SOC.sample_var_2, sample == F)


######
## Random Forest model : model 2
######

rf.model <- randomForest(Class_ML~. , data = train, importance=TRUE, prOximity = TRUE)

ML_TABELA_SOC.sample_var <- na.omit(ML_TABELA_SOC.sample_var)
train <- na.omit(train)
test <- na.omit(test)


ML_TABELA_SOC.sample_var_2 <- na.omit(ML_TABELA_SOC.sample_var_2)
train2 <- na.omit(train2)
test2 <- na.omit(test2)

######
## Confusion Matrix
######

print.table(rf.model$confusion)

######
## Importance : Read ISLR to tree methods
######

rf.model$importance

importance_class1 <- importance(rf.model, class=1)

importance_type1 <- importance(rf.model, type=1, scale = F)

plot_importance <- varImpPlot(rf.model,type=1) 


plot_importance <- varImpPlot(rf.model,type=1, scale = F)  # permutation importances


######
## Predictions
######

rf.preeds <- predict(rf.model, test)

######
## Table
#####

table(rf.preeds, test$Class_ML)

## acuracy
(1611+55)/1981 

(6407+263)/7901 



###Recall
1611/(1611+281)

6407/(6407+1097)

###Precision
6407/(6407+134)

importance (rf.model)
varImpPlot(rf.model)


#######
## Logistic Regression Model
#######

##### TRAIN/TEST SPLIT ######


#set.seed(101)

sample <- sample.split(ML_TABELA_SOC.sample_var$Class_ML, SplitRatio = 0.8) ## obs: here add variable to be predicted
## obs2:  This is only a test

######
## Train
#####

train <- subset(ML_TABELA_SOC.sample_var, sample == T)

######
## Test
######

test <- subset(ML_TABELA_SOC.sample_var, sample == F)

#### REMOVE NAS

ML_TABELA_SOC.sample_var <- na.omit(ML_TABELA_SOC.sample_var)
train <- na.omit(train)
test <- na.omit(test)

######
## MODEL
######

model <- glm(Class_ML~. , family = binomial(link = logit), data = train)

summary(model)

### Akiki criteria - AIC - function step() #### try to fit multiple models to select best variables 

new.step.model <- step(model)

summary(new.step.model)

#### Predict test

test$predicted.outcome <- predict(model, newdata = test, type = "response")

### Confusion Matrix
table(test$Class_ML, test$predicted.outcome > 0.5)


accuracy <- (1586+19)/(1586+346+23+19)

accuracy

# recall
recall <- 1586/(1586+23)

recall
# precision
precision <- 1586/(1586+346)

precision
############
##### Boosting
############

###### Split : Train/Test
library(caTools)

sample <- sample.split(ML_TABELA_SOC.sample_var$Class_ML, SplitRatio = 0.8)

###Train###

train <- subset(ML_TABELA_SOC.sample_var, sample == T)
train <- na.omit(train)

train2$Class_ML <- as.character(train2$Class_ML)

train2$Class_ML[train2$Class_ML == "yes"] <- "0"
train2$Class_ML[train2$Class_ML == "no"] <- "1"

train2$Class_ML <- as.factor(train2$Class_ML)

###Test###

test <- subset(ML_TABELA_SOC.sample_var, sample == F)
test <- na.omit(test)

test2$Class_ML <- as.character(test2$Class_ML)

test2$Class_ML[test2$Class_ML == "yes"] <- "0"
test2$Class_ML[test2$Class_ML == "no"] <- "1"

test2$Class_ML <- as.factor(test2$Class_ML)

##########################################


install.packages("gbm")
library(gbm)

library(xgboost)

########## Boosting algorithm only accepts numeric variables ###### change to numeric/factor

##### CO_PACIENTE_SEXO ### M =1 # F=2

### train
train2$CO_PACIENTE_SEXO <- as.character(train2$CO_PACIENTE_SEXO)

train2$CO_PACIENTE_SEXO[train2$CO_PACIENTE_SEXO == "M"] <- "1"
train2$CO_PACIENTE_SEXO[train2$CO_PACIENTE_SEXO == "F"] <- "2"

train2$CO_PACIENTE_SEXO <- as.factor(train2$CO_PACIENTE_SEXO)

### test
test2$CO_PACIENTE_SEXO <- as.character(test2$CO_PACIENTE_SEXO)

test2$CO_PACIENTE_SEXO[test2$CO_PACIENTE_SEXO == "M"] <- "1"
test2$CO_PACIENTE_SEXO[test2$CO_PACIENTE_SEXO == "F"] <- "2"

test2$CO_PACIENTE_SEXO <- as.factor(test2$CO_PACIENTE_SEXO)

##### State_of_residence ### 

### train
train2$State_of_residence <- as.character(train2$State_of_residence)

train2$State_of_residence[train2$State_of_residence == "RO"] <- "11"
train2$State_of_residence[train2$State_of_residence == "AC"] <- "12"
train2$State_of_residence[train2$State_of_residence == "AM"] <- "13"
train2$State_of_residence[train2$State_of_residence == "RR"] <- "14"
train2$State_of_residence[train2$State_of_residence == "PA"] <- "15"
train2$State_of_residence[train2$State_of_residence == "AP"] <- "16"
train2$State_of_residence[train2$State_of_residence == "TO"] <- "17"
train2$State_of_residence[train2$State_of_residence == "MA"] <- "21"
train2$State_of_residence[train2$State_of_residence == "PI"] <- "22"
train2$State_of_residence[train2$State_of_residence == "CE"] <- "23"
train2$State_of_residence[train2$State_of_residence == "RN"] <- "24"
train2$State_of_residence[train2$State_of_residence == "PB"] <- "25"
train2$State_of_residence[train2$State_of_residence == "PE"] <- "26"
train2$State_of_residence[train2$State_of_residence == "AL"] <- "27"
train2$State_of_residence[train2$State_of_residence == "SE"] <- "28"
train2$State_of_residence[train2$State_of_residence == "BA"] <- "29"
train2$State_of_residence[train2$State_of_residence == "MG"] <- "31"
train2$State_of_residence[train2$State_of_residence == "ES"] <- "32"
train2$State_of_residence[train2$State_of_residence == "RJ"] <- "33"
train2$State_of_residence[train2$State_of_residence == "SP"] <- "35"
train2$State_of_residence[train2$State_of_residence == "PR"] <- "41"
train2$State_of_residence[train2$State_of_residence == "SC"] <- "42"
train2$State_of_residence[train2$State_of_residence == "RS"] <- "43"
train2$State_of_residence[train2$State_of_residence == "MS"] <- "50"
train2$State_of_residence[train2$State_of_residence == "MT"] <- "51"
train2$State_of_residence[train2$State_of_residence == "GO"] <- "52"
train2$State_of_residence[train2$State_of_residence == "DF"] <- "53"

train2$State_of_residence <- as.factor(train2$State_of_residence)

### test
test2$State_of_residence <- as.character(test2$State_of_residence)

test2$State_of_residence[test2$State_of_residence == "RO"] <- "11"
test2$State_of_residence[test2$State_of_residence == "AC"] <- "12"
test2$State_of_residence[test2$State_of_residence == "AM"] <- "13"
test2$State_of_residence[test2$State_of_residence == "RR"] <- "14"
test2$State_of_residence[test2$State_of_residence == "PA"] <- "15"
test2$State_of_residence[test2$State_of_residence == "AP"] <- "16"
test2$State_of_residence[test2$State_of_residence == "TO"] <- "17"
test2$State_of_residence[test2$State_of_residence == "MA"] <- "21"
test2$State_of_residence[test2$State_of_residence == "PI"] <- "22"
test2$State_of_residence[test2$State_of_residence == "CE"] <- "23"
test2$State_of_residence[test2$State_of_residence == "RN"] <- "24"
test2$State_of_residence[test2$State_of_residence == "PB"] <- "25"
test2$State_of_residence[test2$State_of_residence == "PE"] <- "26"
test2$State_of_residence[test2$State_of_residence == "AL"] <- "27"
test2$State_of_residence[test2$State_of_residence == "SE"] <- "28"
test2$State_of_residence[test2$State_of_residence == "BA"] <- "29"
test2$State_of_residence[test2$State_of_residence == "MG"] <- "31"
test2$State_of_residence[test2$State_of_residence == "ES"] <- "32"
test2$State_of_residence[test2$State_of_residence == "RJ"] <- "33"
test2$State_of_residence[test2$State_of_residence == "SP"] <- "35"
test2$State_of_residence[test2$State_of_residence == "PR"] <- "41"
test2$State_of_residence[test2$State_of_residence == "SC"] <- "42"
test2$State_of_residence[test2$State_of_residence == "RS"] <- "43"
test2$State_of_residence[test2$State_of_residence == "MS"] <- "50"
test2$State_of_residence[test2$State_of_residence == "MT"] <- "51"
test2$State_of_residence[test2$State_of_residence == "GO"] <- "52"
test2$State_of_residence[test2$State_of_residence == "DF"] <- "53"

test2$State_of_residence <- as.factor(test2$State_of_residence)

###### RACA #### BRANCA=1 # PRETA=2 # PARDA=3 # AMARELA=4 # INDIGENA=5 # SEM INFORMACAO=6 ## 

### train
train2$RACA <- as.character(train2$RACA)

train2$RACA[train2$RACA == "BRANCA"] <- "1"
train2$RACA[train2$RACA == "PRETA"] <- "2"
train2$RACA[train2$RACA == "PARDA"] <- "3"
train2$RACA[train2$RACA == "AMARELA"] <- "4"
train2$RACA[train2$RACA == "INDIGENA"] <- "5"
train2$RACA[train2$RACA == "SEM INFORMACAO"] <- "6"

train2$RACA <- as.factor(train2$RACA)

### test
test2$RACA <- as.character(test2$RACA)

test2$RACA[test2$RACA == "BRANCA"] <- "1"
test2$RACA[test2$RACA == "PRETA"] <- "2"
test2$RACA[test2$RACA == "PARDA"] <- "3"
test2$RACA[test2$RACA == "AMARELA"] <- "4"
test2$RACA[test2$RACA == "INDIGENA"] <- "5"
test2$RACA[test2$RACA == "SEM INFORMACAO"] <- "6"

test2$RACA <- as.factor(test2$RACA)

test2 <- select(test2,-DS_CARATER, -QT_DIARIAS_UI)  
test2 <- select(test2, -QT_DIARIAS_UI)  


train2 <- select(train2,-DS_CARATER, -QT_DIARIAS_UI)
train2 <- select(train2, -QT_DIARIAS_UI)

train$RACA <- as.numeric(train$RACA)
train$VL_PACIENTE_IDADE <- as.numeric(train$VL_PACIENTE_IDADE)
train$QT_PROCEDIMENTO <- as.numeric(train$QT_PROCEDIMENTO)
train$QT_DIARIAS_UTI <- as.numeric(train$QT_DIARIAS_UTI)
train$QT_DIARIAS <- as.numeric(train$QT_DIARIAS)
train$CO_PACIENTE_SEXO <- as.numeric(train$CO_PACIENTE_SEXO)
train$CO_CAR_INTERNACAO <- as.factor(train$CO_CAR_INTERNACAO)
train$Residents <- as.numeric(train$Residents)
train$State_of_residence <- as.numeric(train$State_of_residence)
train$Class_ML <- as.factor(train$Class_ML)


test$RACA <- as.numeric(test$RACA)
test$VL_PACIENTE_IDADE <- as.numeric(test$VL_PACIENTE_IDADE)
test$QT_PROCEDIMENTO <- as.numeric(test$QT_PROCEDIMENTO)
test$QT_DIARIAS_UTI <- as.numeric(test$QT_DIARIAS_UTI)
test$QT_DIARIAS <- as.numeric(test$QT_DIARIAS)
test$CO_PACIENTE_SEXO <- as.numeric(test$CO_PACIENTE_SEXO)
test$CO_CAR_INTERNACAO <- as.factor(test$CO_CAR_INTERNACAO)
test$Residents <- as.numeric(test$Residents)
test$State_of_residence <- as.numeric(test$State_of_residence)
test$Class_ML <- as.factor(test$Class_ML)


set.seed (1)

#### Bernoulli -> classification problem

boost.ML=gbm(Class_ML~. , data=train ,distribution="bernoulli", n.trees=5000, interaction.depth=4)

summary(boost.ML)

par(mfrow=c(1,2)) 
plot(boost.ML ,i="Child_labor") 
plot(boost.ML ,i="NU_PACIENTE_LOGR_MUNICIPIO")

yhat.boost=predict(boost.ML, data=train , n.trees=5000)

mean((yhat.boost - test)^2)

#############
##### Boosting : xgboost
############
install.packages('vcd')

library(vcd)

sparse_matrix <- Matrix::sparse.model.matrix(Class_ML~ ., data = train)[,-1]
head(sparse_matrix)


output_vector = train[,"Class_ML"] == "Marked"


bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)


importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst, data = sparse_matrix, label = output_vector)

xgb.plot.importance(importance_matrix = importance)

model.matrix(~train$CO_PACIENTE_SEXO)

model.matrix(~train$CO_CAR_INTERNACAO)

model.matrix(~train$State_of_residence)


#############
#### Boosting model
############

library(xgboost)
library(knitr)
library(Matrix)
library(dplyr)
library(ggplot2)
install.packages("GGally")
library(GGally)
library(data.table)

train= data.table(train) #convert train set to data table format

test= data.table(test) #convert test set to data table format

sparse.matrix.train= sparse.model.matrix(Class_ML~.-1, data = train) #converts train set factors to columns
sparse.matrix.test=  sparse.model.matrix(Class_ML~.-1, data = test) #converts test set factors to columns

output_vector = train[,Class_ML]=='0' #output vector to be predicted

xgb.model.one=    xgb.cv(data= sparse.matrix.train,     #train sparse matrix 
                         label= output_vector,    #output vector to be predicted 
                         eval.metric= 'logloss',     #model minimizes Root Mean Squared Error
                         objective= "binary:logistic", #binary or regression : "reg:logistic"
                         nfold= 10,
                         #tuning parameters
                         max.depth= 8,            #Vary btwn 3-15
                         eta= 0.1,                #Vary btwn 0.1-0.3
                         nthread = 5,             #Increase this to improve speed
                         subsample= 1,            #Vary btwn 0.8-1
                         colsample_bytree= 0.5,   #Vary btwn 0.3-0.8
                         lambda= 0.5,             #Vary between 0-3
                         alpha= 0.5,              #Vary between 0-3
                         min_child_weight= 3,     #Vary btwn 1-10
                         nround= 100             #Vary btwn 100-3000 based on max.depth, eta,subsample & colsample
)

xgb.model.two=    xgb.cv(data= sparse.matrix.train,     #train sparse matrix 
                         label= output_vector,    #output vector to be predicted 
                         eval.metric= 'logloss',     #model minimizes Root Mean Squared Error
                         objective= "binary:logistic", #binary or regression : "reg:logistic"
                         nfold= 10,
                         #tuning parameters
                         max.depth= 3,            #Vary btwn 3-15
                         eta= 0.05,                #Vary btwn 0.1-0.3
                         nthread = 5,             #Increase this to improve speed
                         subsample= 1,            #Vary btwn 0.8-1
                         colsample_bytree= 0.5,   #Vary btwn 0.3-0.8
                         lambda= 0.5,             #Vary between 0-3
                         alpha= 0.5,              #Vary between 0-3
                         min_child_weight= 3,     #Vary btwn 1-10
                         nround= 100             #Vary btwn 100-3000 based on max.depth, eta,subsample & colsample
)

plot(data.frame(xgb.model.one.Scores)[,2],type='l',col='black',ylab='CV logloss Error',xlab='# of trees')
#lines(data.frame(xgb.model.one.Scores)[,4],type='l',col='blue')
#lines(data.frame(xgb.model.two.Scores)[,4],type='l',col='red')
lines(data.frame(xgb.model.one.Scores.Binary)[,4],type='l',col='blue')
lines(data.frame(xgb.model.two.Scores.Binary)[,4],type='l',col='red')

xgb.model.one.Scores <- read.csv("/Users/mikaellemos/xgb.model.one.Scores.csv") 
xgb.model.two.Scores <- read.csv("/Users/mikaellemos/xgb.model.two.Scores.csv") 

xgb.model.one.Scores.Binary <- read.csv("/Users/mikaellemos/binary_one.csv") 
xgb.model.two.Scores.Binary <- read.csv("/Users/mikaellemos/binary_two.csv") 


print(xgb.model.one)
print(xgb.model.two)


#Save rmse of the last iteration
rmse <- tail(xgb.model.one.Scores$ train_logloss_mean$train_logloss_std$test_logloss_mean$test_logloss_std, 1)

xgb.model.best=   xgboost(data= sparse.matrix.train,     #train sparse matrix 
                          label= output_vector,          #output vector to be predicted 
                          eval.metric= 'logloss',        #model minimizes Root Mean Squared Error
                          objective= "binary:logistic", #binary or regression : "reg:logistic"
                          #tuning parameters
                          max.depth= 8,            #Vary btwn 3-15
                          eta= 0.1,                #Vary btwn 0.1-0.3
                          nthread = 5,             #Increase this to improve speed
                          subsample= 1,            #Vary btwn 0.8-1
                          colsample_bytree= 0.5,   #Vary btwn 0.3-0.8
                          lambda= 0.5,             #Vary between 0-3
                          alpha= 0.5,              #Vary between 0-3
                          min_child_weight= 3,     #Vary btwn 1-10
                          nround= 30               #Vary btwn 100-3000 based on max.depth, eta, subsample and               colsample
)                                   

importance = xgb.importance(feature_names = sparse.matrix.train@Dimnames[[2]], 
                            model = xgb.model.best)  #Grab all important features
xgb.plot.importance(importance[1:10])  #Plot for top 10 important features

#check missing values 
sapply(train, function(x) sum(is.na(x))/length(x))*100
sapply(test, function(x) sum(is.na(x))/length(x))*100


#########
### xgboost : boosting new model and evaluation
#########

train$CO_CAR_INTERNACAO <- as.factor(train$CO_CAR_INTERNACAO)
test$CO_CAR_INTERNACAO <- as.factor(test$CO_CAR_INTERNACAO)


#convert data frame to data table
setDT(train2) 
setDT(test2)

#using one hot encoding 
labels <- train2$Class_ML 
ts_label <- test2$Class_ML
new_tr <- model.matrix(~.+0,data = train2[,-c("Class_ML"),with=0]) 
new_ts <- model.matrix(~.+0,data = test2[,-c("Class_ML"),with=0])

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label= ts_label)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
### Best iteraction 10

min(xgbcv$test.error.mean)

#first default - model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 2000, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- predict (xgb1,dtrain)

xgbpred <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix
install.packages("caret")
library(caret)

 table(xgbpred, ts_label)

 (1540+84)/(1540+84+70+283) # Accuracy
 
 (1540)/(1540+283) # Recall
 
 (1540)/(1540+70) # Precision
 
 
#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 




