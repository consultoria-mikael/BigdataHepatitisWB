########Random Forest##############
###################################


### Installing Packages

### ISLR : test data for Random Forest 
install.packages("ISLR")
install.packages("randomForest")
install.packages("tree")

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

########
## Classification trees : bagging, random forest, boosting
########


##Random Sampling of a df 
ML_TABELA_SOC.sample <- ML_TABELA_SOC[sample(nrow(ML_TABELA_SOC), 1000), ]    ## 1000 lines, random

##### Select variables

ML_TABELA_SOC.sample_var <- select(ML_TABELA_SOC.sample, NU_PACIENTE_LOGR_MUNICIPIO, QT_PROCEDIMENTO, QT_DIARIAS, QT_DIARIAS_UTI, CO_PACIENTE_SEXO, RACA, NU_MUN_HOSP, CO_PROCEDIMENTO_PRINCIPAL, COD_PROCEDIMENTO_SECUNDARIO, CO_CNES, CO_CAR_INTERNACAO, CO_MOT_SAIDA, -ATESTADO, Fridge, Computer_ownership, Whites, Gini_coefficient, Class_ML)

set.seed(101)
train = sample(1:nrow(ML_TABELA_SOC.sample_var), 500)

bag.ML=randomForest(Class_ML~. , data = ML_TABELA_SOC.sample_var,subset=train, mtry=16,importance=TRUE, prOximity=TRUE, na.action=na.roughfix)

ML_TABELA_SOC.sample_var.test=ML_TABELA_SOC.sample_var[-train ,"Class_ML"]

yhat.bag = predict(bag.ML ,newdata=ML_TABELA_SOC.sample_var[-train ,])

plot(yhat.bag, ML_TABELA_SOC.sample_var.test)
abline(0,1)

mean((yhat.bag - ML_TABELA_SOC.sample_var.test)^2)

set.seed (1)

rf.ML_TABELA_SOC.sample_var=randomForest(Class_ML~.,data=ML_TABELA_SOC.sample_var,subset=train, mtry=6,importance =TRUE, prOximity=TRUE, na.action=na.roughfix)

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


######
## Classification trees
######

tree.ML =tree(as.factor(Class_ML)~. ,ML_TABELA_SOC.sample_var)

summary(tree.ML)

plot(tree.ML)
text(tree.ML ,pretty =0)

tree.ML

set.seed (2)

train=sample(1:nrow(ML_TABELA_SOC.sample_var), 200)

ML.test=ML_TABELA_SOC.sample_var [-train ,]

Class_ML.test=ML_TABELA_SOC.sample_var$Class_ML[-train]

tree.ML=tree(as.factor(Class_ML)~. ,ML_TABELA_SOC.sample_var)

tree.pred=predict(tree.ML, ML.test,type="class")

table(tree.pred, Class_ML.test)

## accurcy

(666+134)/800

## For this case acutacy of 100%

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


prune.ML=prune.misclass(tree.ML,best=2)

plot(prune.ML )
text(prune.ML,pretty=0)

### Accuracy prune tree

tree.pred=predict(prune.ML,ML.test,type="class")

table(tree.pred ,Class_ML.test)

######
### Classification tree : rpart package
######

library("rpart")

class_ML_rpart <- rpart(Class_ML~ CO_MOT_SAIDA+CO_PROCEDIMENTO_PRINCIPAL+CO_PACIENTE_SEXO+COD_PROCEDIMENTO_SECUNDARIO+QT_DIARIAS+QT_DIARIAS_UTI ,method="class", data=ML_TABELA_SOC.sample_var)

class_ML_rpart <- rpart(Class_ML~. ,method="class", data=ML_TABELA_SOC.sample_var)

class_ML_rpart <- rpart(Class_ML~ CO_MOT_SAIDA+CO_PROCEDIMENTO_PRINCIPAL,method="class", data=ML_TABELA_SOC.sample_var)


printcp(class_ML_rpart) # display the results 
plotcp(class_ML_rpart) # visualize cross-validation results 
summary(class_ML_rpart) # detailed summary of splits

# plot tree 
plot(class_ML_rpart, uniform=TRUE, main= "Classification Tree for medical outcome")
text(class_ML_rpart, use.n=TRUE, all=TRUE, cex=.8)


post(class_ML_rpart, file = "/Volumes/Mikael_backup3/PROJETO_BDBM/PLOTS/tree_rpart.ps", 
     title = "Classification Tree for hepatitis medical outcome")

install.packages("party")

#### prune tree

# prune the tree 
pclass_ML_rpart<- prune(class_ML_rpart, cp=   class_ML_rpart$cptable[which.min(class_ML_rpart$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pclass_ML_rpart, uniform=TRUE, 
     main="Pruned Classification Tree for hepatitis medical outcome")
text(pclass_ML_rpart, use.n=TRUE, all=TRUE, cex=.8)

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

sample <- sample.split(ML_TABELA_SOC.sample_var$Class_ML, SplitRatio = 0.7) ## obs: here add variable to be predicted
## obs2:  This is only a test

######
## Train
#####

train <- subset(ML_TABELA_SOC.sample_var, sample == T)

######
## Test
######

test <- subset(ML_TABELA_SOC.sample_var, sample == F)


######
## Random Forest model
######

rf.model <- randomForest(Class_ML~. , data = train, importance=TRUE)

ML_TABELA_SOC.sample_var <- na.omit(ML_TABELA_SOC.sample_var)
train <- na.omit(train)
test <- na.omit(test)


######
## Confusion Matrix
######

print.table(rf.model$confusion)

######
## Importance : Read ISLR to tree methods
######

rf.model$importance

######
## Predictions
######

rf.preeds <- predict(rf.model, test)

######
## Table
#####

table(rf.preeds, test$Class_ML)

#######
## Logistic Regression Model
#######

##### TRAIN/TEST SPLIT ######


#set.seed(101)

sample <- sample.split(ML_TABELA_SOC.sample_var$Class_ML, SplitRatio = 0.7) ## obs: here add variable to be predicted
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


accuracy <- (226+32)/(226+21+19+32)

# recall
recall <- 226/(226+21)

# precision
precision <- 226/(226+19)

