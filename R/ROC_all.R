
### Class tree
tree.probs=predict(class_ML_rpart,
                   newdata=train,
                   type="prob")
head(tree.probs)

train$CO_CAR_INTERNACAO <- as.factor(train$CO_CAR_INTERNACAO)

test$CO_CAR_INTERNACAO <- as.factor(test$CO_CAR_INTERNACAO)


#Calculate ROC curve
rocCurve.tree <- roc(train$Class_ML,tree.probs[,"yes"])
#plot the ROC curve
plot(rocCurve.tree,col=c(4), print.auc=TRUE, add=TRUE)

### RF

rf.probs <-  predict(rf.model, 
                         newdata = test,
                         type="prob")

rf.probs <-  predict(rf.model, 
                     newdata = train,
                     type="prob")

head(rf.probs)

rocCurve.rf <- roc(test$Class_ML,rf.probs[,"yes"])

rocCurve.rf <- roc(train$Class_ML,rf.probs[,"yes"])

#plot the ROC curve
plot(rocCurve.rf, col=c(1)) #, print.auc=TRUE, add=TRUE)

######### LR
lr.probs <-  predict(model, 
                     newdata = train,
                     type="response")

train$prob=prob

prob=predict(model,type=c("response"))

lr <- roc(Class_ML ~ prob, data = train)

plot(lr, print.auc=TRUE, add=TRUE)

###### boosting


gbm.probs=predict(boost.ML,
                  newdata=test,
                  type="prob")



predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}


train.gbm <- train(as.factor(Class_ML) ~ ., 
                   data=train,
                   method="gbm",
                   verbose=F,
                   trControl=cvcontrol)

install.packages("stats4", dependencies=TRUE)

plot(rocCurve.tree,col=c(4))
#plot(rocCurve.bagg,add=TRUE,col=c(6)) # color magenta is bagg
plot(rocCurve.rf,add=TRUE,col=c(1)) # color black is rf
#plot(rocCurve.cf,add=TRUE,col=c(2)) # color red is cforest
#plot(rocCurve.gbm,add=TRUE,col=c(3)) # color green is boosting
par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:


roc_test <- roc(ts_label, xgbpred, algorithm = 2)

roc_train <- roc(labels, xgbpred, algorithm = 2)

plot(roc_train) # print.auc=TRUE, add=TRUE )
lines(roc_train, col="red")

lines(lr, col="blue")
lines(rocCurve.tree, col="green")


library(pROC) 

