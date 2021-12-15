
#' Title
#'
#' @param Data The name of the Dataset.
#' @param xvar X variables
#' @param yvar Y variable
#'
#' @return The output from  \code{\link{RF_Model}}
#' @export
#' @importFrom  pROC multiclass.roc
#' @examples
#' yvar <- c("Loan.Type")
#' m2.xvar0 <- c("sex", "married", "age", "havejob", "educ", "rural", "region","income")
#' m2.xvar1 <- c(m2.xvar0, "networth")
#' BchMk.MLR.1 <- RF_Model(sample_data, c(m2.xvar0, "political.afl", "networth"), yvar)
#' BchMk.MLR.1$finalModel
#' BchMk.MLR.1$Roc$auc




#'
RF_Model <- function(Data, xvar, yvar){

  #I ADDED THIS IF BUT IT GIVES ERROR
  if (yvar == "Loan.Type"){
    Data.sub <- Data[, c(xvar, yvar)]
    Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "No.Loan", "Formal",  "Informal", "L.Both"))
  } else if(yvar == "multi.level"){
    Data.sub <- Data[, c(xvar, yvar)]
  }

  set.seed(87)
  train.set <- createDataPartition(Data.sub[, yvar], p = .80, list = 0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ ,xvar]
  X.test  <- Data.sub.test[ ,xvar]
  Y.train <- Data.sub.train[ ,yvar]
  Y.test  <- Data.sub.test[ ,yvar]

  # Fit the model
  myControl <- trainControl("cv", 10,  verboseIter = TRUE) #classProbs = TRUE, savePredictions=T)  #"cv" = cross-validation, 10-fold

  Model.RF <- train(x = X.train, y = Y.train,  method = "rf",  trControl = myControl,
                    preProcess = c("center", "scale"))

  RF.fit <- randomForest::randomForest(Data.sub.train[,yvar] ~ ., data = Data.sub.train) #, family=binomial(link="logit"))

  Model.RF$finalModel$call <- RF.fit$call

  # Make predictions
  Pred.prob <- predict(Model.RF, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  Model.RF$Pred_prob <- Pred.prob
  Model.RF$Actual <- as.matrix(Y.test)
  Model.RF$Roc <- Roc

  ### Confusion Matrix
  Model.RF$Predicted_class <- predict(Model.RF, newdata = Data.sub.test)
  Model.RF$ConfMat <- table(Model.RF$Predicted_class, Y.test)

  # Model accuracy
  Model.RF$ACC <- mean(Model.RF$Predicted_class == Y.test, na.rm=T)
  return(Model.RF)
}
