#' GLM Function
#'
#' @param Data
#' @param xvar
#' @param yvar
#'
#' @return
#' @export
#' @importFrom  caret createDataPartition
#' @importFrom  caret trainControl
#' @importFrom  caret train
#' @importFrom  pROC multiclass.roc
#'
#' @examples
#'
GLM_Logit_Model <- function(Data, xvar, yvar){

  set.seed(87)
  Data <- as.data.frame(Data)
  Data.sub <- Data[, c(xvar, yvar)]
  #Data.sub[, yvar] <- factor(Data.sub[, yvar], levels = c( "zero", "one"))

  #train.set <- createDataPartition(Data.sub$Credit.Access, p=.80,list=0)
  train.set <- createDataPartition(Data.sub[, yvar], p=.80,list=0)
  Data.sub.train <- Data.sub[ train.set, ]
  Data.sub.test  <- Data.sub[-train.set, ]

  X.train <- Data.sub.train[ , xvar]
  X.test  <- Data.sub.test[, xvar]
  Y.train <- Data.sub.train[ , yvar]
  Y.test  <- Data.sub.test[, yvar]
  #Y.train <- Data.sub.train$Credit.Access
  #Y.test  <- Data.sub.test$Credit.Access

  myControl <- trainControl(method = "cv", number=10, summaryFunction = twoClassSummary, classProbs = TRUE,  verboseIter = TRUE)


  Est.GLM <- train(x=X.train, y=Y.train,  method = "glm", family=binomial, trControl = myControl,
                   preProcess = c("center", "scale"))

  #glm.fit <- glm(Credit.Access ~ ., data = Data.sub.train, family=binomial(link="logit"))

  glm.fit <- glm( Credit.Access   ~ ., data = Data.sub.train, family=binomial(link="logit"))

  Est.GLM$finalModel$call <- glm.fit$call

  z <- summary(Est.GLM)$coefficients/summary(Est.GLM)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1))*2

  Est.GLM$z <- z
  Est.GLM$pval <- p

  # Make predictions
  Pred.prob <- predict(Est.GLM, newdata = Data.sub.test, type = "prob")
  Roc  <- multiclass.roc(Y.test, Pred.prob)

  #colMeans(colAUC(Pred.prob, Data.sub.test[["multi.level"]]))

  Est.GLM$Pred_prob <- Pred.prob
  Est.GLM$Actual <- as.matrix(Y.test)
  Est.GLM$Roc <- Roc

  ### Confusion Matrix
  Est.GLM$Predicted_class <- predict(Est.GLM, newdata = Data.sub.test)
  Est.GLM$ConfMat <- table(Est.GLM$Predicted_class, Y.test)

  # Model accuracy
  Est.GLM$ACC <- mean(Est.GLM$Predicted_class == Y.test, na.rm=T)

  return(Est.GLM)
 }


