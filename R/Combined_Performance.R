

#' Title
#'
#' @param Sub.Est.Mdls
#'
#' @return
#' @export
#'
#' @examples
#' CCP.RF.1  <- ESTIMATE_MODELS(sample_data, yvar=c("Loan.Type"), exog = "political.afl", xvec=m2.xvar0, xadd="networth", type="RF", dnames=c("0","1"))
#' CCP.GLM.1  <- ESTIMATE_MODELS(CCP, yvar=c("Loan.Type"), exog = "political.afl", m2.xvar0, xadd="networth", type="GLM", dnames=c(0,1))
#'
#' CCP.MLR.1 <-  RF_Model(CCP, m2.xvar1, yvar)
#' NoCCP.MLR.1 <-  RF_Model(NoCCP, m2.xvar1, yvar)
#' Sub.CCP.MLR.RF <- list(Mdl.1 = CCP.MLR.1,  Mdl.2= NoCCP.MLR.1  )




Combined_Performance <- function(Sub.Est.Mdls)
{
  nmdls <- length(Sub.Est.Mdls)
  Pred_prob <- NULL
  for (ii in 1:nmdls) {
    Pred_prob <- rbind(Pred_prob, Sub.Est.Mdls[[ii]]$Pred_prob)
  }

  Actual <- NULL
  for (ii in 1:nmdls) {
    Actual <- as.matrix( rbind(Actual, Sub.Est.Mdls[[ii]]$Actual) )
  }

  Combine.Mdl <- list(Pred_prob = Pred_prob)
  Combine.Mdl$Actual <- factor(Actual)

  Combine.Mdl$Roc <- multiclass.roc(Combine.Mdl$Actual, Pred_prob )

  ### Confusion Matrix
  Predicted_class <- NULL
  for (ii in 1:nmdls) {
    Predicted_class <-  rbind( Predicted_class, as.matrix(Sub.Est.Mdls[[ii]]$Predicted_class))
  }

  Combine.Mdl$Predicted_class <- Predicted_class
  Combine.Mdl$ConfMat <- table(Combine.Mdl$Predicted_class, Combine.Mdl$Actual)

  # Model accuracy
  Combine.Mdl$ACC <- mean(Combine.Mdl$Predicted_class == Combine.Mdl$Actual, na.rm=T)

  return(Combine.Mdl)
}

