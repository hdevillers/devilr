#' Compute predictive model statistics considering two classes
#'
#' @param pred predicted values
#' @param expect expected values
#' @param positive value of the positive class
#'
#' @return an array of descriptive statistics
#' @export
binary_model_stats <- function(pred, expect, positive = 1) {
  # Compute TP TN FP FN
  N <- length(pred)
  is.pred.pos <- pred == positive
  is.expext.pos <- expect == positive
  TP <- sum( is.pred.pos & is.expext.pos )
  TN <- sum( !is.pred.pos & !is.expext.pos )
  FP <- sum( is.pred.pos & !is.expext.pos )
  FN <- sum( !is.pred.pos & is.expext.pos )

  # Specificity (TNR, selectivity)
  TNR <- TN / (TN + FP)
  # Sensitivity (TPR, recall)
  TPR <- TP / (TP + FN)
  # False negative rate
  FNR <- FN / (TP + FN)
  # False positive rate
  FPR <- FP / (FP + TN)
  # Positive predicted value
  PPV <- TP / (TP + FP)
  # False discovery rate
  FDR <- FP / (TP + FP)
  # False omission rate
  FOR <- FN / (FN + TN)
  # Negative predictive value
  NPV <- TN / (FN + TN)

  return(c(
    TP = TP, TN = TN, FP = FP, FN =FN,
    specificity = TNR,
    sensitivity = TPR,
    FNR = FNR,
    FPR = FPR,
    PPV = PPV,
    FDR = FDR,
    FOR = FOR,
    NPV = NPV,
    accuracy = (TP + TN) / N,
    balanced.accuracy = (TPR + TNR) / 2,
    F1.score = 2 * TP / (2*TP + FP + FN),
    MCC.v2 = sqrt(TPR)*sqrt(TNR)*sqrt(PPV)*sqrt(NPV) - sqrt(FNR)*sqrt(FPR)*sqrt(FOR)*sqrt(FDR),
    MCC.v1 = (TP * TN - FP * FN) / ( sqrt(TP+FP) * sqrt(TP+FN) * sqrt(TN+FP) * sqrt(TN+FN) ),
    G.mean = sqrt( TP/(TP+FN) * TN / (TN +FP)),
    dominance = TPR - TNR,
    prevalence = (TP + FN) / N,
    markedness = PPV + NPV - 1,
    Fowmles.Mallows.index = sqrt(PPV) * sqrt(TPR),
    Jaccard.index = TP / (TP + FN + FP),
    informedness = TPR + TNR - 1,
    prevalence.threshold = (sqrt(TPR)*sqrt(FPR) - FPR) / (TPR - FPR)
  ))
}
