#' @title Calculate inner cvAUC and outer cvAUC and display in dataframe
#'
#' @description This function calculates the cvAUC of the inner loop and outer loop as well as the respective
#' 95% confidence intervals.
#' @param modelList list containing the results from the modelling process obtained with [ml_eval()].
#' @param train.cv name of the object containing cvAUC results of the training process. Character string.
#' @param rep.outer number of repeats in the outer loop
#' @export

rbind_model_ci <- function(modelList, train.cv = "train.cv", rep.outer){

  ls <- ls_cvAUC(modelList)
  out <- cvAUC(ls$predictions, ls$labels)
  res <-  ci.cvAUC(ls$predictions, ls$labels)

  rbind(cv.AUC.inner = data.frame(cvAUC = mean(unlist_model(modelList, "cvAUC", train.cv, rep.outer)),
                                  lower = mean(unlist_model(modelList, "lower", train.cv, rep.outer)),
                                  upper = mean(unlist_model(modelList, "upper", train.cv, rep.outer))),
        cv.AUC.outer = data.frame(cvAUC = res$cvAUC,
                                  lower = min(res$ci),
                                  upper = max(res$ci)
        )
  )
}
