#' @title Train caret model and extract model results
#'
#' This function trains a model using the trainControl and train function included in \pkg{caret}. Afterwards several metrics are extracted and stored in a list.
#' Stored objects are metrics obtained in the training process (ROC or Accuracy, Hyperparameters, etc.), coefficients of the trained model,
#' results of the cross validation process, predictions on a test set which should be defined with the [caret::createMultiFolds()] function. Accepted train methods are the methods
#' listed in the \pkg{caret} documentation.
#' @param x.train subset of data used for training.
#' @param y.train outcome variable used to compare predictions obtained with the trained model. Data has to split the same way as x.train.
#' Preferred ways of splitting are the [createMultiFolds()] and [createFolds()] functions included in the \pkg{caret} package.
#' @param x.test subset of data model is applied to, to test performance.
#' @param y.test outcome variable corresponding to the x.test split.
#' @param train.method A string specifying which classification or regression model to use. Possible values are found using names(caret::getModelInfo())
#' @param cv.method The resampling method: "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" (for repeated training/test splits), "none" (only fits one model to the entire training set), "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models), timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"
#' @inheritParams caret::trainControl
#' @inheritParams caret::train
#' @param ... additional parameters passed on to [caret::train()] function.
#' @inheritParams pROC::roc


calc_model_metrics <- function(x.train, y.train, x.test, y.test, train.method = "glmnet",summaryFunction = twoClassSummary, savePredictions = TRUE,
                               returnResamp = "all", classProbs=TRUE,  cv.method = "repeatedcv", number = 10, repeats = 5, metric = "ROC", direction = ">",
                               levels = c("no", "yes"), preProcess = c("center","scale"), tuneGrid = NULL, ...){

  # define ctrl function
  cctrl1 <- trainControl(method=cv.method, number=number,repeats = repeats, returnResamp = returnResamp,savePredictions = savePredictions,
                         classProbs=classProbs, summaryFunction=summaryFunction)

  # run glmnet model
  md <- train(x.train, y.train, method = train.method, preProcess = preProcess,
              trControl = cctrl1,metric = metric, tuneGrid = tuneGrid, ...)

  # obtain cv AUC of training folds
  ci_cv <- ci.cv.AUC(md)

  # train coefs
  feat <- coef(md$finalModel, md$finalModel$lambdaOpt)

  # obtain index from max metric
  opt <- md$results[which(md$results$lambda == md$finalModel$lambdaOpt),]

  # predict
  pred <- predict(md, x.test, type="prob")

  # object to return
  res <- list(
    predictions = data.frame(pred.ja = pred$yes, obs = y.test),
    coefficients = rownames_to_column(data.frame(vals = feat[feat[,1] != 0, 1][-1]),"coefs"),
    train.metrics = opt[which(opt$ROC == max(opt$ROC)),],
    train.cv = data.frame(cvAUC = ci_cv$cvAUC,
                          se = ci_cv$se,
                          lower = ci_cv$ci[1],
                          upper = ci_cv$ci[2]),
    test.metrics = data.frame(AUC = auc(roc(y.test, pred[,1], direction = direction, levels = c("no", "yes"))),
                              Sens = sensitivity(y.test, predict(md, x.test, type="raw"))  ,
                              Spec = specificity(y.test, predict(md, x.test, type="raw")))
  )
  return(res)
}
