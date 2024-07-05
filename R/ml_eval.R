#' @title Conduct nested cv using the calc_model_metrics function in this package
#'
#' @description This function takes a model Matrix as input and splits it into a defined number of folds with a specified number of repetitions.
#' Afterwards a nested cv is conducted with the \code{calc_model_metrics} function contained in this package. The
#' results are given as a list of the training process, model coefficients and model evaluation. To accelerate the modelling
#' process parallel computing is activated.
#' @param modelMatrix a model matrix containing the predictor variables. Preferably obtained through the [stats::model.matrix()] function.
#' @param y vector containing outcome variable.
#' @param k.outer define number of folds for k-fold cross validation in the outer loop
#' @param rep.outer for repeated k-fold cross-validation only: the number of complete sets of folds to compute in the outer loop
#' @param k.inner number of folds for k-fold cross validation in the inner loop
#' @param rep.inner number of repeats for repeated k-fold cross validation in the inner loop
#' @param train.method A string specifying which classification or regression model to use. Possible values are found using names(caret::getModelInfo()).
#' @param cv.method The resampling method: "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" (for repeated training/test splits), "none" (only fits one model to the entire training set), "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models), timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV".
#' @inheritParams caret::train
#' @param ... additional arguments passed on to function [calc_model_metrics()]
#'
#' @seealso [calc_model_metrics()] for more arguments.
#' @export

# models a function based on a presepcified model and evaluates training and test test using ROC, Sensitivity and Specificity
ml_eval <- function(modelMatrix, y, k.outer = 10, rep.outer = 10, k.inner = 10, rep.inner = 5,
                   train.method = "glmnet", cv.method = "repeatedcv", metric = "ROC",
                   tuneGrid = expand.grid(alpha = 1, lambda = seq(0.01,0.2,by = 0.01)), ...){
  # define model matrix with selected features
  x <- modelMatrix

  # activate parallel computing
  cl <- parallel::makeCluster(parallel::detectCores(), type='PSOCK')
  registerDoParallel(cl)

  # generate k folds for outer loop
  set.seed(12)
  fold.train <- createMultiFolds(y, k = k.outer, times = rep.outer) # ensure that at least 10 samples are in each fold

  # split data based on these folds (Fold1 means that Fold1 is used for testing)
  train.test.folds <- lapply(c(1:10), function(split){

    # select only folds containing the specified repeat in each iteration
    if(split == 10){
      ind <- names(fold.train) %>% stringr::str_detect("Rep10")
      dat <- fold.train[ind]
    } else {
      ind <- names(fold.train) %>% stringr::str_detect(paste("Rep0",split, sep =""))
      dat <- fold.train[ind]
    }

    # split data into training and test set with each fold being the test set once
    res <- lapply(c(1:k.outer), function(fold){
      list(x.test = x[-dat[[fold]],],
           x.train = x[dat[[fold]],],
           y.test = y[-dat[[fold]]],
           y.train = y[dat[[fold]]]
      )
    })
    return(res)
  })

  # define name of the list elements
  reps <- paste0("Rep", 1:rep.outer)
  folds <- paste0("Fold", 1:k.outer)
  train.test.folds <- setNames(lapply(train.test.folds, setNames, folds), reps)

  set.seed(849)
  results <- lapply(c(1:rep.outer), function(split){
    # select Data from 1 repeat
    dat <- train.test.folds[[paste("Rep",1, sep ="")]]
    # print message to follow progress
    message(paste("Starting calculation of Rep", split,"... of", rep.outer))
    # apply model to all folds of that 1 repeat and test against the remaining fold not used for training
    res <- pblapply(c(1:k.outer), function(fold){
      mlekb::calc_model_metrics(x.train = dat[[fold]]$x.train, y.train = dat[[fold]]$y.train, x.test =dat[[fold]]$x.test,
                         y.test = dat[[fold]]$y.test, number = k.inner, repeats = rep.inner, metric = metric,
                         train.method = train.method, cv.method = cv.method, tuneGrid = tuneGrid, ... )
    })
  })
  return(results)
  parallel::stopCluster(cl)
}


