#' @title Calculates cross validated AUC with 95% confidence intervals for a given caret model object
#'
#' @param model model object obtained with [caret::train()].
#' @export

ci_cv_AUC <- function(model){
  lambda <- NULL
  dat <- dplyr::filter(model$pred, lambda == model$finalModel$lambdaOpt)

  obs <- dat$obs
  event_name <- levels(obs)[2]
  pred <- dat[[event_name]]

  obs <- split(obs, f = dat$Resample)
  pred <- split(pred, f = dat$Resample)

  ci.cvAUC(pred, obs)
}


