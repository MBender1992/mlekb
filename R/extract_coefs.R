#' @title Extract coefficients from model
#'
#' @description Extracts the name of coefficients that are present in each iteration of k-fold cross validation to get
#' statistics of feature importance. Parameters which are included in many of the different splits are considered
#' important features.
#' @param modelList containing the results from the modelling process obtained with [ml_eval()]
#' @export

extract_coefs <- function(modelList){
  lapply(1:10, function(x){
    tmp <- unlist(sapply(sapply(modelList[[x]], '[', 'coefficients'), '[', 'coefs'))
    data.frame(coef = tmp)
  })
}


