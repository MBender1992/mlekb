#' @title convert metrics from training data list to vector
#'
#' @description This function takes the results generated with the [ml_eval()] function and converts the results list
#' into a vector depending on the metric and list element.
#' @param modelList model results generated with [ml_eval()]. List object.
#' @param metric parameter to extract from the modelList.
#' @param element list element from which the metric parameter will be extracted.
#' @param rep.outer number of repeats in the outer loop
#' @export

unlist_model <- function(modelList, metric, element, rep.outer){
  ls <- lapply(1:rep.outer, function(split){
    do.call(rbind.data.frame, sapply(modelList[[split]], '[', element))
  })
  unlist(sapply(ls, "[", metric))
}
