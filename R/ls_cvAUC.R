#' @title Prepare data for input into [cvAUC::cvAUC()]
#'
#' @description Function to split data in a way that the function cvAUC can use it for calculation of ROC, se and confidence intervals
#' @param modelList containing the results from the modelling process obtained with [ml_eval()]
#' @export

ls_cvAUC <- function(modelList){
  predictions <- lapply(1:10, function(d){
    tmp <- sapply(modelList[[d]], "[", "predictions")
    lapply(1:10, function(x){
      1-tmp[[x]]$pred.ja
    })
  })


  labels <- lapply(1:10, function(d){
    labs <- sapply(modelList[[d]], "[", "predictions")
    lapply(1:10, function(x){
      labs[[x]]$obs
    })
  })

  list(predictions = lapply(rapply(predictions, enquote, how="unlist"), eval),
       labels = lapply(rapply(labels, enquote, how="unlist"), eval))
}
