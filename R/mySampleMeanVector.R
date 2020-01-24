#' mySampleMeanVector
#'
#' Calculate the sample mean vector from a matrix
#'
#' @param x A matrix
#'
#' @return A vector
#' @export
#'
#' @examples
mySampleMeanVector <- function(x) {
  v <- 1:nrow(x)
  for(j in 1:nrow(x)) {
    v[j] = sum(x[j,])/length(x[j,])
  }
  return(v)
}
