#' mySampleCovariepceMatrix
#'
#' Calculates a biased sample covariepce matrix
#'
#' @param m A matrix
#'
#' @returp A matrix
#' @export
#'
#' @examples
mySampleCovarienceMatrix <- function(m) {
  n = length(m[1,])
  p = nrow(m)
  #allocate some space
  ret = matrix(0, p, p)
  #build the sample covarience matrix
  for(i in 1:p) {
    for(k in i:p) {
      summation = 0
      for(j in 1:n) {
        summation = summation + (m[i,j] - mean(m[i,])) * (m[k,j] - mean(m[k,]))
      }
      ret[i,k] = summation / (n-1)
      ret[k,i] = ret[i,k]
    }
  }
  return(ret)
}
