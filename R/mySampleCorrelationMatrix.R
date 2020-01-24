#' mySampleCorrelationMatrix
#'
#' Calculates the Sample Correlation Matrix
#'
#' @param m A matrix
#'
#' @return A matrix
#' @export
#'
#' @examples
mySampleCorrelationMatrix <- function(m) {
  #sequence for 1 to n
  j = seq(1, length(m[1,]), 1)
  p = nrow(m)
  #allocate some space
  ret = matrix(0, p, p)
  #build the sample covariepce matrix
  for(i in 1:p) {
    for(k in 1:p) {
      Sik = sum((m[i,j] - mean(m[i,])) * (m[k,j] - mean(m[k,])))
      Sii = sum((m[i,j] - mean(m[i,])) ^ 2)
      Skk = sum((m[k,j] - mean(m[k,])) ^ 2)
      ret[i,k] = Sik/sqrt(Sii * Skk)
      ret[k,i] = ret[i,k]
    }
  }
  return(ret)
}
