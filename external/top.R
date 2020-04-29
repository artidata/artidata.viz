#' Title
#'
#' @param mat a matrix or data.frame
#' @param n Number of rows and columns to display
#'
#' @return NULL
#' @export
#' @seealso \code{\link[utils]{head}}
#' @examples \dontrun{
#' }
#'
#' x=matrix(rnorm(100),ncol = 10)
#' top(x)
top = function(mat,n=5){
  print(mat[1:n,1:n])
  return(NULL)
}
