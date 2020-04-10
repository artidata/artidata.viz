#' Title
#' @param name
#' @return
#' @export
#'
#' @examples
hello <- function(name) {
  print(paste0("hello",if(name!="")" ",name,"!"))
}
