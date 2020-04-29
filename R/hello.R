#' Greeting
#' @param name
#' write your name here!
#'
#' @return
#'
#' @export
#'
hello <- function(name) {
  if(missing(name)) name=""
  message(paste0("Hello",if(name!="")" ",name,"!\nMy name is Imaduddin Haetami, I am the creator of this package!\nHope that you find it useful <3"))
}
