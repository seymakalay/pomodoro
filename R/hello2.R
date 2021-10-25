#' My Hello2 Word Function
#'
#' @param x The name of the person to say hi to.
#'
#' @return The output from  \code{\link{print}}
#' @export
#'
#' @examples
#' hello2("Seyma")
#' \dontrun{
#' hello2("Steve")
#' }
hello2 <- function(x) {
  print(paste0("Hello2 ", x, ",this is the world!"))
}
