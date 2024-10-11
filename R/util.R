#' @title Easily update the CRFutil2 library by installing the current version from the github site
#' @description Easily update the CRFutil2 library by installing the current version from the github site
#'
#' @param XX  XX
#'
#' @details Easily update the CRFutil2 library by installing the current version from the github site
#'
#' @return The function will XX
#'
#' @examples XX
#'
#' @export
update_CRFutil2 <- function() {
  print("Updating CRFutil2")
  remotes::install_github("npetraco/CRFutil2")
  print("Done!!")
}
