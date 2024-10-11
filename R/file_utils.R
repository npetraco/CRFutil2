#' @title       Function to download script from the Notes
#' @description The function will download script from the Notes
#'
#' @param script_name Name of the script in the Notes
#' @param url_head Location of the script (optional). If NULL, default location is used.
#' @param download_location Where to download the script. If NULL, default location is used.
#'
#' @details The function will download script from the Notes. Default url_head is currently: XXXX.
#' Default download_location is currently: XXXX.
#'
#' @return The feature function, which is a vector.
#'
#' @examples XX
#'
#' @export
get_script <- function(script_name, url_head=NULL, download_location=NULL) {

  if(is.null(url_head)) {
    url_head.loc <- "https://raw.githubusercontent.com/npetraco/CRFutil2/refs/heads/main/inst/doc/Notes_scripts_bank"
  } else {
    url_head.loc <- url_head
  }

  if(is.null(download_location)) {
    download_location.loc <- tempdir()
  } else {
    download_location.loc <- download_location
  }

  url_loc <- paste0(url_head.loc, "/", script.name)
  #print(url_loc)

  file_loc <- paste0(download_location.loc,"/",script.name)
  #print(file_loc)

  download.file(url_loc, file_loc)
  print("File downloaded to:")
  print(file_loc)
  file.edit(file_loc)

}
