#' @title       Simulate a sample
#' @description Wrapper for CRF simulation functions.
#'
#' @param crf               a crf object
#' @param size              number of samples
#' @param crf.sample.method sampling method in CRF: sample.exact, .junction, .conditional, .cutset
#' @param dress.sampleQ     use the default state names from the CRF sampler or dress up with the state names stipulated in the crf object?
#' @param data.frameQ       whether or not to return sample as a data frame
#' @param ...               other arguments needed for CRF sampling methods
#'
#' @details The function is a wrapper for the CRF simulation functions which knits them all
#' into one function and allows the user to change the default state names (1,2,...etc) into
#' whatever their state names are actually intended to be.
#'
#' @return A sample of configurations.
#'
#' @examples XXXX
#'
#' @export
sample.crf <- function(crf, size, crf.sample.method, dress.sampleQ=T, data.frameQ=T, ...) {

  a.samp <- crf.sample.method(crf, size, ...)

  if(dress.sampleQ == T){
    a.samp <- dress.sample(crf, samples = a.samp)
  }

  if(data.frameQ==T){
    a.samp <- data.frame(a.samp)
  }

  return(a.samp)

}
