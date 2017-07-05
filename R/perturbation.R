#' Temperature field in to LZ temperature field
#'
#' Temperature field in to LZ temperature field
#' @param temp temperature field
#' @param modelPert  list(ditr,param)
#' @keywords tempLZ
#' @export
#' @examples
#' \dontrun{
#' perturbation()
#' }

perturbation <- function(temp,modelPert){
  res <- temp+rnorm(n=1,mean=0,sd=modelPert$sig)
  return(res)
}
