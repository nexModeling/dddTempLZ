#' Temperature for each elevation zone
#'
#' The function \code{stateX()} process the temperature for each elevation zone
#' @param v temperature value (scalar)
#' @param modelTempLZ
#'  list(nbLevelZone=a,Tlr=b,hfelt=c,midmett=d)
#' @keywords TempLZ
#' @export
#' @examples
#' \dontrun{
#' stateX()
#' }
stateX <- function(v,modelTempLZ) {
  res <- v*(1+modelTempLZ$Tlr*((modelTempLZ$hfelt-modelTempLZ$midmett)/100))
  return(res)
}
