#' Temperature field in to LZ temperature field
#'
#' Temperature field in to LZ temperature field
#' @param temp temperature field
#' @param modelTempLZ  list(nbLevelZone,Tlr,hfelt,midmett)
#' @keywords tempLZ
#' @export
#' @examples
#' \dontrun{
#' lz()
#' }
lz <- function(temp,modelTempLZ){
  tempLZ <- temp*(1+modelTempLZ$Tlr*((modelTempLZ$hfelt-modelTempLZ$midmett)/100))
  return(tempLZ)
}
