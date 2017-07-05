#' Get the observations
#'
#' Get and process, if needed, the observations
#' @param method method for processing observations, "lz", "processedNVE"
#' @param path Directory where to get the files
#' @param filename Filename (path included) of the file to be read
#' @param ... options for the different methods
#' @keywords tempLZ
#' @export
#' @examples
#' \dontrun{
#' getTempLZ()
#' }
getTempLZ <- function(method=NULL,path=NULL,filename=NULL,...) {

    tempLZ <- switch(method,
                   "processedNVE" = getTempLZ.processedNVE(path=path,filename=filename),
                   "lz"           = getTempLZ.lz(path=path,filename=filename,...),
                   "perturbation" = getTempLZ.perturbation(path=path,filename=filename,...),
                   (message=paste0("Invalid method:", method,".")))

    return(tempLZ)
}


getTempLZ.processedNVE <- function(path,filename){
  env <- environment()
  path <- normalizePath(file.path(path,filename),mustWork = FALSE)
  tmp <- utils::read.table(path,sep="\t")
  assign("tempLZ",tmp[,15:24],envir=env)
  rm(tmp)
  return(get("tempLZ",envir=env))
}


getTempLZ.lz <- function(path,filename,...){
  env <- environment()
  path <- normalizePath(file.path(path,filename),mustWork = FALSE)
  tmp <- utils::read.table(path,sep="\t")
  assign("temp",tmp[,5],envir=env)
  rm(tmp)
  tempLZ <- lz(temp=get("temp",envir=env),...)
  return(tempLZ)
}

getTempLZ.perturbation <- function(path,filename,...){
  env <- environment()
  path <- normalizePath(file.path(path,filename),mustWork = FALSE)
  tmp <- utils::read.table(path,sep="\t")
  assign("tempLZ",tmp[,15:24],envir=env)
  rm(tmp)
  res <- perturbation(temp=get("tempLZ",envir=env),...)
  return(res)
}
