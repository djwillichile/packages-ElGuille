#' @import magrittr
#' @title Transponer data.frame
#' @description Esta es una función que permite transponer de manera eficiente una data.fame con más de 2 columnas.
#' @param x data.frame object from R
#' @param originalType a numeric vector with the fields you want to keep the original types. The default value is NULL.
#' @param fieldsFactor a numeric vector with the fields you want to assign with Factor class. Default is \code{NULL}.
#' @details Esta funcion es ideal para obtener el metadata en los encabezados de los archivos que contienen bases de datos meteorológicas generados por el laboratorio.
#' La funcion recibe un data.frame horizontal \code{x} y devuelve un data.frame vertical con los campos formateados según su tipo.
#' @examples
#' 
#' data("metaData")
#' View(metaData)
#' 
#' transposed(metaData,1)
#' 
#' @export

transposed <- function(x,originalType=NULL,fieldsFactor=NULL) {
  x
  if(!is.data.frame(x)) stop("'x' should be data.frame object from R")
  if(ncol(x)<2) stop("dataframe must have more than 2 columns")
  if(!is.numeric(originalType)&!is.null(originalType)) stop("'originalType' should be an numeric vector")
  if(!is.numeric(fieldsFactor)&!is.null(fieldsFactor)) stop("'fieldsFactor' should be an numeric vector")
  
  df <- x
  dft <- as.data.frame(t(df[-1]), stringsAsFactors = FALSE) 
  names(dft) <- df[,1]
  if(is.null(originalType)){
    dft <- type.convert(dft,as.is = T)
  }else{
    dft[-originalType]=type.convert(dft[-originalType],as.is = T)
  }
  
  if(!is.null(fieldsFactor)){
    dft[fieldsFactor]=as.data.frame(lapply(dft[fieldsFactor],as.factor))
  }
  
  rownames(dft)=NULL
  return(dft)
}