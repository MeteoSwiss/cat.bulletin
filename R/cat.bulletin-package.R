#' Generation of MeteoSwiss climate bulletins
#' 
#' @name cat.bulletin-package
#' @docType package
#' @import assertthat
NULL


.onLoad <- function(libname,pkgname){cat.lang::load.text(package=pkgname)}

.onUnload <- function(libpath){cat.lang::unload.text(libpath)}