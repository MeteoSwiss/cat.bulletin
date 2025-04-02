#' Generation of MeteoSwiss climate bulletins
#' 
#' @name cat.bulletin-package
#' @docType package
#' @import assertthat
NULL


.onLoad <- function(libname,pkgname){
  cat.lang::load.text(package=pkgname)
  
  stylepath <- dirname(system.file("tex", "mch_basisformular.cls", 
                                   package = "cat.bulletin", mustWork = TRUE))
  if (!grepl(stylepath, Sys.getenv("TEXINPUTS"))) {
    Sys.setenv(TEXINPUTS = paste0(".:", stylepath, "/:", 
                                  Sys.getenv("TEXINPUTS")))
  }
}


.onUnload <- function(libpath){cat.lang::unload.text(libpath)}