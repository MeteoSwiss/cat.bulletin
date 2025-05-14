#' Generation of MeteoSwiss climate bulletins
#' 
#' cat.bulletin is used to automate climate bulletins to be published on the MeteoSwiss website as both html (xml) and pdf. 
#' @import assertthat
#' @keywords internal
"_PACKAGE"



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