#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  bulletin <- create_bulletin() %>%
    add_text(paste("# Monthly Bulletin", Sys.Date())) %>%
    add_text(paste("normal text")) %>%
    add_image(filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png"),
              filename = "loess.png",
              caption = "This is a caption.")
  
  
  
  ### Write bulletin output
  
  pdf <- bulletin_to_pdf(bulletin)
  xml <- bulletin_to_xml(bulletin)
  zip <- bulletin_to_webzip(bulletin)
  
  cli::cli_h1("Output:")
  cli::cli_li(paste("pdf:", pdf))
  cli::cli_li(paste("xml:", xml))
  cli::cli_li(paste("zip:", zip))
}
