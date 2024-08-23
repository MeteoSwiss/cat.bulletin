#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  
  
  bulletin <- create_bulletin() %>%
    add_text(paste("# Monthly Bulletin", Sys.Date())) %>%
    add_text(paste("normal text")) %>%
    add_image(filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png"),
              filename = "loess.png",
              caption = "This is a caption.")
  
  bulletin_pdfxmlzip(bulletin)
}
