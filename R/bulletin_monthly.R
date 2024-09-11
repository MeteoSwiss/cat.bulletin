#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  
  bulletin <- create_bulletin(bulletin_path = "./bulletin") %>%
    monatsbulletin_head() %>%
    monatsbilanz_temp()
  
  #bulletin_pdfxmlzip(bulletin)
  bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, "bulletin.pdf"))
}

monatsbulletin_head <- function(bulletin) {
  add_text(bulletin, paste("# Monthly Bulletin", Sys.Date())) %>%
    add_text(paste("normal text")) %>%
    add_image(filepath = system.file(package="cat.bulletin", "example-data", "climate-temperature-evolution-loess_climanom_1864-today_loess30_winter_regSwiss_fr.png"),
              filename = "loess.png",
              caption = "This is a caption.")
}

monatsbilanz_temp <- function(bulletin) {
  x <- 5
  add_Rmd(bulletin, filename = "test-element.Rmd")
}
