create_bulletin_monthly <- function() {
  bulletin <- create_bulletin() %>%
    add_text(paste("# Monthly Bulletin", Sys.Date())) %>%
    add_text(paste("normal text"))
  
  pdf <- bulletin_to_pdf(bulletin)
  cat(paste("pdf:", pdf), fill = TRUE)
  
  
  xml <- bulletin_to_xml(bulletin)
  cat(paste("xml:", xml), fill = TRUE)
  
  zip <- bulletin_to_webzip(bulletin)
  cat(paste("zip:", zip), fill = TRUE)
}
