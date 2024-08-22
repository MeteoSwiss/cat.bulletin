#' @importFrom magrittr %>%
#' @export
create_bulletin_monthly <- function() {
  bulletin <- create_bulletin() %>%
    add_text(paste("# Monthly Bulletin", Sys.Date())) %>%
    add_text(paste("normal text"))
  
  pdf <- bulletin_to_pdf(bulletin)
  xml <- bulletin_to_xml(bulletin)
  zip <- bulletin_to_webzip(bulletin)
  
  cli::cli_h1("Output:")
  cli::cli_li(paste("pdf:", pdf))
  cli::cli_li(paste("xml:", xml))
  cli::cli_li(paste("zip:", zip))
}
