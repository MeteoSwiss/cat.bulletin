#' Create the zip file containing all data needed for publication on MeteoSwiss website.
#' @param zipfilename The name of the zipfile to be created. The zip file will be created within \code{bulletin$bulletin_path}.
#' @inheritParams bulletin_to_pdf
#' @family rendering
#' @export
bulletin_to_webzip <- function(bulletin, zipfilename = "climate-bulletin.zip") {

  log_info("Starting the webzip build process for bulletin ", bulletin$bulletin_id, style = "h1")
  
  publication <- empty_multi_language_string(languages = bulletin$languages)
  
  log_info("... webzip build process: generating pdfs for all languages", style = "h2")
  
  # generate all pdfs
  for (language in bulletin$languages) {
    tryCatch({
      pdf_filename <- languaged_filename(
        filename = paste(bulletin$bulletin_id, bulletin$year, bulletin$month, sep = "_"),
        language = language,
        ending = "pdf"
        )
      filename <- bulletin_to_pdf(bulletin, filename = file.path(bulletin$files_path, pdf_filename), language = language)
       publication <- update_multi_language_string(publication, language, paste0(bulletin$files_dir, "/", basename(filename)))
    },
      error = function(e) stop(e)
    )
  }
  
  bulletin$metadata <- update_metadata_element(metadata = bulletin$metadata, publication = publication)

  bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "publication.xml"))
  
  log_info("... webzip build process: creating zip file", style = "h2")
  
  withr::with_dir(new = file.path(bulletin$bulletin_path),
                  code = utils::zip(zipfile = zipfilename, 
                                    files = c("publication.xml",
                                              publication,
                                              "images",
                                              "files"
                                    )
                  )
  )
  
  log_info("Webzip for bulletin '", bulletin$bulletin_id, "' written to", zipfilename, ".", style = "success")
  
  filename
}
