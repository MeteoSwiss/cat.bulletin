
#' @export
bulletin_to_webzip <- function(bulletin, zipfilename = tempfile(fileext = ".zip")) {

  publication <- empty_multi_language_string(languages = bulletin$languages)
  
  # generate all pdfs
  for (language in bulletin$languages) {
    tryCatch({
      filename <- bulletin_to_pdf(bulletin, filename = file.path(bulletin$bulletin_path, languaged_filename(bulletin$metadata$path, language, "pdf")))
       publication <- update_multi_language_string(publication, language, basename(filename))
    },
      error = function(e) stop(e)
    )
  }
  
  bulletin$metadata <- update_metadata_element(metadata = bulletin$metadata, publication = publication)

  bulletin_to_xml(bulletin, filename = file.path(bulletin$bulletin_path, "publication.xml"))
  
  withr::with_dir(new = file.path(bulletin$bulletin_path),
                  code = utils::zip(zipfile = zipfilename, 
                                    files = c("publication.xml",
                                              publication,
                                              "images"
                                    )
                  )
  )
  filename
}
