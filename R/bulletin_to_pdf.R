
#' Render a bulletin to pdf
#' @param filename The name of the file to write the pdf.
#' @param bulletin The bulletin object created with \code{\link{create_bulletin}}.
#' @param language Single language identifier.
#' @details 
#' This function will set the current active language to language as a side effect.
#' @family rendering
#' @export
bulletin_to_pdf <- function(bulletin, 
                            language = bulletin$language, 
                            filename = tempfile(pattern = languaged("bulletin", language),
                                                fileext = ".pdf")
) {
  #documentation: https://bookdown.org/yihui/rmarkdown/pdf-document.html
  
  log_debug("Processing bulletin for language", language, "to pdf via markdown...")
  #cat.report::load.cat.report()
  
  markdown_file = bulletin_to_markdown(bulletin, language = language)
  log_debug("Processing file", markdown_file, "to pdf.")
  log_debug("Expected pdf-file:", filename)
  rmarkdown::render(markdown_file, 
                    envir = bulletin$bulletin_envir, 
                    # output_format = "pdf_document", 
                    output_file = filename, 
                    clean = FALSE)
}


#' Render a bulletin to markdown
#' 
#' @details 
#' This function will set the current active language to language as a side effect.
#' @param filename The name of the file to write the R markdown to.
#' @inheritParams bulletin_to_pdf
#' @family rendering
#' @export
bulletin_to_markdown <- function(bulletin, 
                                 language = bulletin$language, 
                                 filename = tempfile(pattern = languaged("bulletin", language),
                                                     fileext = ".Rmd")
) {
  bulletin <- set_active_language(bulletin, language)
  
  file_conn <- file(filename, open = "wb") # readr::write_lines only supports binary connections
  on.exit(close(file_conn))
  
  withr::with_locale(
    new = c("LC_TIME" = get_locale(language)), {
      # write the R markdown front matter first
      write_markdown_frontmatter(bulletin = bulletin, file_conn = file_conn)
      
      # write latex code to come within document but before content
      write_latex_preabmle(bulletin = bulletin, file_conn = file_conn)
      
      # process metadata
      write_markdown_metadata(bulletin = bulletin, file_conn = file_conn)
      
      # add markdown for all elements
      for (element in get_elements(bulletin, appear = "pdf")) {
        tryCatch({
          lines <- do.call(what = paste0(element$type, "_to_markdown"), args = list(element = element))
          readr::write_lines(lines, file = file_conn)
        },
        error = function(e) {
          warning_message <- paste("Could not process element", element$id, ":", e)
          warning(warning_message)
        }
        )
      }
    })
  filename
}

write_markdown_frontmatter <- function(bulletin, file_conn) {
  front_matter <- c(
    "---"
  )
  
  if (!is.null(bulletin$metadata$title)) {
    front_matter <- c(
      front_matter,
      paste("title:", bulletin$metadata$title[bulletin$language])
    )
  }
  
  babel <- switch(bulletin$language,
                  "de" = "ngerman",
                  "fr" = "french", 
                  "it" = "italian",
                  "en" = "british",
                  stop("unknown language")
  )
  
  front_matter <- c(front_matter,
                    "documentclass: |",
                    "  ```{=latex}",
                    "  mch_basisformular",
                    "   ```",
                    "output:",
                    "  pdf_document:",
                    "    keep_tex: true",
                    "    fig_caption: true",
                    "    fig_width: 3",
                    #                    paste0("    lang: ", bulletin$language, "-CH"),
                    "header-includes:",
                    "  - \\usepackage[utf8]{inputenc}",
                    "  - \\usepackage{xcolor}",
                    paste0("  - \\usepackage[", babel, "]{babel}"),
                    #"includes:",
                    #    "      in_header: 'preamble.tex',
                    #"  before_body: 'before_body.tex'",
                    #paste0("before_body: ", system.file("tex", 'before_body.tex', package = "cat.bulletin")),
                    "---"
  )
  readr::write_lines(front_matter, file = file_conn)
}

write_latex_preabmle <- function(bulletin, file_conn = file_conn) {
  
  # latex commands must be escaped (double backslash)
  preamble <- c(
    "```{=latex}
    % define variables used in headers and footers
    \\catpackage{cat.bulletin}
    %\\copyrightmeteo{}
    %\\contact{}
    
    %\\headerleft{left header}
    %\\headercenter{center header}
    %\\headerright{right header}
    ",
                #language for header picture
                paste0("\\lang{", cat.func::isolang2dwhlang(bulletin$language), "}"),
                "% show the MeteoSwiss logo on the first page
    \\thispagestyle{frontpage}",
    "```"
  )
  
  readr::write_lines(preamble, file = file_conn)
  
}


write_markdown_metadata <- function(bulletin, file_conn = file_conn) {
  write_lines <- function(lines) readr::write_lines(lines, file = file_conn)
  metadata <- bulletin$metadata
  language <- bulletin$language
  
  # already rendered by frontmatter -> remove there if treated here
  #if (!is.null(metadata$title)) {
  #title <- paste("#", metadata$title[language]) # level 1 title
  #lines <- text_to_markdown(element = text_element(text = title))
  #write_lines(lines)
  #}
  
  if (!is.null(metadata$lead)) {
    lead <- metadata$lead[language]
    lines <- text_to_markdown(element = text_element(text = lead))
    write_lines(lines)
  }
  
  # Teaser image
  if (!is.null(metadata$teaser_image)) {
    image_element <- copy_teaser_image(filepath = metadata$teaser_image, bulletin = bulletin)
    if (!is.null(metadata$teaser_source)) {
      image_element$source <- metadata$teaser_source[language]
    }
    lines <- image_to_markdown(element = image_element)
    write_lines(lines)
  }
  
  bulletin
  
}
