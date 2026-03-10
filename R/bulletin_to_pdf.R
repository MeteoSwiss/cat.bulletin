
#' Render a bulletin to pdf
#' @param filename The name of the file to write the pdf.
#' @param bulletin The bulletin object created with \code{\link{create_bulletin}}.
#' @param language Single language identifier.
#' @section PDF options:
#' \describe{
#'  \item{Contact email address}{Per default, MeteoSwiss Customer Service email is used in the footer. 
#'  To overwrite this, set the slot \code{pdf_contact_email} of the bulletin object to an email address 
#'  or to NULL to suppress the contact output. You can do this at creation time using the \code{bulletin_args} argument 
#'  of \code{\link{create_bulletin}} or set it at a later stage.  
#'  }
#' }
#' @details 
#' This function will set the current active language to language as a side effect.
#' @examples
#' bulletin <- create_test_bulletin_for_web()
#' # change pdf contact email 
#' bulletin[["pdf_contact_email"]] <- "helpdesk@meteoswiss.ch"
#' bulletin_to_pdf(bulletin)
#' 
#' @family rendering
#' @export
bulletin_to_pdf <- function(bulletin, 
                            language = bulletin$language, 
                            filename = tempfile(pattern = languaged("bulletin", language),
                                                fileext = ".pdf")
) {
  #documentation: https://bookdown.org/yihui/rmarkdown/pdf-document.html

  log_info("Processing bulletin for ", language, " to pdf via markdown...")
  #cat.report::load.cat.report()
  
  markdown_file = bulletin_to_markdown(bulletin, language = language,
                                       filename = file.path(bulletin$bulletin_path, 
                                                            paste0(bulletin$bulletin_id, "_", language, ".Rmd"))
  )
  log_debug("Processing file ", markdown_file, " to pdf.")
  
  # create file in workdir 
  pdf_outfile <- tempfile(tmpdir = bulletin$bulletin_path, 
                          pattern = paste0(bulletin$bulletin_id, "_", language, "_"),
                          fileext = ".pdf"
  )
  
  log_debug("Expected pdf-file: ", pdf_outfile)
  
  quiet = logger::log_threshold() >= logger::DEBUG # be verbose on debug level
  # !! latex / pandoc won't work within tryCatch block !! 
  #  tryCatch(
  rmarkdown::render(markdown_file, 
                    envir = bulletin$bulletin_envir, 
                    # output_format = "pdf_document", 
                    output_file = pdf_outfile, 
                    quiet = quiet,
                    clean = FALSE)
  #   warning = function(w) {
  #     log_debug("Latex warning in bulletin_to_pdf:", w$message)
  #   },
  #   error = function(e) {
  #     stop(paste("Could not produce pdf for language", language, ".", e$message))
  #   }
  # )
  
  if (has_element(bulletin, type = "shorties_list")) {
    log_debug("Looking for pdfs to attach from shorties lists.")
    
    shorties_lists <- get_elements(bulletin, language = language, type = "shorties_list")
    
    log_debug("Found ", length(shorties_lists), " shorties_lists")
    
    combined_outfile <- NULL
    
    pdfs <- unlist(sapply(shorties_lists, get_shorties_pdfs))
    
    if (length(pdfs) > 0) {
      log_info("Attaching ", length(pdfs), " pdf files to bulletin")
      tryCatch({
        combined_outfile <- file.path(bulletin$bulletin_path, 
                                      paste0(bulletin$bulletin_id, "_combined_", language, ".pdf")
        )
        qpdf::pdf_combine(input = c(pdf_outfile, pdfs),
                          output = combined_outfile)
        log_debug("Successfully merged pdfs to file ", combined_outfile)
      },
      error = function(e) {
        log_warn("Error during combination of bulletin pdf with pdfs from shorties.", e$message)
        combined_outfile <- NULL
      }
      )
    }
    
    # Copying outfile to the final location
    if (!is.null(combined_outfile)) 
      pdf_outfile <- combined_outfile
  }
  
  log_debug("Copy outfile '", pdf_outfile, "' to final location: ", filename)
  file.copy(pdf_outfile, filename, overwrite = TRUE)
  
  log_info("PDF produced for language ", language, ".")
  return(filename)
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
  
  babel_lang <- switch(bulletin$language,
                       "de" = "ngerman",
                       "fr" = "french", 
                       "it" = "italian",
                       "en" = "british",
                       stop("unknown language")
  )
  
  keep_tex = getOption("log_level", default = 1) > 1
  front_matter <- c(front_matter,
                    "documentclass: |",
                    "  ```{=latex}",
                    "  mch_basisformular",
                    "   ```",
                    "output:",
                    "  pdf_document:",
                    paste0("    keep_tex: ", if (keep_tex) "true" else "false"),
                    "header-includes:",
                    "  - \\usepackage[utf8]{inputenc}",
                    "  - \\usepackage{tcolorbox}",
                    # use babel for language specific formatting, redefine labels for figures and tables
                    paste0("  - \\usepackage[", babel_lang, "]{babel}"),
                    paste0("  - \\addto\\captions", babel_lang, "{\\renewcommand{\\figurename}{", cat.lang::get.text("figure_label"),"}}"),
                    paste0("  - \\addto\\captions", babel_lang, "{\\renewcommand{\\tablename}{", cat.lang::get.text("table_label"),"}}"),
                    "---"
  )
  readr::write_lines(front_matter, file = file_conn)
}

write_latex_preabmle <- function(bulletin, file_conn = file_conn) {
  
  # latex commands must be escaped (double backslash)
  preamble <- c(
    "```{=latex}
    % define variables used in headers and footers",
    paste0("\\catpackage{", get_calling_namespace(), "}"),
    paste0("\\copyrightmeteo{", cat.lang::get.text("copyright"), "}")
  )
  # add contact email (can be configured via bulletin-object)
  contact_email <- cat.lang::get.text("email.kud") #default
  if (utils::hasName(bulletin, "pdf_contact_email")) {
    contact_email <- bulletin[["pdf_contact_email"]]
  }
  if (! is.null(contact_email))
    preamble <- c(preamble, 
                  paste0("\\contact{", cat.lang::get.text("contact"), ": ", contact_email, "}")
    )
  # continue preamble
  preamble <- c(preamble, 
                "%\\headerleft{left header}
    %\\headercenter{center header}
    %\\headerright{right header}
    ",
                #language for header picture
                paste0("\\lang{", cat.func::isolang2dwhlang(bulletin$language), "}"),
                paste0("\\title{",bulletin$metadata$title[bulletin$language],"}"),
                "% make room for frontpage header - has to come before maketitle",
                "\\newgeometry{top=48mm,bottom=16mm,left=30mm,right=20mm}",
                "\\maketitle",
                "% show the MeteoSwiss logo on the first page
    \\thispagestyle{frontpage}",
                "```"
  )
  
  # load required R packages
  preamble <- c(
    preamble, c(
      "```{r initalSetup, include=FALSE}",
      "require(kableExtra)",
      "```"
    )
  )
  
  readr::write_lines(preamble, file = file_conn)
  # if (!is.null(bulletin$metadata$title)) {
  #   front_matter <- c(
  #     front_matter,
  #     paste("title:", bulletin$metadata$title[bulletin$language])
  #   )
  # }
  
}


write_markdown_metadata <- function(bulletin, file_conn = file_conn) {
  write_lines <- function(lines) readr::write_lines(lines, file = file_conn)
  metadata <- bulletin$metadata
  language <- bulletin$language
  
  if (!is.null(metadata$lead)) {
    lead <- metadata$lead[language]
    lines <- text_to_markdown(element = text_element(text = lead))
    write_lines(lines)
  }
  
  # Teaser image
  if (!is.null(metadata$teaser_image)) {
    image_element <- copy_teaser_image(filepath = metadata$teaser_image, bulletin = bulletin)
    
    # do not use caption and source of the image element so that we do not get a "figure" label
    # -> prepare manual caption
    caption <- metadata$teaser_caption[language]
    if (!is.null(metadata$teaser_source)) {
      caption <- paste0(caption, " (", metadata$teaser_source[language], ")")
    }
    
    lines <- image_to_markdown(element = image_element)
    write_lines(lines)
    write_lines(caption) # output manual caption as simple text
    write_lines("\n\n")
  }
  
  bulletin
  
}

