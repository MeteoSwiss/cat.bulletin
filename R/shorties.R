shorties_list_element <- function(shorties, title = NULL, appear = NULL, id = NULL) {
  shorties_list_element <- bulletin_element(type = "shorties_list", id = id, appear = appear)
  shorties_list_element[["shorties"]] <- shorties
  shorties_list_element[["title"]] <- title
  shorties_list_element
}

#' add shorties list to a bulletin
#' 
#' A shorty is a text element with title, text and optional link. The content is read from a file 
#' (see \code{\link{read_shorties}} and \code{\link{read_shorty}}).
#' A group or list of shorties is rendered together in a text section that has a title.
#' The group is identified by a group_id which serves to identify the shorty files that belong to the group.
#' If there are several language versions for a shorty (i.e., same group and id), the version of the current language is shown in both markdown and xml.
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @inheritParams read_shorties
#' @param title The text to be used as title for the group of shorties in the pdf and xml.
#' @family bulletin_elements
#' @export
add_shorties_list <- function(bulletin, 
                              group_id, 
                              title = NULL,
                              path,
                              subdir = NULL,
                              id = NULL, 
                              appear = c("xml", "pdf")) {
  shorties <- read_shorties(group_id = group_id, 
                            language = bulletin$language,
                            path = path, 
                            subdir = subdir)
  set_element(bulletin, 
              shorties_list_element(shorties = shorties, 
                                    title = title,
                                    id = id, 
                                    appear = appear)
  )
}

shorties_list_to_markdown <- function(element) {
  if (length(element$shorties) > 0) {
    md <- c(
      paste("##", element$title, "\n"),
      sapply(element$shorties, shorty_to_markdown)
    )
    paste(md, collapse = "\n")
  } else {
    "<!-- No content in this language -->"
  }
}

# A shorty is rendered as a title that works as a link and the lead text below.
shorty_to_markdown <- function(shorty) {
  md <- c(
    paste("###", 
          if (is.null(shorty$link)) shorty$title else paste0("[", shorty$title, "](", shorty$link, ")"),
          "\n"),
    paste(shorty$lead, "\n"),
    "\n"
  )
  paste(md, collapse = "\n")
}


shorties_list_to_xml <- function(xml, element, language) {
  md_in <- tempfile()
  html_out <- tempfile()
  writeLines(shorties_list_to_markdown(element), con = md_in)
  markdown::markdownToHTML(file = md_in, output = html_out, fragment.only = TRUE)
  html <- readr::read_lines(html_out)
  html <- paste(html, collapse = " ")
  text_node <- assure_node_of_type(xml, type = "text") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html) 
  text_node
}

#' Create a shorty list object
#' @param title title element
#' @param lead lead/content of the shorty
#' @param link optional link of the shorty
#' @param language language identifier for this shorty
#' @param id string identifying the shorty
create_shorty <- function(title, lead, link = NULL, language, id) {
  list(
    title = title,
    lead = lead,
    link = link,
    language = language,
    id = id
  )
}

#' Read all shorties for the given group_id and language from the bulletin_prod_path given in config.
#' @param group_id The string that identifies the group of shorties in the directory given.
#' @param language language identifier
#' @param path The base directory from where to read the shorties text files.
#' @param subdir An optional string giving a subdirectory within the base directory to look for the shorties text files.
#' @seealso read_shorty
#' @keywords internal
read_shorties <- function(group_id, language, path, subdir = NULL) {
  language <- match.arg(language, choices = c("de", "fr", "it", "en"))
  assert_that(is.readable(path),
              msg = paste("Cannot read base path for shorties", path))
  shorties_path <- ifelse(is.null(subdir), path, file.path(path, subdir))
  assert_that(is.readable(shorties_path),
              msg = paste("Cannot read shorties path", shorties_path))
  
  # list all files for given group
  pattern <- paste0("^", group_id, "_.*_", language, "\\.txt")
  #pattern <- paste0("^", group_id, "_.*\\.txt")
  log_debug("Looking for shorties with pattern", pattern, "in path", shorties_path)
  files <- list.files(shorties_path, pattern = pattern)
  
  lapply(files, function(file) {
    tryCatch(
      read_shorty(filepath = file.path(shorties_path, file)),
      error = function(e) {
        warning(paste("Could not read shorty file", file, "properly. Ignoring it."))
      }
    )
  })
}


#' Read a shorty from given filepath
#' @param filepath Path to the file that contains the short content.
#' @details 
#' A shorty file is a text file with two or three lines.
#' \itemize{
#'  \item{"line 1"}{Title}
#'  \item{"line 2"}{Content}
#'  \item{"optional line 3"}{Link (\url{https://...})} 
#' }
#' @keywords internal
read_shorty <- function(filepath) {
  log_debug("reading shorty file", filepath)
  assert_that(is.readable(filepath),
              msg = paste("Cannot read shorty text file", filepath))
  
  
  # get language and id from filename
  filename = basename(filepath)
  substrings <- unlist(strsplit(filename, "_"))
  tryCatch({
    id <- substrings[2]
    language <- substr(substrings[length(substrings)],1,2) # first two charachters of last
  },
  error = function(e) {
    stop(paste("Could not extract id and language from shorty filename", filename))
  })
  
  lines <- readLines(filepath)
  if (length(lines) > 3)
    warning(paste("shorty file", filepath, "contains more than 3 lines. Using only the first three lines."))
  assert_that(length(lines) >= 1,
              msg = paste("shorty file", filepath, "must contain at least 2 lines for title and lead.")
  )
  title <- lines[1]
  lead <- lines[2]
  link <- if(length(lines) >= 3) lines[3] else NULL
  
  create_shorty(title = title,
                lead = lead,
                link = link,
                id = id,
                language = language
  )
}

