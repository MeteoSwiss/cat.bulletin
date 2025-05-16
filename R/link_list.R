#' Create a link list element
#' @param title The text to be used as title for the group of links in the pdf and xml.
#' @inheritParams bulletin_element
#' @export
link_list_element <- function(title = NULL, appear = c("xml", "pdf"), id = NULL) {
  link_list_element <- bulletin_element(type = "link_list", id = id, appear = appear)
  link_list_element[["link_list"]] <- list()
  link_list_element[["title"]] <- title
  link_list_element
}

#' Add a link list to a bulletin
#' @param link_list a link_list created with \code{\link{link_list_element}}.
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @family bulletin_elements
#' @export
#' @examples 
#' bulletin <- create_bulletin()
#' link_list <- link_list_element(title = "My Link List", id = "myLinkList") %>%
#'   add_link(path = "/meteoswiss/homepage/service-and-publications/publications", 
#'   label = "Publikationen", 
#'   url = "https://www-integ.meteoswiss.ch/services-and-publications/publications.html")
#' bulletin <- add_link_list(bulletin, link_list)
add_link_list <- function(bulletin, 
                          link_list
) {
  assert_that(bulletin_element_is_of_type(link_list, "link_list"))
  
  set_element(bulletin, 
              link_list,
  )
}

link_list_to_markdown <- function(element) {
  if (length(element$link_list) > 0) {
    md <- c(
      paste("##", element$title, "\n"),
      sapply(element$link_list, link_to_markdown)
    )
    paste(md, collapse = "\n")
  } else {
    "<!-- No content in this language -->"
  }
}

link_to_markdown <- function(link) {
  label <- if (is.null(link$label)) link$url else link$label
  md <- if (!is.null(link$url)) {
    paste0("* [", label, "](", link$url, ")\n")
  } else {
    log_info("Link", link$label, "has empty url", style = "warning")
    ""
  }
  paste(md, collapse = "\n")
}


link_list_to_xml <- function(xml, element, language) {
  # l!! assert that all languages have the same link list (paths!)
  
  link_list_node <- assure_node_of_type(xml, type = "link-download-list") %>%
    set_languaged_attribute(attribute = "title", language = language, value = element$title) 
  
  # iterate links within list
  first_run <- xml2::xml_length(link_list_node) == 0
  
  for (i in 1:length(element$link_list)) {
    link <- element$link_list[[i]]
    link_node <- if (first_run) {
      node <- xml2::xml_add_child(link_list_node, .value = "link")
      xml_set_attribute(node, attribute = "path", value = link$path)
      xml_set_attribute(node, attribute = "type", value = "internalLink")
      node
    } else {
      node <- xml2::xml_child(link_list_node, search = i)
      # assert that the path is identical
      if (!xml2::xml_has_attr(node, "path") || xml2::xml_attr(node, "path") != link$path)
        log_info("Path for link", i, "(language ", language, ") does not match the path of the corresponding link in the primary language", style = "warning")
      node
    }
    set_languaged_attribute(link_node, attribute = "label", value = link$label, language = language)
  }
  
  link_list_node
}

#' Create a link
#' @param path the CMS path to link to for this link
#' @param label the label text to show for this link
#' @param url the link url used in the pdf export
#' @export
create_link <- function(path, label, url) {
  list(
    path = path,
    label = label,
    url = url
  )
}

#' Create a link_list object
#' @param link_list the link_list element to add the link to
#' @inheritParams create_link
#' @export
add_link <- function(link_list, path, label, url) {
  link_list$link_list <- c(link_list$link_list, list(create_link(path = path, label = label, url = url)))
  link_list
}
