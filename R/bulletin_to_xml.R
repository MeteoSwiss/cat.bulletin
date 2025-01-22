
#' @export
bulletin_to_xml <- function(bulletin, filename = tempfile(fileext = ".xml")) {
  language = bulletin$language
  
  xml <- xml2::xml_new_root(.value = "publication-page")
  
  root_node <- xml2::xml_root(xml)
  #metadata_node <- xml2::xml_add_child(root_node, .value = "metadata")
  #content_node <- xml2::xml_add_child(root_node, .value = "content")
  xml_fill_element_publication_page(xml, bulletin)
  
  # for (element in get_elements(bulletin)) {
  #   log_debug("processing element", element$id)
  #   function_name <- paste0(element$type, "_to_xml")
  #   xml_node <- switch(element$type,
  #                      title = root_node,
  #                      content_node
  #   )
  #   
  #   do.call(what = function_name, 
  #           args = list(xml = xml_node, element = element, language = language))
  # }
  xml2::write_xml(xml2::xml_root(xml), file = filename)
  filename
}

xml_fill_element_publication_page <- function(xml, bulletin) {
  metadata = bulletin$metadata
  xml %>%
    xml_set_attribute("sender", metadata$sender) %>%
    xml_set_attribute("path", 
                      paste0(
                        "/meteoswiss/homepage/service-and-publications/publications/reports-and-bulletins/",
                        metadata$path
                      )
    ) %>%
    xml_set_attribute("alias", metadata$alias) %>%
    xml_set_attribute("enabledLocales", paste0(bulletin$languages, collapse = ",")) %>%  
    xml_set_attribute("title", metadata$title, languages = bulletin$languages) %>%
    xml_set_attribute("type", "complex") %>%
    xml_set_attribute("lead", metadata$lead, languages = bulletin$languages) 
}

#' set attribute of xml node
#' if languages is set to a (set of) language identifier, the languaged version of the attributes are set. 
#' If the attribute is not multilanguage, and error is thrown.
xml_set_attribute <- function(xml, attribute, value, languages = NULL) {
  
  if (is.null(value)) {
    return(xml)
  }
  
  if (is.null(languages)) {
    xml2::xml_attr(xml, attribute) <- value
  } else {
    # languaged versions
    assert_multi_language_string(value, languages = languages, name = attribute)
    for (language in languages) {
      if (has_name(value, language)) {
        xml2::xml_attr(xml, languaged(attribute, language)) <- value[language]
      } else {
        warning(paste("Cannot set attribute", attribute, "for language", language))
      }
    }
  }
  
  xml
}