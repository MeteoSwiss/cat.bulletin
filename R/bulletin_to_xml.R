
#' @export
bulletin_to_xml <- function(bulletin, filename = tempfile(fileext = ".xml")) {
  xml <- xml2::xml_new_root(.value = "publication-page")
  
  root_node <- xml2::xml_root(xml)
  
  xml_fill_element_publication_page(xml, bulletin = bulletin)
  
  content_node <- xml2::xml_add_child(root_node, .value = "content") %>%
    xml_add_bulletin_elements(bulletin = bulletin)
  
  xml2::write_xml(xml2::xml_root(xml), file = filename)
  filename
}

xml_add_bulletin_elements <- function(content_node, bulletin) {
  for (language in bulletin$languages) {
    log_debug("processing language", language)
    bulletin <- set_active_language(bulletin, language = language)
    for (element in get_elements(bulletin)) {
      log_debug("processing element", element$id)
      function_name <- paste0(element$type, "_to_xml")
      xml_node <- content_node
      
      do.call(what = function_name, 
              args = list(xml = xml_node, element = element, language = language))
    }
  }
  content_node
}

xml_fill_element_publication_page <- function(xml, bulletin) {
  metadata = bulletin$metadata
  xml <- xml %>%
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
  
  #metadata node
  
  metadata_node <- xml2::xml_add_child(xml, .value = "metadata") %>%
    xml_set_attribute("description", metadata$lead, languages = bulletin$languages) %>%
    xml_set_attribute("keywords", metadata$keywords, languages = bulletin$languages) 
  
  # teaser node 
  
  file.copy(metadata$image$filepath, file.path(bulletin$image_path, metadata$image$filename))
  teaser_node <- xml2::xml_add_child(xml, .value = "image") %>%
    xml_set_attribute("fileName", paste0(bulletin$image_dir, "/", metadata$image$filename)) 
  
  # publication node
  publication_node <- xml2::xml_add_child(xml, .value = "publication") %>%
    xml_set_attribute("publishedAt", metadata$publishedAt) %>%
    xml_set_attribute("categories", metadata$categories, languages = bulletin$languages) %>%
    xml_set_attribute("publicationType", metadata$publication_type) %>% 
    xml_set_attribute("authors", metadata$authors, languages = bulletin$languages) 
  
  # document node
  document_node <- xml2::xml_add_child(publication_node, .value = "document") %>%
    xml_set_attribute("type", "downloadLink") %>%
    xml_set_attribute("fileName", metadata$publication, languages = bulletin$languages) 
  
  xml
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