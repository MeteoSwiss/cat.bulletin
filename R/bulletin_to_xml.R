
#' @export
bulletin_to_xml <- function(bulletin, filename = tempfile(fileext = ".xml")) {
  
  assert_that(length(get_elements(bulletin)) > 0, msg = "Bulletin must contain at least one element for xml processing.")
  
  xml <- xml2::xml_new_root(.value = "publication-page")
  
  root_node <- xml2::xml_root(xml)
  
  xml_fill_element_publication_page(xml, bulletin = bulletin)
  
  content_node <- xml2::xml_add_child(root_node, .value = "content") %>%
    xml_add_bulletin_elements(bulletin = bulletin)
  
  xml2::write_xml(xml2::xml_root(xml), file = filename)
  filename
}

xml_add_bulletin_elements <- function(content_node, bulletin) {
  
  default_language <- bulletin$languages[1]
  
  for (element in get_elements(bulletin, language = default_language, appear = "xml")) {
    log_debug("processing element", element$id)
    bulletin <- set_active_language(bulletin, language = default_language)
    function_name <- paste0(element$type, "_to_xml")
    
    withr::with_locale(
      new = c("LC_TIME" = get_locale(default_language)), {
        xml_node <- tryCatch({
          do.call(what = function_name, 
                  args = list(xml = content_node, element = element, language = default_language))
        },
        error = function(e) {
          warning_message <- paste("Could not process xml element", element$id, "for default language", default_language, ":", e)
          warning(warning_message)
          next
        }
        )
      })
    
    # process other languages
    for (language in bulletin$languages[-1]) {
      bulletin <- set_active_language(bulletin, language = language)
      
      # find element
      if (has_element(bulletin, language = language, id = element$id)) {
        lang_element <- get_elements(bulletin, language = language, id = element$id)[[1]]
        withr::with_locale(
          new = c("LC_TIME" = get_locale(language)), {
            tryCatch({
              do.call(what = function_name, 
                      args = list(xml = xml_node, element = lang_element, language = language))
            },
            error = function(e) {
              warning_message <- paste("Could not process xml element", element$id, "for language", language, ":", e)
              warning(warning_message)
              next
            })
          })
      } else {
        warning(paste("no element with id ", element$id, "found for language", language))
      }
    }
  }
  content_node
}

xml_fill_element_publication_page <- function(xml, bulletin) {
  metadata = bulletin$metadata
  xml <- xml %>%
    xml_set_attribute("sender", metadata$sender) %>%
    xml_set_attribute("path", metadata$path) %>%
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
  
  teaser_image_element <- copy_teaser_image(filepath = metadata$teaser_image, bulletin = bulletin)
  teaser_node <- xml2::xml_add_child(xml, .value = "image") %>%
    xml_set_attribute("fileName", paste0(teaser_image_element$image_dir, "/", teaser_image_element$filename)) %>%
    xml_set_attribute("source", metadata$teaser_source, languages = bulletin$languages)
  
  # publication node
  publication_node <- xml2::xml_add_child(xml, .value = "publication") %>%
    xml_set_attribute("publishedAt", metadata$publishedAt) %>%
    xml_set_attribute("categories", metadata$categories) %>%
    xml_set_attribute("publicationType", metadata$publication_type) %>% 
    xml_set_attribute("authors", metadata$authors)
  
  if (!is.null(metadata$edition)) 
    publication_node <- publication_node %>%  xml_set_attribute("edition", metadata$edition, languages = bulletin$languages)
  
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

#' Make sure that there is a node of the given type (i.e. given name)
#' 
#' If the given node is already of the type, no action is taken.
#' If the given node is not of the type, a child node is added with value set to the type. 
#' @examples 
#' \dontrun{
#' xml_node <- xml2::xml_new_root("root")
#' xml_node <- assure_node_of_type(xml_node, "text")
#' }
#' @keywords internal
assure_node_of_type <- function(xml_node, type) {
  if (xml2::xml_name(xml_node) != type) {
    xml_node <- xml2::xml_add_child(xml_node, .value = type)
  }
  xml_node
}

