
disclaimer_element <- function(caption_text_element, body_Rmd_element, appear = NULL, id = NULL) {
  disclaimer_element <- bulletin_element(type = "disclaimer", id = id, appear = appear)
  disclaimer_element[["caption_text_element"]] <- caption_text_element
  disclaimer_element[["body_Rmd_element"]] <- body_Rmd_element
  disclaimer_element
}

#' add disclaimer element to a bulletin
#' 
#' A disclaimer element consists of a caption (given as text) and a body (specified as a Rmd file).
#' On the website (xml), the disclaimer is rendered as an accordeon.
#' In the pdf, the caption is omitted and the body is set in a gray box. 
#' @param caption_text disclaimer caption as text
#' @param body_Rmd_element_id element_id of the Rmd element for the body of the disclaimer (see \code{\link{add_Rmd}}).
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @inheritParams add_Rmd
#' @export
add_disclaimer <- function(bulletin, 
                           caption_text, 
                           body_Rmd_element_id, 
                           envir = parent.frame(), 
                           appear = c("xml", "pdf"),
                           id = NULL) {
  log_debug("Adding disclaimer element")
  
  # caption text
  caption_text_element = text_element(text = caption_text)
  
  # body as Rmd
  filename <- paste0(paste(bulletin$bulletin_id, body_Rmd_element_id, bulletin$language, sep ="_"), ".Rmd")
  body_Rmd_element <- Rmd_element(filename = filename, envir = envir)
  
  add_element(bulletin, disclaimer_element(
    caption_text_element = caption_text_element,
    body_Rmd_element = body_Rmd_element, id = id, appear = appear)
  )
}

disclaimer_to_markdown <- function(element) {
  c(
    "```{=tex}",
    "\\begin{tcolorbox}",
    "```",
    # text_to_markdown(element$caption_text_element), "\n",
    Rmd_to_markdown(element$body_Rmd_element), "\n",
    "```{=tex}",
    "\\end{tcolorbox}",
    "```"
  )
}

disclaimer_to_xml <- function(xml, element, language) {
  box_node <- assure_node_of_type(xml, type = "box")
  
  title = element$caption_text_element$text
  body = Rmd_to_html(element$body_Rmd_element)
  
  log_debug("Processing disclaimer to xml with title", title)
  
  # get text node
  text_node  <- if (xml2::xml_length(box_node) == 0) {
    xml2::xml_add_child(box_node, .value = "text")
  } else {
    xml2::xml_child(box_node)
  }
  # set title
  set_languaged_attribute(box_node,
                          attribute = "heading", 
                          language = language, 
                          value = title)
  
  # set body
  set_languaged_attribute(text_node, 
                          attribute = "html", 
                          language = language, 
                          value = body)
  
  return(box_node)
}

# this version of the code sets the disclaimer inside an accordeon
# did not work on the website in April 2025
disclaimer_to_xml2 <- function(xml, element, language) {
  accordion_node <- assure_node_of_type(xml, type = "accordion")
  
  title = element$caption_text_element$text
  body = Rmd_to_html(element$body_Rmd_element)
  
  # get panel node
  accordion_panel_node  <- if (xml2::xml_length(accordion_node) == 0) {
    xml2::xml_add_child(accordion_node, .value = "accordion-panel")
  } else {
    xml2::xml_child(accordion_node)
  }
  # set title
  set_languaged_attribute(accordion_panel_node,
                          attribute = "title", 
                          language = language, 
                          value = title)
  
  # get text node inside box
  if (xml2::xml_length(accordion_panel_node) == 0) {
    box_node <- xml2::xml_add_child(accordion_panel_node, .value = "box")
    text_node <- xml2::xml_add_child(box_node, .value = "text")
  } else {
    box_node <- xml2::xml_child(accordion_panel_node)
    text_node <-xml2::xml_child(box_node)
  }
  
  # set body
  set_languaged_attribute(text_node, attribute = "html", language = language, value = body)
  
  # set title
  set_languaged_attribute(box_node,
                          attribute = "heading", 
                          language = language, 
                          value = title)
  
  return(accordion_node)
}