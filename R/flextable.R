flextable_element <- function(flextable, bulletin_envir) {
  assertthat::assert_that(inherits(flextable, "flextable"))
  flextable_element <- bulletin_element(type = "flextable")
  # save the table by element_id in the bulletin markdown environment so that it can be accessed later in the rendering process
  assign(flextable_element$id, flextable, envir = bulletin_envir)
  flextable_element[["bulletin_envir"]] <- bulletin_envir
  
  flextable_element
}

get_flextable <- function(flextable_element) {
  get(flextable_element$id, envir = flextable_element[["bulletin_envir"]])
}

#' Add a flextable to a bulletin
#' 
#' The \code{flextable} package for supporting tables.
#' @inheritParams add_image
#' @param flextable a flextable object created with the \code{flextable} package.
#' @examples
#' myData <- data.frame(a = 3, b = 4)
#' myTable <- flextable(myData) 
#' bulletin <- create_bulletin() %>%
#'   add_flextable(flextable = myTable, caption = "An example table.")
#' @export
add_flextable <- function(bulletin, flextable, caption = NULL) {
  assertthat::assert_that(inherits(flextable, "flextable"))
  
  # add caption
  if (!is.null(caption)) {
    flextable <- set_caption(flextable, caption)
  }
  
  add_element(bulletin, element = flextable_element(flextable = flextable, bulletin_envir = bulletin$bulletin_envir))
}


#' @details 
#' \code{flextable_to_markdown}: Rendering of flextables to markdown is not possible (only directly to either html or pdf). 
#' The flextable object is thus stored by element_id in the bulletin environment and rendered in the final rendering step. 
#' @param element the bulletin element to be processed to markdown.
#' @rdname bulletin_to_markdown
flextable_to_markdown <- function(element) {
  md  <- paste("```{r, echo=FALSE}",
               element$id,
               "```", 
               sep = "\n"
  )
  md
}


flextable_to_xml <- function(xml, element) {
  # get the flextable 
  flextable <- get_flextable(element)
  
  # write the flextable out as html
  file = tempfile(fileext = ".html")
  flextable::save_as_html(flextable, path = file)
  
  # reread the file and extract the table node, convert it to xml and add it to the document
  html <- xml2::read_html(file)
  node <- xml2::xml_find_first(html, '//table')
  nodelist <- xml2::as_list(node)
  xmlnode <- xml2::as_xml_document(list(table = nodelist))
  xml2::xml_add_child(xml, .value = xmlnode)
  xml
}