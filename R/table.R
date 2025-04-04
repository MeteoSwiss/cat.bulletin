table_element <- function(table, bulletin_envir, appear = NULL, id = NULL, caption = NULL, table_nr = NULL) {
  assertthat::assert_that(inherits(table, "data.frame"))
  table_element <- bulletin_element(type = "table", id = id, appear = appear)
  # save the table in the bulletin markdown environment so that it can be accessed later in the rendering process
  table_var <- generate_element_id(type = "table")
  table_element[["table_var"]] <- table_var
  assign(table_var, table, envir = bulletin_envir)
  table_element[["bulletin_envir"]] <- bulletin_envir
  table_element[["caption"]] <- caption
  table_element[["table_nr"]] <- caption
  
  table_element
}

get_table <- function(table_element) {
  get(table_element$table_var, envir = table_element[["bulletin_envir"]])
}

#' Add a table to a bulletin
#' 
#' The \code{table} package for supporting tables.
#' @inheritParams bulletin_element 
#' @inheritParams add_element
#' @param table a table object created with the \code{table} package.
#' @param caption The caption for the table.
#' @family bulletin_elements
#' @examples
#' myData <- data.frame(a = 3, b = 4)
#' bulletin <- create_bulletin() %>%
#'   add_table(table = myData, caption = "An example table.")
#' @export
add_table <- function(bulletin, table, id = NULL, caption = NULL, appear = c("xml", "pdf")) {
  assertthat::assert_that(inherits(table, "data.frame"))
  
  # try to get previous table for table numbering
  table_nr <- length(get_elements(bulletin, type = "table")) + 1
  
  bulletin <- add_element(bulletin, element = table_element(table = table, 
                                                            id = id,
                                                            caption = caption,
                                                            bulletin_envir = bulletin$bulletin_envir,
                                                            table_nr = table_nr,
                                                            appear = appear)
  )
  
  # xml supports no table caption -> add a text element only visible in xml
  xml_caption <- paste0(cat.lang::get.text("table_label", lang = cat.func::isolang2dwhlang(bulletin$language)),
                        " ", table_nr, ": ",
                        caption)
  add_element(bulletin, element = text_element(id = paste(id, "_label"),
                                               text = xml_caption,
                                               appear = "xml")
  )
}


#' @rdname bulletin_to_markdown
table_to_markdown <- function(element) {
  md  <- paste("```{r, echo=FALSE}",
               paste0("kableExtra::kbl(", element$table_var, ", caption = '", element$caption, "')"),
               "```", 
               sep = "\n"
  )
  md
}

table_to_xml <- function(xml, element, language) {
  # write the table out as html
  file = tempfile(fileext = ".html")
  html <- as.character(kableExtra::kbl(get_table(element), format = "html"))
  
  table_node <- assure_node_of_type(xml, type = "table") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html)

  table_node
}

