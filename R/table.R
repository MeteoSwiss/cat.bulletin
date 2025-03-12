table_element <- function(table, bulletin_envir, id = NULL, caption = NULL) {
  assertthat::assert_that(inherits(table, "data.frame"))
  table_element <- bulletin_element(type = "table", id = id)
  # save the table in the bulletin markdown environment so that it can be accessed later in the rendering process
  table_var <- generate_element_id(type = "table")
  table_element[["table_var"]] <- table_var
  assign(table_var, table, envir = bulletin_envir)
  table_element[["bulletin_envir"]] <- bulletin_envir
  table_element[["caption"]] <- caption
  
  table_element
}

get_table <- function(table_element) {
  get(table_element$table_var, envir = table_element[["bulletin_envir"]])
}

#' Add a table to a bulletin
#' 
#' The \code{table} package for supporting tables.
#' @inheritParams add_image
#' @param table a table object created with the \code{table} package.
#' @examples
#' myData <- data.frame(a = 3, b = 4)
#' bulletin <- create_bulletin() %>%
#'   add_table(table = myData, caption = "An example table.")
#' @export
add_table <- function(bulletin, table, id = NULL, caption = NULL) {
  assertthat::assert_that(inherits(table, "data.frame"))
  add_element(bulletin, element = table_element(table = table, 
                                                id = id,
                                                caption = caption,
                                                bulletin_envir = bulletin$bulletin_envir)
  )
}


#' @rdname bulletin_to_markdown
table_to_markdown <- function(element) {
  md  <- paste("```{r, echo=FALSE}",
               paste0("kableExtra::kbl(", element$table_var, ")"),
               "```", 
               sep = "\n"
  )
  md
}

table_to_xml <- function(xml, element, language) {
  # write the table out as html
  file = tempfile(fileext = ".html")
  html <- as.character(kableExtra::kbl(get_table(element), format = "html"))
  
  text_node <- assure_node_of_type(xml, type = "table") %>%
    set_languaged_attribute(attribute = "html", language = language, value = html) 
  text_node
}