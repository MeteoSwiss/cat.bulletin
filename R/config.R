get_config_value <- function(key) {
  tryCatch(
  config::get(value = key, file = system.file(package = "cat.bulletin", "config", "config.yml"), use_parent = FALSE),
  error = function(e)
    stop(paste("Error while reading config file:", e))
  )
}
