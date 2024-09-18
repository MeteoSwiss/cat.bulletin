download_realization <- function(bulletin, product, filter, out_path, filename) {
  cat.func::download_realization(
    product = product,
    filter = filter,
    out_path = out_path,
    filename = filename,
    token_refresher = 
      mch.auth::oltoken_token_refresher(
        oltoken_envvar = "MCHDWH_OL_TOKEN",
        stage = bulletin$stage
      )
  )
}

download_data <- function(bulletin, product, filter, filename) {
  download_realization(bulletin = bulletin,
                       product = product,
                       filter = filter,
                       out_path = bulletin$data_path,
                       filename = filename
  )
}

donload_image <- function(bulletin, product, filter, filename) {
  download_realization(bulletin = bulletin,
                       product = product,
                       filter = filter,
                       out_path = bulletin$image_path,
                       filename = filename
  )
}
