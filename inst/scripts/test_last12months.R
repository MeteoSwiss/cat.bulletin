######################### TEST #######################
# Produce bulletins for last 24 months
######################################################

y1 <- 2024
y2 <- 2025

#for (m in 10) {
for (m in 1:12) {
  dir_path <- "/prod/zue/climate/others/zue/klimainformation/automatisierung_monatsbulletin/texfiles/bulletin"
  items <- list.files(dir_path, full.names = TRUE)
  unlink(items, recursive = TRUE)
  
  y <- y1
  if (m == 1){
    y <- y2
  }
  if (m < 10) {
    m0 <- paste0("0",m)
  } else {
    m0 <- m
  }
  
  create_bulletin_monthly(year = y, month = m, provisional = FALSE, language = "de")
  
  source_file <- "/prod/zue/climate/others/zue/klimainformation/automatisierung_monatsbulletin/texfiles/bulletin/bulletin.pdf"
  destination_dir <- paste0("/prod/zue/climate/basic_serv/information/klimakommunikation/Neukonzeption Berichtelandschaft/Monatsbulletin_Tests/",y,m0,"_bulletin_test_",format(Sys.time(),"%Y%m%d%H%M"),".pdf")
  
  # Copy the file
  file.copy(source_file, destination_dir)
}
