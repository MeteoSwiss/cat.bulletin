######################### TEST #######################
# Produce bulletins for last 24 months
######################################################
devtools::load_all(".")

y1 <- 2024
y2 <- 2025

for (mx in 1) {
#for (mx in 1:12) {
  dir_path <- "/prod/zue/climate/others/zue/klimainformation/automatisierung_monatsbulletin/texfiles/climate-bulletin-monthly"
  items <- list.files(dir_path, full.names = TRUE)
  unlink(items, recursive = TRUE)
  
  y <- y1
  if (mx <= 4){
    y <- y2
  }
  if (mx < 10) {
    m0 <- paste0("0",mx)
  } else {
    m0 <- mx
  }
  
  bulletin <- bulletin_monthly(year = y, month = mx, provisional = FALSE, workdir = ".")
  
  for (lang in c("de","fr","it")) {
    bulletin_to_pdf(bulletin, language = lang, filename = file.path(getwd(),paste0(y,m0,"_",lang,".pdf")))
    source_file <- paste0("/prod/zue/climate/others/zue/klimainformation/automatisierung_monatsbulletin/texfiles/climate-bulletin-monthly/",y,m0,"_",lang,".pdf")
    destination_dir <- paste0("/prod/zue/climate/basic_serv/information/klimakommunikation/Neukonzeption Berichtelandschaft/Monatsbulletin_Tests/",y,m0,"_finalcheck_",lang,".pdf")
    # Copy the file
    file.copy(source_file, destination_dir)
  }
  
  
}
