devtools::unload();devtools::load_all()
bulletin <- bulletin_monthly(year = 2025, month = 3, workdir = ".")
bulletin_to_pdf(bulletin, language = "fr", filename = file.path(getwd(),"fr.pdf"))

#create_bulletin_monthly(year=2025,month=3)