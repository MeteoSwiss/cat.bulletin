devtools::unload();devtools::load_all()
bulletin <- bulletin_monthly(year = 2025, month = 4, workdir = ".")
bulletin_to_pdf(bulletin, language = "de", filename = file.path(getwd(),"de.pdf"))
bulletin_to_webzip(bulletin)

create_bulletin_monthly(year=2025,month=3)

# test bulletin in environment without bulletin_prod_path
withr::with_envvar(list(R_CONFIG_ACTIVE = "testNonReadableProdPath"),
                   code = {
                     bulletin <- cat.bulletin::bulletin_monthly(year = 2025, month = 4, workdir = ".")
                     bulletin_to_webzip(bulletin = bulletin)
                   }
)
