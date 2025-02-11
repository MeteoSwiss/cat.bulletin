# gar nichts anpassen:

test <- evoclim::homogval.evol(
  homog.param = "ths200m0",
  stations = "swissmean",
  begin.date = "186401",
  add.current.mo = FALSE,
  end.date = NULL,
  val.resolution = "m",
  anomalies = FALSE,
  begin.refperiod = "1871",
  end.refperiod = "1900",
  data.line = "loess",
  write.txt = TRUE,
  loess.param = list(
    window = 30,
    conf.int = TRUE,
    do.diff = TRUE,
    y1 = 1885,
    y2 = NULL,
    y1.as.mean = TRUE,
    add.exp.val = TRUE
  ),
  equal.yaxe = TRUE,
  add.ranking.high = TRUE,
  add.ranking.low = TRUE,
  add.2nd.norm.line = c(1991, 2020),
  add.2nd.norm.value = TRUE
)

