

library(rsconnect)

setwd("/path/")

rsconnect::setAccountInfo(name='name',
                          token='token',
                          secret='secret')


rsconnect::deployApp(
  appDir = "/path/",
  appName = "patchSeqQC",
  forceUpdate = TRUE
)


