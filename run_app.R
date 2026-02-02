
library(rsconnect)

# NOTE: Replace paths and account info with your own deployment settings.
# This script is intended for manual deployment via rsconnect.
setwd("/path/")

rsconnect::setAccountInfo(name='name',
                          token='token',
                          secret='secret')


rsconnect::deployApp(
  appDir = "/path/",
  appName = "patchSeqQC",
  forceUpdate = TRUE
)


