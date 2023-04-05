source("header.R")

writeLines(capture.output(session_info()), str_c("session-info-", user(),".txt"))
