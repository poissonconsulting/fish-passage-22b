arc_to_dd <- function(x) {
  x_degrees <- str_extract(x, pattern = "[[:digit:]]*(?=°)") |> as.numeric()
  x_arcmins <- str_extract(x, pattern = "(?<=° )[[:digit:]]*(?=')")  |> as.numeric()
  x_arcsecs <- str_extract(x, pattern = "(?<=' )[[:graph:]]*(?=\\\")")  |> as.numeric()
  
  decimal_degrees <- x_arcmins / 60 + x_arcsecs / 3600
  x <- x_degrees + decimal_degrees
  x
}