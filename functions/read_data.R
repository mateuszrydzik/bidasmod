library(sf)

read_data <- function(path, col_name=NULL, urban_list=NULL, crs=NULL) {
  data <- read_sf(path)
  if (!is.null(col_name) && !is.null(urban_list)) {
    data <- data[data[[col_name]] %in% urban_list, ]
  }
  if (!is.null(crs)) {
    data <- st_transform(data, crs = crs)
  }
  return(data)
}