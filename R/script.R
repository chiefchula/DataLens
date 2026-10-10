pacman::p_load(dplyr, readxl, readr)

# DataLens: universal data profiling tool

import_data <- function(file_path) {
  
  extension <- tolower(
    tools::file_ext(file_path)
  )
  
  if (extension == "csv") {
    
    data <- readr::read_csv(
      file_path,
      show_col_types = FALSE
    )
    
  } else if (extension == "xlsx") {
    
    data <- readxl::read_excel(file_path)
    
  } else {
    
    stop("File format not supported yet.")
  }
  
  data <- as.data.frame(data)
  
  return(data)
}
