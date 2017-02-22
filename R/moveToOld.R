#' Move a file to the old directory
#' 
#' Moves the file to the folder named "old" in the directory provided in the 
#'    `folder` attribute, from the `folder`.
#'    
#' @param file_name Name of the file.
#' @param folder Full (or relative) path to the folder with files.
moveToOld <- 
  function(file_name, folder) {
    
    dir.create(str_c(folder,"/","old"), showWarnings = FALSE)
    
    suppressWarnings(file.copy(from = str_c(folder, file_name),
                               to = str_c(folder, "/old/", file_name), 
                               overwrite = TRUE, 
                               copy.date = TRUE))
    
    invisible(file.remove(str_c(folder, file_name)))
    
  }