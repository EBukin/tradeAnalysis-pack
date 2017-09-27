#' Create folders for storring CT data files on the hard drive
#' 
#' @param rootFolder The folder in which all storage is created. Beter keep default.
build_ct_storage <-
  function(rootFolder = "~") {
    ifCreateDir <- function(basePath, folderName, returnName = TRUE) {
      newFolder <- file.path(basePath, folderName)
      if (!file.exists(newFolder))
        dir.create(newFolder)
      if (returnName)
        normalizePath(newFolder)
    }
    
    # Files structure:
    # ctData
    #-----ctBulkZIP
    #----------ctAnAll
    #----------ctMnAll
    #----------ctAnByCountry
    #----------ctMnByCountry
    #-----ctBulkR
    #----------ctAnAll
    #---------------wtoAnAll
    #--------------------AllInOne
    #----------ctMnAll
    #---------------wtoMnAll
    #--------------------AllInOne
    
    rootFolder <-  normalizePath(path.expand(rootFolder))
    
    baseDB <- ifCreateDir(rootFolder, "ctData")
    ctBulkZIP <- ifCreateDir(baseDB, "ctBulkZIP")
    ShinyDataFolder <- ifCreateDir(baseDB, "ShinyData")
    ifCreateDir(ctBulkZIP, "ctAnAll", F)
    ifCreateDir(ctBulkZIP, "ctMnAll", F)
    # ifCreateDir(ctBulkZIP, "ctAnByCountry", F)
    # ifCreateDir(ctBulkZIP, "ctMnByCountry", F)
    ctBulkR <- ifCreateDir(baseDB, "ctBulkR")
    ctAnAll <- ifCreateDir(ctBulkR, "ctAnAll")
    wtoAnAll <- ifCreateDir(ctAnAll, "wtoAnAll")
    ctMnAll <- ifCreateDir(ctBulkR, "ctMnAll")
    wtoMnAll <- ifCreateDir(ctMnAll, "wtoMnAll")
    wtoAnAllInOne <- ifCreateDir(wtoAnAll, "AllInOne")
    wtoMnAllInOne <- ifCreateDir(wtoMnAll, "AllInOne")
    
    # Copy Scripts
    SourceFolder <- system.file("ctData", package = "tradeAnalysis")
    filesToCopy <- file.path(SourceFolder, list.files(SourceFolder, all.files = T, no.. = TRUE))
    invisible(lapply(filesToCopy[stringr::str_detect(filesToCopy[], ".R$")], FUN = function(x) file.copy(from = x, to = baseDB, overwrite = TRUE)))
    invisible(lapply(filesToCopy[!stringr::str_detect(filesToCopy[], ".R$")], FUN = function(x) file.copy(from = x, to = baseDB, overwrite = FALSE)))
    
  }
