#' Create folders for storring CT data files on the hard drive
#' 
#' @param rootFolder The folder in which all storage is created. Beter keep default.
build_ct_storage_structure <-
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
    ifCreateDir(ctBulkZIP, "ctAnAll", F)
    ifCreateDir(ctBulkZIP, "ctMnAll", F)
    ifCreateDir(ctBulkZIP, "ctAnByCountry", F)
    ifCreateDir(ctBulkZIP, "ctMnByCountry", F)
    ctBulkR <- ifCreateDir(baseDB, "ctBulkR")
    ctAnAll <- ifCreateDir(ctBulkR, "ctAnAll")
    wtoAnAll <- ifCreateDir(ctAnAll, "wtoAnAll")
    ctMnAll <- ifCreateDir(ctBulkR, "ctMnAll")
    wtoMnAll <- ifCreateDir(ctMnAll, "wtoMnAll")
    wtoAnAllInOne <- ifCreateDir(wtoAnAll, "AllInOne")
    wtoMnAllInOne <- ifCreateDir(wtoMnAll, "AllInOne")
  }
