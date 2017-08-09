#' Loading data from the folderes structured with the `built_ct_storage_structure()`
#'
#' @param rootFolder folder with the files that need to be loaded
#' @param subset vector containing all HS codes that need to be subsetted during loading
#' @param period range of years, where the information is required
#' @param forceReload ligical value indicating if data has to be leloaded from
#'         multiple files or could be loaded fom the previously loaded file
#' @param resave ligical indicating if the file has to be resaved in the folder with the loaded data
load_ct_rdata <-
  function(rootFolder,
    subset = NA,
    period = c(1990:2020),
    forceReload = FALSE,
    resave = TRUE) {
    # Folder finder
    allInOneFile <- file.path(rootFolder, "AllInOne")
    if (any(is.na(subset))) {
      allInOneFile <- file.path(allInOneFile, "AllInOne.rds")
    } else {
      allInOneFile <- file.path(allInOneFile, "AllInOneSubset.rds")
    }
    allSeparateFiles <- listCTdata(rootFolder, ".Rdata")
    lastModSepFiles <-
      file.mtime(file.path(rootFolder, list.files(rootFolder, "Rdata"))) %>%
      max()
    GlobalCahnged <- FALSE
    if (file.exists(allInOneFile) &
        lastModSepFiles < file.mtime(allInOneFile) &
        !forceReload) {
      data <- readr::read_rds(allInOneFile)
    } else {
      data <-
        load_some_ct_bulks_rdata(
          fromFolder = rootFolder,
          filesList = flt_year_ct(listCTdata(rootFolder, "Rdata"), period)$name,
          hsCodes = subset
        )
      GlobalCahnged <- TRUE
    }
    if (GlobalCahnged & resave) {
      write_rds(data, path = allInOneFile, compress = "gz")
    }
    data
  }
