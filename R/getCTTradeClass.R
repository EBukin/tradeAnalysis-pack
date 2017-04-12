getCTTradeClass <- 
  function() {
    classifications <- 
      c("HS as reported" = "HS",
        "HS 1992" = "H0",
        "HS 1996" = "H1",
        "HS 2002" = "H2",
        "HS 2007" = "H3",
        "HS 2012" = "H4",
        "HS 2017" = "H5",
        "SITC.4" = "S4",
        "SITC.3" = "S3",
        "SITC.2" = "S2",
        "SITC.1" = "S1",
        "SITC" = "ST",
        "BEC" = "BEC",
        "EB02" = "EB02")
    classifications
  }
