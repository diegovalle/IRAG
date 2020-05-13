## Auto-Install the following packages
.packs <- c("ggplot2", "tabulizer", "dplyr", "stringr",
            "rvest", "hrbrthemes", "scales", "tidyr", 
            "directlabels", "readxl",
            "readr", "ggrepel", "mgcv", "tidybayes", "tsibble", 
            "lubridate", "forcats")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success])
  sapply(names(.success)[!.success], require, character.only = TRUE)
}

options(stringsAsFactors = FALSE)

