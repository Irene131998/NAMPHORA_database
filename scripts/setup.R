## Script to set up dependencies.

# Get the project directory (root directory)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Install renv R pkg
#if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 2. List of necessary packages
#libraries <- c(
#  "neotoma2", "dplyr", "tidyr", "readr", "readxl", "openxlsx",
#  "tibble", "stringr", "tools", "here", "purrr", "BIEN", "rbacon",
#  "rice", "terra", "ggplot2", "sf", "leaflet", "htmlwidgets", "RColorBrewer",
#  "knitr", "leaflet.extras", "quarto", "base64enc", "bit", "bit64", "bslib", "cachem", "cellranger", "class", "classInt", 
#  "cli", "clipr", "colorspace", "commonmark", "cpp11", "crayon", "curl", "dataspice", 
#  "DBI", "digest", "e1071", "EML", "emld", "evaluate", "fansi", "farver", 
#  "fastmap", "fontawesome", "fs", "generics", "glue", "gtable", "highr", "hms", "htmltools", "httpuv", "isoband", 
#  "jqr", "jquerylib", "jsonld", "jsonlite", "KernSmooth", "labeling", "later", "lattice", 
#  "lazyeval", "lifecycle", "listviewer", "magrittr", "MASS", "Matrix", "memoise", 
#  "mgcv", "mime", "munsell", "nlme", "pillar", "pkgconfig", "prettyunits", 
#  "progress", "promises", "proxy", "R6", "rappdirs", "Rcpp", 
#  "rematch", "rhandsontable", "rlang", "rmarkdown", "rprojroot", 
#  "rstudioapi", "s2", "sass", "scales", "shiny", "sourcetools", "stringi", 
#  "tidyselect", "tinytex", "tzdb", "units", "utf8", 
#  "uuid", "V8", "vctrs", "viridisLite", "vroom", "whisker", "withr", "wk", "xfun", 
#  "xml2", "xtable", "yaml", "zip")


# 4. Initialise renv project (creates renv/ directory and renv.lock)
#renv::init()

# 5. Install packages in the renv project environment
#renv::install(libraries)

# 6. Save the packages information in the renv.lock file
#renv::snapshot()

# 7. Check environment status
#renv::status()

# 8. Installs the exact package versions used in this study. Run this line (once) to set up the environment from renv.lock file.
renv::restore()  
 