library(parallel)
library(tidyverse)

source("./src/main/R/tracing_files.R")

read_binary_tracing_files <- function(roots) {

  entries <- detect_binary_tracing(roots = roots)
  files <- lapply(entries, function(entry) { entry["file"] })

  tibbles <- lapply(files, function(file) {
    print(paste("Start reading file:", file))
    read_rds(file)
  })

  print(paste("Starting to bind", length(tibbles), "tibbles"))
  result <- bind_rows(tibbles)
  return(result)
}