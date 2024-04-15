library(parallel)
library(tidyverse)

source("./src/main/R/tracing_files.R")

read_binary_tracing_files <- function(roots, on_load = function(data) { return(data) }, parallel = FALSE) {

  entries <- detect_binary_tracing(roots = roots)
  files <- lapply(entries, function(entry) { entry["file"] })

  if (parallel) {
    tibbles <- mclapply(files, function(file) { read_binary_file(file, on_load) }, mc.cores = 12)
  } else {
    tibbles <- lapply(files, function(file) { read_binary_file(file, on_load) })
  }
  print(paste("Finished reading, binding", length(tibbles), "tibbles"))
  result <- bind_rows(tibbles)
  return(result)
}

read_binary_file <- function(file, on_load) {
  print(paste("Start reading file:", file))
  data <- read_rds(file)
  on_load(data = data)
}

read_matsim_tracing_files <- function(roots) {
  entries <- detect_matsim_tracing(roots = roots)
  files <- lapply(entries, function(entry) { entry["file"] })

  tibbles <- lapply(files, function(file) {
    print(paste("Start reading file:", file))
    read_csv(file)
  })

  print(paste("Starting to bind", length(tibbles), "tibbles"))
  result <- bind_rows(tibbles)
  return(result)
}

read_neighbor_files <- function(roots) {
  entries <- detect_neighbor_files(roots = roots)
  files <- lapply(entries, function(entry) { entry["file"] })

  tibbles <- lapply(files, function(file) {
    read_csv(file) %>%
      mutate(size = max(rank) + 1)
  })

  result <- bind_rows(tibbles)
  return(result)
}
