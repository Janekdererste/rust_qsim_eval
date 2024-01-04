library(parallel)
library(tidyverse)

source("./src/main/R/parsing.R")

trace_dirs <- function(root) {
  sub_dirs <- list.dirs(root)
  trace_directories <- sub_dirs[grep("/instrument", sub_dirs)]
  return(trace_directories)
}

trace_files_in_folders <- function(trace_dirs) {
  nested <- lapply(trace_dirs, function(trace_dir) {
    parent_dir <- basename(dirname(trace_dir))
    trace_files <- list.files(trace_dir, full.names = TRUE)
    lapply(trace_files, function(file) {
      c(dir = parent_dir, file = file)
    })
  })

  flat <- list_flatten(nested)
  return(flat)
}

stream_json <- function(files) {
  tibbles <- mclapply(files, function(entry){

    file_tibble <- tibble(size = numeric(), rank = numeric(), func = character(), busy = numeric(), idle = numeric())
    size <- as.numeric(str_extract(entry["dir"], "\\d+"))

    jsonlite::stream_in(file(entry["file"]), handler = function(traces){
      traces <- traces %>%
        unnest(fields) %>%
        unnest(span) %>%
        mutate(func = paste(target, name, sep = "::")) %>%
        mutate(size = size) %>%
        mutate(busy = parse_duration(time.busy)) %>%
        mutate(idle = parse_duration(time.idle)) %>%
        select(size, rank, func, busy, busy, idle)
      file_tibble %>% rows_append(traces)
    }, pagesize = 5000)
    return(file_tibble)
  }, mc.cores = 12, mc.preschedule = TRUE)

  result <- bind_rows(tibbles)
  return(result)
}

load_data <- function(files, num_cores) {
  tibbles <- mclapply(files, function(entry){

    size <- as.numeric(str_extract(entry["dir"], "\\d+"))
    file_data <- read_csv(entry["file"]) %>%
      mutate(size = size) %>%
      mutate(func = paste(target, func_name, sep = "::")) %>%
      select(timestamp, func, duration, rank, size)
    return (file_data)
  }, mc.cores = num_cores, mc.preschedule = TRUE)
  result <- bind_rows(tibbles)
  return (result)
}

load_csv_instrument <- function(root, num_cores = 1) {
  dirs <- trace_dirs(root)
  files <- trace_files_in_folders(dirs)
  data <- load_data(files, num_cores)
}

load_json <- function(root) {
  dirs <- trace_dirs(root)
  files <- trace_files_in_folders(dirs)
  traces <- stream_json(files)
}