set_working_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  # extract the script path
  script <- args[grep("--file", args)] # Extract the path from the argument
  script_path <- sub("--file=", "", script)

  # Normalize the path to handle any symbolic links or relative path elements
  normalized_script_path <- normalizePath(script_path)

  # Extract the directory part of the path
  script_dir <- dirname(normalized_script_path)

  print(paste("Setting working dir to:", paste0(script_dir, "/")))
  setwd(dirname(script_dir))
}

set_working_dir()

library(parallel)
library(tidyverse)
source("./R/tracing_files.R")

parse_roots <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("A root to search for files is expected for example 'RScript /path/to/script.R /path/to/root/folder '")
  }

  args
}

read_write_files <- function(files) {
  avail_cores <- parallel::detectCores()
  used_cores <- max(1, avail_cores - 2)
  mclapply(files, function(entry) {

    file_path <- entry["file"]
    file_data <- read_file(file_path)

    size <- as.numeric(str_extract(entry["dir"], "\\d+"))
    converted_data <- convert_data(data = file_data, size = size)

    # write it to a binary file, which is (hopefully faster to read)
    out_file_path <- sub("\\.csv$", ".rds", file_path)
    print(paste("Writing transformed data to:", out_file_path))
    ignore <- write_rds(converted_data, file = out_file_path, compress = "none", version = 3, text = FALSE)
  }, mc.cores = used_cores)
}

read_file <- function(file_path) {
  # Read data
  print(paste("reading file: ", file_path))
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  file_data <- read_csv(file_path)
  if (nrow(file_data) == 0 || ncol(file_data) != 6) {
    warning(paste("Failed to load tibble from file:", file_path, "The tibble looks like below:"))
    print(file_data)
    stop("Aborting reading process!")
  }
  return(file_data)
}

convert_data <- function(data, size, width = 30) {
  data %>%
    mutate(size = size) %>%
    mutate(func = paste(target, func_name, sep = "::")) %>%
    filter(func_name == "collect_travel_times" |
             func_name == "get_travel_times_by_mode_to_send" |
             func_name == "gather_travel_time_lengths" |
             func_name == "gather_travel_times_var_count" |
             func_name == "deserialize_travel_times" |
             func_name == "communicate_travel_times" |
             func_name == "handle_traffic_info_messages" |
             func_name == "handle_all" |
             func_name == "communicate_all"
    )
}

convert_csv_to_binary <- function(roots) {
  files <- detect_csv_tracing(roots = roots)
  read_write_files(files = files)
}

roots <- parse_roots()
convert_csv_to_binary(roots)
