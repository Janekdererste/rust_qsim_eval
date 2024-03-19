library(parallel)
library(tidyverse)

source("tracing_files.R")

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
    mutate(
      time_bin = cut_width(sim_time, width = width, boundary = 0, closed = "left"),
      bin_start = floor(sim_time / width) * width
    ) %>%
    group_by(bin_start, size, rank, func) %>%
    summarize(median_dur = median(duration), .groups = "drop")
}

convert_csv_to_binary <- function(roots) {
  files <- detect_csv_tracing(roots = roots)
  read_write_files(files = files)
}

# if we want to improve this, we could supply the starting point as command line args
convert_csv_to_binary("./")