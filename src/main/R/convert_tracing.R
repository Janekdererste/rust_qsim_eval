library(parallel)
library(tidyverse)

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

detect_tracing_files <- function(roots, pattern) {

  avail_cores <- parallel::detectCores()
  used_cores <- max(1, avail_cores - 2)

  print("Detecting trace files.")

  files <- lapply(roots, function(root) {
    dirs <- fs::dir_ls(root, recurse = TRUE, type = "directory")
    instrument_dirs <- dirs[basename(dirs) == "instrument"]
    instrument_files <- mclapply(instrument_dirs, function(dir) {
      parent_dir <- basename(dirname(dir))
      instrument_files <- list.files(dir, pattern = pattern, full.names = TRUE)
      lapply(instrument_files, function(file) {
        c(dir = parent_dir, file = file)
      })
    }, mc.cores = used_cores)
    list_flatten(instrument_files)
  })
  list_flatten(files)
}

read_binary_files <- function(files) {
  tibbles <- lapply(files, function(file) {
    print(paste("Start reading file:", file))
    read_rds(file)
  })

  print(paste("Starting to bind", length(tibbles), "tibbles"))
  result <- bind_rows(tibbles)
  return(result)
}

convert_csv_to_binary <- function(roots) {
  files <- detect_tracing_files(roots = roots, pattern = "^instrument_process_\\d+\\.csv$")
  read_write_files(files = files)
}

read_binary_tracing <- function(roots) {
  entries <- detect_tracing_files(roots = roots, pattern = "^instrument_process_\\d+\\.rds")
  files <- lapply(entries, function(entry) { entry["file"] })
  read_binary_files(files)
}

roots <- c(
  "/Users/janek/hlrn/berlin-v6.0-25pct/output-with-tracing/size-2"
  #"/Users/janek/hlrn/berlin-v6.0-25pct/output-with-tracing/size-4"
)

#convert_csv_to_binary(roots = roots)
result <- read_binary_tracing(roots = roots)
print(result)