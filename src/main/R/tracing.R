library(parallel)
library(tidyverse)

source("./src/main/R/parsing.R")

trace_dirs <- function(roots) {
  lapply(roots, function(root) {
    sub_dirs <- list.dirs(root)
    trace_directories <- sub_dirs[grep("/instrument", sub_dirs)]
    return(trace_directories)
  })
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
  tibbles <- mclapply(files, function(entry) {

    file_tibble <- tibble(size = numeric(), rank = numeric(), func = character(), busy = numeric(), idle = numeric())
    size <- as.numeric(str_extract(entry["dir"], "\\d+"))

    jsonlite::stream_in(file(entry["file"]), handler = function(traces) {
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

load_rust_data <- function(files, num_cores) {

  tibbles <- lapply(files, function(entry) {

    file_path <- entry["file"]
    print(paste("reading file: ", file_path))
    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
      return(tibble())  # return an empty tibble if the file does not exist
    }

    size <- as.numeric(str_extract(entry["dir"], "\\d+"))
    file_data <- read_csv(file_path)
    if (nrow(file_data) == 0 || ncol(file_data) != 6) {
      warning(paste("Failed to load tibble from file:", file_path, "The tibble looks like below:"))
      print(file_data)
      stop("Aborting reading process!")
    }
    file_data <- file_data %>%
      mutate(size = size) %>%
      mutate(func = paste(target, func_name, sep = "::"))
    return(file_data)
  }) #, mc.cores = num_cores, mc.preschedule = TRUE)
  print(paste("Starting to bind ", length(tibbles), " tibbles"))
  result <- bind_rows(tibbles)
  return(result)
}

load_matsim_data <- function(files, num_cores) {
  tibbles <- lapply(files, function(file) {
    if (!file.exists(file)) {
      warning(paste("File does not exist: ", file))
      return(tibble())
    }

    file_data <- read_csv(file)
    return(file_data)
  }) #, mc.cores = num_cores, mc.preschedule = TRUE)
  result <- bind_rows(tibbles)
  return(result)
}

convert_to_binary <- function(roots, num_cores = 1) {
  print("Detecting directories")
  dirs <- trace_dirs(roots)
  print("Detecting tracing files")
  files <- trace_files_in_folders(dirs)
  convert_tracing_files(files)
}

convert_tracing_files <- function(files) {
  avail_cores <- parallel::detectCores()
  used_cores <- max(1, avail_cores - 2)
  mclapply(files, function(entry) {

    file_path <- entry["file"]
    print(paste("reading file: ", file_path))
    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
    }

    size <- as.numeric(str_extract(entry["dir"], "\\d+"))
    file_data <- read_csv(file_path)
    if (nrow(file_data) == 0 || ncol(file_data) != 6) {
      warning(paste("Failed to load tibble from file:", file_path, "The tibble looks like below:"))
      print(file_data)
      stop("Aborting reading process!")
    }
    width <- 30
    file_data <- file_data %>%
      mutate(size = size) %>%
      mutate(func = paste(target, func_name, sep = "::")) %>%
      mutate(
        time_bin = cut_width(sim_time, width = width, boundary = 0, closed = "left"),
        bin_start = floor(sim_time / width) * width
      ) %>%
      group_by(bin_start, size, rank, func_name) %>%
      summarize(median_dur = median(duration), .groups = "drop")

    #filter(func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
    #         func == "rust_q_sim::simulation::network::sim_network::move_links" |
    #         func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
    #         func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
    #         func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs" |
    #         func == "rust_q_sim::simulation::simulation::run")

    out_file_path <- sub("\\.csv$", ".rds", file_path)

    write_rds(file_data, file = out_file_path, compress = "none", version = 3, text = FALSE)
  }, mc.cores = used_cores)
}

load_rust_tracing_data <- function(root, num_cores = 1, file_name_filter = c(), node_count_filter = -1) {
  dirs <- trace_dirs(root)
  files <- trace_files_in_folders(dirs)
  if (length(file_name_filter) > 0) {
      files <- files[grep(file_name_filter, files)]
  }
  if (node_count_filter > 0) {
    files <- files[sapply(files, function(x) endsWith(x["dir"], paste0("-",node_count_filter)))]
  }
  data <- load_rust_data(files, num_cores)
}

load_matsim_tracing_data <- function(root, num_cores = 1) {
  files <- list.files(path = root, pattern = "instrument-mobsim\\.csv$", recursive = TRUE, full.names = TRUE)
  data <- load_matsim_data(files, num_cores)
}

load_json <- function(root) {
  dirs <- trace_dirs(root)
  files <- trace_files_in_folders(dirs)
  traces <- stream_json(files)
}