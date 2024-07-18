library(parallel)

detect_tracing <- function(roots, pattern) {

  avail_cores <- parallel::detectCores()
  used_cores <- max(1, avail_cores - 2)

  print("Detecting trace files.")

  files <- lapply(roots, function(root) {
    dirs <- fs::dir_ls(root, recurse = TRUE, type = "directory")
    instrument_files <- mclapply(dirs, function(dir) {
      parent_dir <- basename(dirname(dir))
      # we have read directories recursively, so only select dir/file pairs which are direct
      # relatives
      instrument_files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = FALSE)
      lapply(instrument_files, function(file) {
        c(dir = parent_dir, file = file)
      })
    }, mc.cores = used_cores)
    list_flatten(instrument_files)
  })
  list_flatten(files)
}

detect_binary_tracing <- function(roots) {
  detect_tracing(roots = roots, pattern = "^instrument_process_\\d+\\.rds")
}

detect_csv_tracing <- function(roots) {
  detect_tracing(roots = roots, pattern = "^instrument_process_\\d+\\.csv")
}

detect_matsim_tracing <- function(roots) {
  detect_tracing(roots = roots, pattern = "^.+\\.instrument-mobsim\\.csv$")
}

detect_neighbor_files <- function(roots) {
  detect_tracing(roots = roots, pattern = "neighbors\\.csv")
}