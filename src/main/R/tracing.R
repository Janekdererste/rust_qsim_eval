trace_dirs <- function(root) {
  sub_dirs <- list.dirs(root)
  trace_directories <- sub_dirs[grep("/trace$", sub_dirs)]
  return(trace_directories)
}

trace_files_in_folders <- function(trace_dirs) {

  result <- list()

  for (dir in trace_dirs) {
    parent_dir <- basename(dirname(dir))
    trace_files <- list.files(dir, full.names = TRUE)
    result[[parent_dir]] <- trace_files
  }
  return(result)
}

flat_trace_files <- function(trace_files) {

  flat_list <- trace_files |>
    imap(function(filenames, parent_dir) {
      files <- filenames |>
        map(function(filename) {
          return(c(dir = parent_dir, file = filename))
        })
      return (files)
    }) |>
    list_flatten()

  return(flat_list)
}

traces <- function(flat_trace_files) {

  tibbles <- flat_trace_files |>
    map(function(entry) {
      jsonlite::stream_in(file(entry["file"])) %>%
        add_column(dir = entry["dir"])
    })
  tibbles %>% bind_rows()
}

load_traces <- function(root) {
  paths <- trace_dirs(root)
  files <- trace_files_in_folders(paths)
  flat_files <- flat_trace_files(files)
  traces(flat_files) %>%
    unnest(fields) %>%
    unnest(span)
}