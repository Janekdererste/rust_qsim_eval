#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

trace_dirs <- function(root) {
  sub_dirs <- list.dirs(root)
  trace_directories <- sub_dirs[grep("/trace$", sub_dirs)]
  return(trace_directories)
}

trace_files <- function(trace_dirs) {

  result <- list()

  for (dir in trace_dirs) {
    parent_dir <- basename(dirname(dir))
    trace_files <- list.files(dir)
    ###}
    result[[parent_dir]] <- trace_files
  }
  return(result)
}

paths <- trace_dirs("/Users/janek/Documents/equil-output")
files <- trace_files(paths)



ndjson <- jsonlite::stream_in(file("/Users/janek/Documents/equil-output/output-3/trace/trace_process_0.txt"))

p <- ggplot(ndjson, aes(x = factor(target), y = fields$time.busy)) +
  geom_point() +
  theme_light()
p

