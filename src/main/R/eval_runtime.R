#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")


traces <- load_traces("/Users/janek/Documents/equil-output")
times <- traces %>%
  mutate(func = paste(target, name, sep = "::")) %>%
  mutate(size = str_extract(dir, "\\d+")) %>%
  mutate(busy = parse_duration(time.busy)) %>%
  mutate(idle = parse_duration(time.idle)) %>%
  select(size, func, busy, busy, idle)

run_times <- times %>%
  filter(func == "rust_q_sim::simulation::simulation::run")

a <- blue_to_red()

p <- ggplot(run_times, aes(x = size, y = busy)) +
  geom_boxplot(color = blue(), fill = gray()) +
  theme_light()
p

