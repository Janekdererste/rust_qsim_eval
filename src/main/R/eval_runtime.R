#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")


traces <- load_csv_instrument("/Users/janek/Documents/rust_q_sim/equil/output", num_cores = 8)
overall_run_time <- traces %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  mutate (speedup = (1/duration) / (1/max(duration))) %>%
  mutate(secs = duration / 1e9)

p <- ggplot(overall_run_time, aes(x = size, y = secs)) +
  geom_point(color = blue()) +
  stat_summary(fun = mean, color = blue(), geom = "line") +
  theme_light()
p



