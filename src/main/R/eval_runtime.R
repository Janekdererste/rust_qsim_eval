#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")


traces <- load_csv_instrument("/Users/janek/Cluster/pqsim_benchmark/berlin-v6.0-25pct/output/", num_cores = 8)
overall_run_time <- traces %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  mutate (speedup = (1/duration) / (1/max(duration))) %>%
  mutate(secs = duration / 1e9)

duration_summary <- overall_run_time %>%
  group_by(size) %>%
  summarize(mean_duration = mean(secs))

speedup_summary <- overall_run_time %>%
  group_by(size) %>%
  summarize(mean_speedup = mean(speedup))

p <- ggplot(duration_summary, aes(x = size, y = mean_duration)) +
  geom_line(color = blue()) +
  geom_point(color = blue()) +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5, hjust = -0.05) +
  ggtitle("Overall Runtime [s]") +
  theme_light()
p

p <- ggplot(speedup_summary, aes(x = size, y = mean_speedup)) +
  geom_line(color = blue()) +
  geom_point(color = blue()) +
  geom_text(aes(label = round(mean_speedup, 1)), vjust = 1.5, hjust = -0.1) +
  ggtitle("Overall Speedup") +
  theme_light()
p



