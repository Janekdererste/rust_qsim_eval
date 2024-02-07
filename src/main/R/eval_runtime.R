#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")

traces <- load_rust_tracing_data("/Users/janek/Cluster/pqsim_benchmark/rvr-v1.4-10pct/output", num_cores = 8)
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
  geom_line(color = pink()) +
  geom_point(color = pink()) +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5, hjust = -0.05) +
  xlab("Number of Cores") +
  ggtitle("RVR-v1.4 10% Scenario - Overall Runtime [s] on 2 x 24-Core Epyc 7352") +
  theme_light()
p

p <- ggplot(speedup_summary, aes(x = size, y = mean_speedup)) +
  geom_line(color = blue()) +
  geom_point(color = blue()) +
  geom_text(aes(label = round(mean_speedup, 1)), vjust = 1.5, hjust = -0.1) +
  xlab("Number of Cores") +
  ggtitle("RVR-v1.4 10% Scenario - Overall Speedup on 2 x 24-Core Epyc 7352") +
  theme_light()
p

#------------------------ Load matsim classic ---------------
matsim_traces <- load_matsim_tracing_data("/Users/janek/Cluster/matsim-benchmark/rvr-v1.4-10pct/output-10pct", num_cores = 8)
matsim_traces <- matsim_traces %>%
  mutate(secs = duration / 1e9) %>%
  mutate(speedup = (1/duration) / (1/max(duration)))

p <- ggplot(matsim_traces, aes(x = size, y = secs)) +
  geom_line(color = blue()) +
  geom_point(color = blue()) +
  geom_text(aes(label = round(secs, 1)), vjust = -0.5, hjust = -0.05) +
  xlab("Number of Cores") +
  ggtitle("RVR-v1.4 10% Scenario - Overall Runtime Java-Qsim [s] on 2 x 24-Core Epyc 7352") +
  theme_light()
p

#-------------------- Compare classic and rust runtimes

matsim_durations <- matsim_traces %>%
  select(secs, size)
rust_durations <- duration_summary %>%
  mutate(secs = mean_duration) %>%
  select(secs, size)

joined <- matsim_durations %>%
  left_join(rust_durations, by = join_by(size), suffix = c(".matsim", ".rust")) %>%
  pivot_longer(cols = c(ends_with(".matsim"), ends_with(".rust")), values_to = "secs") %>%
  filter(!is.na(secs))

p <- ggplot(joined, aes(x = size, y = secs, color = name)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +  # Add this line for log scaling on the y-axis +
  scale_x_log10() +
  xlab("Number of Cores") +
  ggtitle("RVR-v1.4 10% Scenario - Runtimes Java vs. Rust on 2 x 24-Core Epyc 7352") +
  theme_light() +
  scale_color_manual(values = neon())
p

ratios <- joined %>%
  group_by(size) %>%
  mutate(ratio = secs[name == "secs.matsim"] / secs[name == "secs.rust"]) %>%
  ungroup() %>%
  filter(name == "secs.rust") %>%
  select(size, ratio)

p <- ggplot(ratios, aes(x = size, y = ratio)) +
  geom_line(color = gray()) +
  geom_point(color = gray()) +
  xlab("Number of Cores") +
  ggtitle("RVR-v1.4 10% Scenario - Speedup Rust vs. Java on 2 x 24-Core Epyc 7352)") +
  theme_light()
p



