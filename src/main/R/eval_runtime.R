#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

rvr_traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "rvr-10%")

rvr_1pct_traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-1pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "rvr-1%")

berlin_traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "berlin-25%")

berlin_in_link_traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "berlin-25%-in-link-cap")

berlin_1pct_traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-1pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "berlin-1%")

matsim_traces <- read_matsim_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/matsim-benchmark") %>%
  mutate(run_time = duration / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  select(size, run_time, rtr) %>%
  mutate(name = "matsim-rvr-10%")

combined <- bind_rows(rvr_traces, berlin_in_link_traces, rvr_1pct_traces, berlin_traces, berlin_1pct_traces, matsim_traces)

ggplot(combined, aes(x = size, y = run_time, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(run_time, 1)), vjust = -0.5, hjust = -0.05) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("Overall Runtime [s] on Intel® Xeon® Platinum 9242 Processor ") +
  theme_light()

ggplot(combined, aes(x = size, y = rtr, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(rtr, 1)), vjust = -0.5, hjust = -0.05) +
  scale_color_manual(values = qualitative()) +
  xlab("Number of Cores") +
  ggtitle("Real Time Ratio on Intel® Xeon® Platinum 9242 Processor ") +
  theme_light()



