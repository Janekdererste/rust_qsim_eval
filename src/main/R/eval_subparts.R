#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)

source("./src/main/R/tracing.R")
source("./src/main/R/colors.R")

traces_combined <- load_rust_tracing_data("/Users/janek/Documents/rust_q_sim/berlin/output-1pct", num_cores = 8)

main_functions <- traces_combined %>%
  filter(func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
           func == "rust_q_sim::simulation::network::sim_network::move_links" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
           func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs" |
           func == "rust_q_sim::simulation::simulation::wakeup" |
           func == "rust_q_sim::simulation::simulation::terminate_teleportation")

mean_timings <- main_functions %>%
  filter(sim_time > 0) %>%
  group_by(func_name, size) %>%
  summarize(mean_dur = mean(duration))

ggplot(mean_timings, aes(x = factor(size), y = mean_dur / 1e3, fill = as.factor(func_name))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of performing one sim step") +
  scale_fill_manual(values = qualitative()) +
  scale_color_manual(values = neon()) +
  theme_light()

mean_sim_work <- traces_combined %>%
  filter(
    func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
      func == "rust_q_sim::simulation::network::sim_network::move_links" |
      func == "rust_q_sim::simulation::simulation::wakeup" |
      func == "rust_q_sim::simulation::simulation::terminate_teleportation"
  ) %>%
  group_by(func_name, size) %>%
  summarize(mean_dur = mean(duration))

ggplot(mean_sim_work, aes(x = factor(size), y = mean_dur / 1e3, fill = as.factor(func_name))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of performing simulation Work per time step") +
  scale_fill_manual(values = qualitative()) +
  scale_color_manual(values = neon()) +
  theme_light()


messaging <- traces_combined %>%
  filter(
    func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs")

acc_messaging <- messaging %>%
  group_by(func_name, size) %>%
  summarize(avg_dur = median(duration))


#----------- Plot times over sim time ---------

receive_msg_data <- traces_combined %>%
  filter(
    func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs"
  ) %>%
  group_by(sim_time, size, func_name) %>%
  summarize(diff_dur = max(duration) - min(duration))

# I would like to plot the wait times together with a trend line but somehow I can't achieve this....
ggplot(receive_msg_data, aes(x = sim_time, y = diff_dur / 1e3, color = as.factor(size))) +
  geom_point(alpha = 0.1, shape = '.') +
  geom_smooth() +
  ylim(0, 1000) +
  scale_color_manual(values = qualitative()) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0, shape = 21))) +
  theme_light()

width <- 60
mean_binned <- traces_combined %>%
  filter(
    func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
      func == "rust_q_sim::simulation::network::sim_network::move_links" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs"
  ) %>%
  mutate(
    time_bin = cut_width(sim_time, width = width, boundary = 0, closed = "left"),
    bin_start = floor(sim_time / width) * width
  ) %>%
  group_by(bin_start, size, rank, func_name) %>%
  # We summarize using median, because we have outliers in the data set. Here, we want to show
  # that we structurally get closer to each other in execution time, if we put more cores onto the
  # problem. The outliers should be investigated as well I guess.
  summarize(median_dur = median(duration), .groups = 'drop')

receive_timing <- mean_binned %>%
  filter(size > 1 & size <= 32) %>%
  filter(func_name == "receive_msgs")

ggplot(receive_timing, aes(x = bin_start, y = median_dur / 1e3, color = as.factor(rank))) +
  geom_point(alpha = 0.3, shape = '.') +
  geom_smooth(se = TRUE) +
  facet_wrap(~size, scales = "free_y") +
  ylim(0, 200) +
  scale_color_manual(values = many()) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0))) +
  xlab("Simulation Time [s]") +
  ylab("median duration [\u00B5s]") +
  ggtitle(paste("Median Execution Times for communicators::receive_msgs -", width, "sim. second bins")) +
  theme_light()


diff_receive_msg <- receive_timing %>%
  filter(bin_start < max(bin_start)) %>%
  group_by(bin_start, size) %>%
  summarize(dur_span = max(median_dur) - min(median_dur))

ggplot(diff_receive_msg, aes(x = bin_start, y = dur_span / 1e3, color = as.factor(size))) +
  geom_point(alpha = 0.2, shape = '.') +
  geom_smooth(se = TRUE) +
  ylim(0, 200) +
  scale_color_manual(values = many()) +
  xlab("Simulation Time [s]") +
  ylab("diff execution time [\u00B5s]") +
  ggtitle(paste("Diff execution times slowest and fastest for communicators::receive_msgs -", width, "sim. second bins")) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0))) +
  theme_light()

ggplot(diff_receive_msg, aes(x = bin_start, y = as.factor(size), fill = dur_span / 1e3)) +
  geom_raster()

move_links <- mean_binned %>%
  filter(func_name == "move_links")

ggplot(move_links, aes(x = bin_start, y = median_dur / 1e3, color = as.factor(rank))) +
  geom_point(alpha = 0.5, size = 0.25) +
  facet_wrap(~size) +
  scale_color_manual(values = many()) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0))) +
  xlab("Simulation Time [s]") +
  ylab("median duration [\u00B5s]") +
  ggtitle(paste("Median Execution Times for network::move_links -", width, "sim. second bins")) +
  theme_light()
