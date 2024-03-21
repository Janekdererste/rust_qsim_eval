#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

traces <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-with-tracing")

execution_times <- traces %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9)

ggplot(execution_times, aes(x = size, y = run_time)) +
  geom_line(color = pink()) +
  geom_point(color = pink()) +
  scale_y_log10() +
  scale_x_log10() +
  geom_text(aes(label = round(run_time, 1)), vjust = -0.5, hjust = -0.05) +
  xlab("Number of Cores") +
  ggtitle("Overall Runtime [s] on Intel® Xeon® Platinum 9242 Processor ") +
  theme_light()

iteration <- traces %>%
  filter(bin_start > 0) %>%
  filter(
    func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs" |
      func == "rust_q_sim::simulation::network::sim_network::move_links" |
      func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
      func == "rust_q_sim::simulation::simulation::wakeup" |
      func == "rust_q_sim::simulation::simulation::terminate_teleportation"
  ) %>%
  mutate(func_name = sub(".*::", "", func)) %>%
  mutate(sim_time = bin_start) %>%
  mutate(duration = median_dur) %>%
  mutate(func = func_name) %>%
  select(size, sim_time, rank, func, duration)

order <- c("handle_msgs", "receive_msgs", "send_msgs", "move_nodes", "move_links", "terminate_teleportation", "wakeup")

mean_timings <- iteration %>%
  group_by(func, size) %>%
  summarize(avg_overall = mean(duration), .groups = "drop") %>%
  mutate(func = fct_relevel(func, order))

ggplot(mean_timings, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of performing one sim step") +
  scale_fill_manual(values = qualitative()) +
  scale_color_manual(values = neon()) +
  theme_light()

mean_sim_work <- mean_timings %>%
  filter(
    func == "move_nodes" |
      func == "move_links" |
      func == "terminate_teleportation" |
      func == "wakeup"
  )

ggplot(mean_sim_work, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of performing simulation Work per time step") +
  scale_fill_manual(values = neon()) +
  scale_color_manual(values = neon()) +
  theme_light()

mean_messaging <- mean_timings %>%
  filter(
    func == "handle_msgs" |
      func == "receive_msgs" |
      func == "send_msgs"
  )

ggplot(mean_messaging, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of performing messaging per time step") +
  scale_fill_manual(values = neon()) +
  theme_light()

mean_message_handling <- mean_messaging %>%
  filter(
    func == "handle_msgs" | func == "send_msgs"
  )

ggplot(mean_message_handling, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  ggtitle("Mean duration of Messaging without waiting times") +
  scale_fill_manual(values = neon()) +
  theme_light()

#----------- Plot times over sim time ---------

diff_wait_times <- iteration %>%
  filter(
    func == "receive_msgs"
  ) %>%
  group_by(sim_time, size, func) %>%
  summarize(diff_dur = max(duration) - min(duration))

# I would like to plot the wait times together with a trend line but somehow I can't achieve this....
ggplot(diff_wait_times, aes(x = sim_time, y = diff_dur / 1e3, color = as.factor(size))) +
  geom_point(alpha = 0.5, shape = '.') +
  #geom_smooth() +
  #ylim(0, 1000) +
  scale_color_manual(values = qualitative()) +
  scale_fill_manual(values = qualitative()) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1))) +
  ggtitle("Difference between longest and shortest receive_msgs by size and sim time") +
  xlab("Simulated time [s]") +
  ylab("Difference longest - slowest [\u00B5s] ") +
  labs(color = "# Cores") +
  theme_light()

wait_times <- iteration %>%
  filter(func == "receive_msgs") %>%
  filter(size > 1 & size <= 1024)

p <- ggplot(wait_times, aes(x = sim_time, y = duration / 1e3)) + #color = as.factor(rank))) +
  geom_point(alpha = 0.3, shape = '.') +
  #geom_smooth(se = TRUE) +
  facet_wrap(~size) +
  #ylim(0, 200) +
  #scale_color_manual(values = many()) +
  #guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0, shape = 1))) +
  xlab("Simulation Time [s]") +
  ylab("median duration [\u00B5s]") +
  ggtitle(paste("Median Execution Times for communicators::receive_msgs -", 30, "sim. second bins")) +
  theme_light()
ggsave("./wait-times.png", p, dpi = 320)
p

acc_work_times <- iteration %>%
  filter(func == "wakeup" |
           func == "terminate_teleportation" |
           func == "move_nodes" |
           func == "move_links") %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(acc_duration = wakeup +
    terminate_teleportation +
    move_nodes +
    move_links) %>%
  select(size, sim_time, rank, acc_duration)

acc_work_times_1024 <- acc_work_times %>%
  filter(size == 1024)

p <- ggplot(acc_work_times_1024, aes(x = sim_time, y = acc_duration / 1e3, color = as.factor(rank))) +
  geom_point(alpha = 0.6, shape = '.') +
  scale_y_log10() +
  #facet_wrap(~size, scale = "free_y") +
  xlab("Simulation Time [s]") +
  ylab("duration [\u00B5s]") +
  ggtitle(paste("Execution time for simulation work (wakeup + teleport + move_nodes + move_links)")) +
  theme_light() +
  theme(legend.position = "none")
p

#acc_work_times_1024_busy <- acc_work_times_1024 %>%
#filter(sim_time > 18000 & sim_time < 86400)

p <- ggplot(acc_work_times_1024, aes(x = sim_time, y = rank, fill = acc_duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Busy Times for 1024 cores") +
  theme_light()
p

wait_times_1024 <- iteration %>%
  filter(func == "receive_msgs") %>%
  filter(size == 1024) %>%
  select(size, sim_time, rank, duration)

p <- ggplot(wait_times_1024, aes(x = sim_time, y = duration / 1e3, color = as.factor(rank))) +
  geom_point(alpha = 0.6, shape = '.') +
  #facet_wrap(~size, scale = "free_y") +
  xlab("Simulation Time [s]") +
  ylab("duration [\u00B5s]") +
  scale_y_log10() +
  ggtitle(paste("Wait Times for 1024")) +
  theme_light() +
  theme(legend.position = "none")
p

p <- ggplot(wait_times_1024, aes(x = sim_time, y = rank, fill = duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Wait times for 1024 cores") +
  theme_light()
p

# try to plot the 1024 run with neighbor information
neighbors <- read_csv("/Users/janek/Documents/rust_q_sim/berlin-v6.0-25pct-1024-neighbors.csv")
neighbors %>%
  summarize(mean(neighbors))

joined <- wait_times_1024 %>% left_join(neighbors, by = join_by(rank == rank))

p <- ggplot(joined, aes(x = sim_time, y = duration / 1e3, color = as.factor(neighbors))) +
  geom_point(alpha = 0.6, shape = '.') +
  #facet_wrap(~size, scale = "free_y") +
  xlab("Simulation Time [s]") +
  ylab("duration [\u00B5s]") +
  scale_y_log10() +
  scale_color_manual(values = qualitative()) +
  ggtitle(paste("Wait Times by #neighbors for 1024")) +
  theme_light() +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1)))
#theme(legend.position = "none")
p
ggsave("./wait-times-neighbor.png", p)

communication <- traces %>%
  filter(bin_start > 0) %>%
  filter(
    func == "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles"
  ) %>%
  mutate(func_name = sub(".*::", "", func)) %>%
  mutate(sim_time = bin_start) %>%
  mutate(duration = median_dur) %>%
  mutate(func = func_name) %>%
  select(size, sim_time, rank, func, duration)

joined_communication <- communication %>% left_join(neighbors, by = join_by(rank == rank))

p <- ggplot(joined_communication, aes(x = sim_time, y = duration / 1e3, color = as.factor(neighbors))) +
  geom_point(alpha = 0.6, shape = '.') +
  #facet_wrap(~size, scale = "free_y") +
  xlab("Simulation Time [s]") +
  ylab("duration [\u00B5s]") +
  scale_y_log10() +
  scale_color_manual(values = qualitative()) +
  ggtitle(paste("Communication times by #neighbors for 1024")) +
  theme_light() +
  theme(legend.position = "right") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1)))
p