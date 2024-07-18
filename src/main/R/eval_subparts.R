#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(patchwork)
library(wesanderson)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

on_load <- function(data) {
  transformed <- data %>%
    filter(bin_start > 0) %>%
    filter(
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
        func == "rust_q_sim::simulation::network::sim_network::move_links" |
        func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
        func == "rust_q_sim::simulation::simulation::wakeup" |
        func == "rust_q_sim::simulation::simulation::terminate_teleportation") %>%
    mutate(func_name = sub(".*::", "", func)) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    mutate(func = func_name) %>%
    select(size, sim_time, rank, func, duration)

  work <- transformed %>%
    filter(func == "wakeup" |
             func == "terminate_teleportation" |
             func == "move_nodes" |
             func == "move_links") %>%
    pivot_wider(names_from = func, values_from = duration) %>%
    mutate(duration = wakeup +
      terminate_teleportation +
      move_nodes +
      move_links) %>%
    mutate(name = "work") %>%
    select(size, sim_time, rank, duration, name)
  wait <- transformed %>%
    filter(func == "receive_msgs") %>%
    mutate(name = "wait") %>%
    select(size, sim_time, rank, duration, name)
  bind_rows(work, wait)
}

#
# wait_work <- read_binary_tracing_files(c(
#   #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-2",
#   #   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-4",
#   #   #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-8",
#   #   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-16",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-32",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-64",
#   #   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-128",
#   #   #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-256",
#   #   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-512",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-1024"
#   #   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-2048",
#   #   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-4096",
#   #   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-8192"
# ), on_load, parallel = TRUE)

wait_work <- read_binary_tracing_files(c(
  #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-8",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-16",
  #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-32",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-64",
  #"/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-256",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-1024"
), on_load, parallel = TRUE)
print(wait_work)

# wait_work <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-empty/output-with-tracing"
#   , on_load, parallel = TRUE)
#wait_work <- wait_work %>%
#  mutate(sim_time = duration)


wait_work_by_size <- wait_work %>%
  group_by(sim_time, size, name) %>%
  summarize(max_dur = max(duration), diff_dur = max(duration) - min(duration), .groups = 'drop') %>%
  filter(diff_dur < 300000)

p <- ggplot(wait_work_by_size, aes(sim_time, max_dur / 1e3, color = as.factor(name))) +
  geom_point(shape = '.') +
  facet_wrap(~size, scales = "fixed") +
  #scale_y_log10() +
  #ylim(0, 3000) +
  ggtitle("Max duration of work and wait per sim step.") +
  ylab("Max. Duration [\u00B5s]") +
  labs(color = "") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1))) +
  scale_color_manual(values = c("#feb70e", "#0e54fe")) +
  theme_light()
ggsave("work-wait-hlrn.pdf", plot = p, device = "pdf", width = 210, height = 118, units = "mm")
ggsave("work-wait-hlrn.png", plot = p, device = "png", width = 210, height = 118, units = "mm")
p

p <- ggplot(wait_work_by_size, aes(sim_time, diff_dur / 1e3, color = as.factor(name))) +
  geom_point(shape = '.') +
  facet_wrap(~size, scales = "fixed") +
  #scale_y_log10() +
  #ylim(0, 600) +
  ggtitle("Difference between max. and min. duration per sim step.") +
  ylab("Max. Duration [\u00B5s]") +
  xlab("Simulation Time") +
  labs(color = "Algorithm phase") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1))) +
  scale_color_manual(values = c("#feb70e", "#0e54fe")) +
  theme_light()
ggsave("work-wait-diff-hlrn.pdf", plot = p, device = "pdf", width = 210, height = 100, units = "mm")
ggsave("work-wait-diff-hlrn.png", plot = p, device = "png", width = 210, height = 100, units = "mm")
p

p <- ggplot(wait_work_by_size, aes(sim_time, max_dur / 1e3, color = as.factor(size))) +
  geom_point(shape = '.') +
  facet_wrap(~name) +
  #scale_y_log10() +
  ylim(0, 400) +
  scale_color_manual(values = qualitative()) +
  ylab("Max. Duration [\u00B5s]") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1))) +
  theme_light()
p

work_16 <- wait_work %>%
  filter(size == 16) %>%
  filter(sim_time <= 86400) %>%
  filter(name == "work") %>%
  group_by(sim_time) %>%
  mutate(diff_to_min = duration - min(duration)) %>%
  ungroup()

work_1024 <- wait_work %>%
  filter(size == 1024) %>%
  filter(sim_time <= 86400) %>%
  filter(name == "work") %>%
  group_by(sim_time) %>%
  mutate(diff_to_min = duration - min(duration)) %>%
  mutate(high_duration = duration > quantile(duration, 0.95)) %>%
  mutate(slowest = duration == max(duration)) %>%
  ungroup()

p_16_diff <- ggplot(work_16, aes(x = sim_time, y = rank, fill = diff_to_min / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(fill = "Diff. [\u00B5s]") +
  ggtitle(paste("Difference to fastest work time for 16 Cores")) +
  theme_light()

p_1024_diff <- ggplot(work_1024, aes(x = sim_time, y = rank, fill = diff_to_min / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(fill = "Diff. [\u00B5s]") +
  ggtitle(paste("Difference to fastest work time for 1024 cores")) +
  theme_light()

combined <- p_16_diff + p_1024_diff
combined

p <- ggplot(work_1024, aes(x = sim_time, y = rank, fill = as.factor(high_duration))) +
  geom_raster() +
  #scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(fill = "Diff. [\u00B5s]") +
  ggtitle(paste("Difference to fastest work time for 1024 cores")) +
  theme_light()
p

p <- ggplot(work_1024, aes(x = sim_time, y = rank, fill = as.factor(slowest))) +
  geom_raster() +
  #scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(fill = "Diff. [\u00B5s]") +
  ggtitle(paste("Difference to fastest work time for 1024 cores")) +
  theme_light()
p


traces <- read_binary_tracing_files(c(
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-1",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-2",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-4",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-8",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-16",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-32",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-64",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-128",
  # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-256",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-1024",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-2048",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-4096",
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-25pct/output-trace-pre-cmp/size-8192"
))

# execution_times <- traces %>%
#   filter(func == "rust_q_sim::simulation::simulation::run") %>%
#   group_by(size) %>%
#   summarize(run_time = mean(median_dur) / 1e9)
#
# ggplot(execution_times, aes(x = size, y = run_time)) +
#   geom_line(color = pink()) +
#   geom_point(color = pink()) +
#   scale_y_log10() +
#   scale_x_log10() +
#   geom_text(aes(label = round(run_time, 1)), vjust = -0.5, hjust = -0.05) +
#   xlab("Number of Cores") +
#   ggtitle("Overall Runtime [s] on Intel® Xeon® Platinum 9242 Processor ") +
#   theme_light()


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
  select(size, sim_time, rank, acc_duration,)

p <- ggplot(acc_work_times, aes(x = sim_time, y = rank, fill = acc_duration / 1e3)) +
  geom_raster() +
  facet_wrap(~size, scales = "free_y") +
  scale_fill_viridis_c(option = "magma", direction = -1, transform = "log10") +
  ggtitle("Busy Times") +
  theme_light()
p
#
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
#
# mean_sim_work <- mean_timings %>%
#   filter(
#     func == "move_nodes" |
#       func == "move_links" |
#       func == "terminate_teleportation" |
#       func == "wakeup"
#   )
#
# ggplot(mean_sim_work, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
#   geom_bar(stat = "identity", position = "stack") +
#   xlab("Number of Cores") +
#   ylab("Mean Duration [\u00B5s]") +
#   ggtitle("Mean duration of performing simulation Work per time step") +
#   scale_fill_manual(values = neon()) +
#   scale_color_manual(values = neon()) +
#   theme_light()
#
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
#
# mean_message_handling <- mean_messaging %>%
#   filter(
#     func == "handle_msgs" | func == "send_msgs"
#   )
#
# ggplot(mean_message_handling, aes(x = factor(size), y = avg_overall / 1e3, fill = as.factor(func))) +
#   geom_bar(stat = "identity", position = "stack") +
#   xlab("Number of Cores") +
#   ylab("Mean Duration [\u00B5s]") +
#   ggtitle("Mean duration of Messaging without waiting times") +
#   scale_fill_manual(values = neon()) +
#   theme_light()

#----------- Plot times over sim time ---------

diff_wait_times <- iteration %>%
  filter(
    func == "receive_msgs"
  ) %>%
  group_by(sim_time, size, func) %>%
  summarize(diff_dur = max(duration) - min(duration))

# I would like to plot the wait times together with a trend line but somehow I can't achieve this....
ggplot(diff_wait_times, aes(x = sim_time, y = diff_dur / 1e3, color = as.factor(size))) +
  geom_point(alpha = 0.8, shape = '.') +
  ylim(0, 750) +
  #scale_y_log10() +
  scale_color_manual(values = qualitative()) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1))) +
  ggtitle("Difference between longest and shortest receive_msgs by size and sim time") +
  xlab("Simulated time [s]") +
  ylab("Difference longest - slowest [\u00B5s] ") +
  labs(color = "# Cores") +
  theme_light()
#
# wait_times <- iteration %>%
#   filter(func == "receive_msgs") %>%
#   filter(size > 1 & size <= 1024)
#
# p <- ggplot(wait_times, aes(x = sim_time, y = duration / 1e3)) + #color = as.factor(rank))) +
#   geom_point(alpha = 0.3, shape = '.') +
#   #geom_smooth(se = TRUE) +
#   facet_wrap(~size) +
#   #ylim(0, 200) +
#   #scale_color_manual(values = many()) +
#   #guides(color = guide_legend(override.aes = list(size = 4, alpha = 1.0, shape = 1))) +
#   xlab("Simulation Time [s]") +
#   ylab("median duration [\u00B5s]") +
#   ggtitle(paste("Median Execution Times for communicators::receive_msgs -", 30, "sim. second bins")) +
#   theme_light()
# #ggsave("./wait-times.png", p, dpi = 320)
# p

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
  select(size, sim_time, rank, acc_duration,)


acc_work_times_2048 <- acc_work_times %>%
  filter(size == 2048)
acc_work_times_8 <- acc_work_times %>%
  filter(size == 8)
acc_work_times_1024 <- acc_work_times %>%
  filter(size == 1024)
# p <- ggplot(acc_work_times_1024, aes(x = sim_time, y = acc_duration / 1e3, color = as.factor(rank))) +
#   geom_point(alpha = 0.6, shape = '.') +
#   scale_y_log10() +
#   #facet_wrap(~size, scale = "free_y") +
#   xlab("Simulation Time [s]") +
#   ylab("duration [\u00B5s]") +
#   ggtitle(paste("Execution time for simulation work (wakeup + teleport + move_nodes + move_links)")) +
#   theme_light() +
#   theme(legend.position = "none")
# p

#acc_work_times_1024_busy <- acc_work_times_1024 %>%
#filter(sim_time > 18000 & sim_time < 86400)

p <- ggplot(acc_work_times, aes(x = sim_time, y = rank, fill = acc_duration / 1e3)) +
  geom_raster() +
  facet_wrap(~size, scales = "free_y") +
  scale_fill_viridis_c(option = "magma", direction = -1, transform = "log10") +
  ggtitle("Busy Times for 1024 cores") +
  theme_light()
p
ggsave("busy-times-1024.pdf", p, width = 16, height = 9)

p <- ggplot(acc_work_times_2048, aes(x = sim_time, y = rank, fill = acc_duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Busy Times for 2048 cores") +
  theme_light()
ggsave("busy-times-2048.pdf", p, width = 16, height = 9)
p

p <- ggplot(acc_work_times_8, aes(x = sim_time, y = rank, fill = acc_duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Busy Times for 8 cores") +
  theme_light()
ggsave("busy-times-8.pdf", p, width = 16, height = 9)
p

wait_times_1024 <- iteration %>%
  filter(func == "receive_msgs") %>%
  filter(size == 1024) %>%
  select(size, sim_time, rank, duration)
wait_times_2028 <- iteration %>%
  filter(func == "receive_msgs") %>%
  filter(size == 2048) %>%
  select(size, sim_time, rank, duration)
wait_times_8 <- iteration %>%
  filter(func == "receive_msgs") %>%
  filter(size == 8) %>%
  select(size, sim_time, rank, duration)
# p <- ggplot(wait_times_1024, aes(x = sim_time, y = duration / 1e3, color = as.factor(rank))) +
#   geom_point(alpha = 0.6, shape = '.') +
#   #facet_wrap(~size, scale = "free_y") +
#   xlab("Simulation Time [s]") +
#   ylab("duration [\u00B5s]") +
#   scale_y_log10() +
#   ggtitle(paste("Wait Times for 1024")) +
#   theme_light() +
#   theme(legend.position = "none")
# p

p <- ggplot(wait_times_8, aes(x = sim_time, y = rank, fill = duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Wait times for 8 cores") +
  theme_light()

p
p <- ggplot(wait_times_8, aes(x = sim_time, y = rank, fill = duration / 1e3)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10") +
  ggtitle("Wait times for 2048 cores") +
  theme_light()
ggsave("wait-times-8.pdf", p, width = 16, height = 9)
p

size <- 64
work_wait_8 <- traces %>%
  filter(bin_start > 0) %>%
  filter(size == size) %>%
  filter(
    func == "rust_q_sim::simulation::network::sim_network::move_links" |
      func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
      func == "rust_q_sim::simulation::simulation::wakeup" |
      func == "rust_q_sim::simulation::simulation::terminate_teleportation" |
      func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
      func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs"
  ) %>%
  mutate(func_name = sub(".*::", "", func)) %>%
  mutate(sim_time = bin_start) %>%
  mutate(func = func_name) %>%
  select(size, sim_time, rank, func, max_dur, median_dur)

work_8 <- work_wait_8 %>%
  filter(func != "receive_msgs") %>%
  group_by(sim_time, rank) %>%
  summarize(sum_max_dur = sum(max_dur), sum_median_dur = sum(median_dur)) %>%
  group_by(sim_time) %>%
  summarize(max_max = max(sum_max_dur), max_median = max(sum_median_dur), diff_max = max(sum_max_dur) - min(sum_max_dur)) %>%
  mutate(name = "work")

wait_8 <- work_wait_8 %>%
  filter(func == "receive_msgs") %>%
  group_by(sim_time) %>%
  summarize(max_max = max(max_dur), max_median = max(median_dur), diff_max = max(max_dur) - min(max_dur)) %>%
  mutate(name = "receive_msgs")

acc_work_wait_8 <- work_8 %>% bind_rows(wait_8)

ggplot(acc_work_wait_8, aes(sim_time, diff_max / 1e3)) +
  geom_point(shape = '.', color = pink()) +
  ylim(0, 1000) +
  facet_wrap(~name) +
  ylab("Diff max and min duration [\u00B5s]") +
  ggtitle(paste("Diff. max - min work and wait times per time step for", size, "cores")) +
  theme_light()

tiles <- work_wait_8 %>%
  filter(func != "receive_msgs") %>%
  group_by(sim_time, rank) %>%
  summarize(max_dur = sum(max_dur), median_dur = sum(median_dur)) %>%
  mutate(func = "work")
tiles <- bind_rows(tiles, work_wait_8 %>%
  filter(func == "receive_msgs") %>%
  mutate(func = "wait"))

p <- ggplot(tiles, aes(x = sim_time, y = rank, fill = median_dur / 1e3)) +
  geom_raster() +
  facet_wrap(~func) +
  scale_fill_viridis_c(option = "magma", direction = -1, transform = "log10") +
  ggtitle(paste("Wait and work times for", size, "cores")) +
  theme_light()
p

#
# # try to plot the 1024 run with neighbor information
# neighbors <- read_csv("/Users/janek/Documents/rust_q_sim/berlin-v6.0-25pct-1024-neighbors.csv")
# neighbors %>%
#   summarize(mean(neighbors))
#
# joined <- wait_times_1024 %>% left_join(neighbors, by = join_by(rank == rank))
#
# p <- ggplot(joined, aes(x = sim_time, y = duration / 1e3, color = as.factor(neighbors))) +
#   geom_point(alpha = 0.6, shape = '.') +
#   #facet_wrap(~size, scale = "free_y") +
#   xlab("Simulation Time [s]") +
#   ylab("duration [\u00B5s]") +
#   scale_y_log10() +
#   scale_color_manual(values = qualitative()) +
#   ggtitle(paste("Wait Times by #neighbors for 1024")) +
#   theme_light() +
#   theme(legend.position = "right") +
#   guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1)))
# #theme(legend.position = "none")
# p
# ggsave("./wait-times-neighbor.png", p)
#
# communication <- traces %>%
#   filter(bin_start > 0) %>%
#   filter(
#     func == "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles"
#   ) %>%
#   mutate(func_name = sub(".*::", "", func)) %>%
#   mutate(sim_time = bin_start) %>%
#   mutate(duration = median_dur) %>%
#   mutate(func = func_name) %>%
#   select(size, sim_time, rank, func, duration)
#
# joined_communication <- communication %>% left_join(neighbors, by = join_by(rank == rank))
#
# p <- ggplot(joined_communication, aes(x = sim_time, y = duration / 1e3, color = as.factor(neighbors))) +
#   geom_point(alpha = 0.6, shape = '.') +
#   #facet_wrap(~size, scale = "free_y") +
#   xlab("Simulation Time [s]") +
#   ylab("duration [\u00B5s]") +
#   scale_y_log10() +
#   scale_color_manual(values = qualitative()) +
#   ggtitle(paste("Communication times by #neighbors for 1024")) +
#   theme_light() +
#   theme(legend.position = "right") +
#   guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 1)))
# p