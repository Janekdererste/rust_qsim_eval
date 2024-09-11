#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(patchwork)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

function_names <- c("rust_q_sim::simulation::messaging::communication::communicators::send_msgs",
                    "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs",
                    "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs",
                    "rust_q_sim::simulation::simulation::wakeup",
                    "rust_q_sim::simulation::simulation::terminate_teleportation",
                    "rust_q_sim::simulation::network::sim_network::move_nodes",
                    "rust_q_sim::simulation::network::sim_network::move_links",
                    "rust_q_sim::simulation::messaging::events::finish",
                    "rust_q_sim::simulation::simulation::run")
labels <- c("Send", "Receive", "Handle", "Finish activities", "Teleport", "Move nodes", "Move links", "Finish", "Run")
function_filter <- c("Send", "Receive", "Handle", "Finish activities", "Teleport", "Move nodes", "Move links", "Run")
work_filter <- c("Finish activities", "Teleport", "Move nodes", "Move links")
comm_filter <- c("Send", "Receive", "Handle")
func_to_label <- setNames(labels, function_names)

match_func_labels <- function(func_value) {
  func_to_label[[func_value]]
}

on_load_tracing <- function(data) {
  data %>%
    filter(func %in% function_names) %>%
    mutate(func = sapply(func, match_func_labels)) %>%
    #filter(bin_start > 0) %>%
    filter(func %in% function_filter) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    select(size, sim_time, rank, func, duration)
}

data_tracing_1024 <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-102.4/",
  on_load = on_load_tracing,
  parallel = TRUE) %>%
  mutate(name = "102.4")
data_no_tracing_1024 <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-runtimes/instrument-102.4") %>%
  mutate(name = "no tracing") %>%
  select(size, mean_dur, name)

runtimes_tracing_1024 <- data_tracing_1024 %>%
  filter(func == "Run") %>%
  group_by(size) %>%
  summarize(mean_dur = mean(duration)) %>%
  ungroup() %>%
  mutate(name = "with tracing") %>%
  select(size, mean_dur, name)
runtimes_1024 <- bind_rows(runtimes_tracing_1024, data_no_tracing_1024)

p <- ggplot(runtimes_1024, aes(x = size, y = mean_dur / 1e9, color = name)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Runtime RVR 102.4% with and without tracing enabled") +
  theme_light()
p

# data_tracing_512 <- read_binary_tracing_files(
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-51.2/",
#   on_load = on_load_tracing,
#   parallel = TRUE) %>%
#   mutate(name = "51.2")
# data_tracing_256 <- read_binary_tracing_files(
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-25.6/",
#   on_load = on_load_tracing,
#   parallel = TRUE) %>%
#   mutate(name = "25.6")
# data_tracing_128 <- read_binary_tracing_files(
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-12.8/",
#   on_load = on_load_tracing,
#   parallel = TRUE) %>%
#   mutate(name = "12.8")
data_tracing_100 <- read_binary_tracing_files(
  "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-10.0/",
  on_load = on_load_tracing,
  parallel = TRUE) %>%
  mutate(name = "10.0")
# data_tracing_64 <- read_binary_tracing_files(
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/scaling-tracing/instrument-6.4/",
#   on_load = on_load_tracing,
#   parallel = TRUE) %>%
#   mutate(name = "6.4")
data_tracing_all <- bind_rows(
  #data_tracing_64,
  data_tracing_100,
  #data_tracing_128,
  #data_tracing_256,
  #data_tracing_512,
  #data_tracing_1024
)

rm(data_tracing_64)
rm(data_tracing_100)
#rm(data_tracing_128)
rm(data_tracing_256)
#rm(data_tracing_512)
rm(data_tracing_1024)

runtime_100_1 <- data_tracing_all %>%
  filter(size == 1) %>%
  filter(func == "Run") %>%
  filter(name == "10.0") %>%
  pull(duration) %>%
  as.numeric()

func <- function(p) {
  t_cmp <- runtime_100_1 * 1e-9 / p
  N_nb <- 2 * (3 * sqrt(p) - 1) * (sqrt(p) - 1) / p
  #N_nb <- 10
  t_lt <- 2 * N_nb * 1e-6 * 129600
  return(t_cmp + t_lt)
}

runtimes <- data_tracing_all %>%
  filter(func == "Run") %>%
  filter(name == "10.0" | name == "102.4") %>%
  group_by(size, name) %>%
  summarize(duration = mean(duration), .groups = "drop")

p <- ggplot(runtimes, aes(x = size, y = duration / 1e9, color = name)) +
  geom_point() +
  geom_line() +
  stat_function(fun = func, color = "black") +
  #stat_function(fun = func, color = "#F0A202") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Runtime RVR 102.4% with and without tracing enabled") +
  theme_light()
p


colors_msging <- c("#feb70e", "#fece5a", "#fee5a6")
colors_work <- c("#0e54fe", "#4178fe", "#739cfe", "#a6c0fe")
order <- c("Receive", "Send", "Handle", "Move nodes", "Move links", "Finish activities", "Teleport")

# sub_parts_1024 <- data_tracing_all %>%
#   filter(name == "102.4") %>%
#   filter(func %in% order) %>%
#   group_by(size, name, func) %>%
#   summarize(mean_dur = mean(duration), sum_dur = sum(duration)) %>%
#   mutate(func = fct_relevel(func, order))
#
# p <- ggplot(sub_parts_1024, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_fill_manual(values = c(colors_msging, colors_work)) +
#   xlab("Number of Processes") +
#   ylab("Mean Duration [\u00B5s]") +
#   labs(fill = "Functions") +
#   ggtitle("Mean duration of performing one sim step for 102.4% sample size") +
#   theme_light()
# p

sub_parts_100 <- data_tracing_all %>%
  filter(name == "10.0") %>%
  filter(func %in% order) %>%
  group_by(size, name, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration)) %>%
  mutate(func = fct_relevel(func, order))

p <- ggplot(sub_parts_100, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c(colors_msging, colors_work)) +
  xlab("Number of Processes") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Functions") +
  ggtitle("Mean duration of performing one sim step for 10.0% sample size") +
  theme_light()
p
#
# sub_parts <- data_tracing_all %>%
#   filter(size == 1024) %>%
#   filter(func %in% order) %>%
#   group_by(size, name, func) %>%
#   summarize(mean_dur = mean(duration), sum_dur = sum(duration), .groups = "drop") %>%
#   mutate(func = fct_relevel(func, order)) %>%
#   mutate(name = as.numeric(name)) %>%
#   arrange(name)
#
# p <- ggplot(sub_parts, aes(x = as.factor(name), y = mean_dur / 1e3, fill = as.factor(func))) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_fill_manual(values = c(colors_msging, colors_work)) +
#   xlab("Sample Size") +
#   ylab("Average share of function") +
#   labs(fill = "Functions") +
#   ggtitle("Shares of performing one sim step with 1024 processes") +
#   theme_light()
# p
# p <- ggplot(sub_parts, aes(x = as.factor(name), y = mean_dur / 1e3, fill = as.factor(func))) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = c(colors_msging, colors_work)) +
#   xlab("Sample Size") +
#   ylab("Mean Duration [\u00B5s]") +
#   labs(fill = "Functions") +
#   ggtitle("Mean duration of performing one sim step with 1024 processes") +
#   theme_light()
# p
#
# ## now plot the slowest work and the diff for the 102.4% sample
# work_1024 <- data_tracing_all %>%
#   filter(name == "102.4") %>%
#   filter(func %in% work_filter) %>%
#   pivot_wider(names_from = func, values_from = duration) %>%
#   mutate(duration = `Finish activities` +
#     `Teleport` +
#     `Move nodes` +
#     `Move links`) %>%
#   group_by(sim_time, size) %>%
#   summarize(
#     slowest = max(duration),
#     fastest = min(duration),
#     diff_to_med = max(duration) - median(duration),
#     median = median(duration),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = c("slowest", "fastest", "diff_to_med", "median"), values_to = "duration", names_to = "type")
#
# p <- ggplot(work_1024, aes(x = sim_time, y = duration / 1e3, color = as.factor(type))) +
#   geom_point(shape = '.', alpha = 0.5) +
#   facet_wrap(~size, scales = "free") +
#   ggtitle("Time spent in 'work' phase for 102.4% sample with different #Processes") +
#   ylab("Duration of 'work' phase [\u00B5s]") +
#   guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
#   theme_light()
# p

## now plot the slowest work and the diff for the 102.4% sample
work_100 <- data_tracing_all %>%
  filter(name == "10.0") %>%
  filter(func %in% work_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = `Finish activities` +
    `Teleport` +
    `Move nodes` +
    `Move links`) %>%
  group_by(sim_time, size) %>%
  summarize(
    slowest = max(duration),
    fastest = min(duration),
    diff = (max(duration) - min(duration)) / 1e3,
    rel_diff = (max(duration) / min(duration)),
    median = median(duration),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("slowest", "fastest", "rel_diff", "diff", "median"), values_to = "duration", names_to = "type") %>%
  filter(type == "rel_diff" | type == "diff") %>%
  filter(size == 16 | size == 64 | size == 1024)

p <- ggplot(work_100, aes(x = sim_time, y = duration, color = as.factor(type))) +
  geom_point(shape = '.', alpha = 0.5) +
  facet_wrap(~size, scales = "fixed") +
  #ylim(0, 0.01) +
  ggtitle("Time spent in 'work' phase for 10.0% sample with different #Processes") +
  ylab("Duration of 'work' phase [\u00B5s]") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
  theme_light()
p

# ## now plot the slowest work and the diff for the all sample sizes sample
# work_all <- data_tracing_all %>%
#   filter(size == 1024) %>%
#   filter(func %in% work_filter) %>%
#   pivot_wider(names_from = func, values_from = duration) %>%
#   mutate(duration = `Finish activities` +
#     `Teleport` +
#     `Move nodes` +
#     `Move links`) %>%
#   group_by(sim_time, name) %>%
#   summarize(
#     slowest = max(duration),
#     fastest = min(duration),
#     diff_to_med = max(duration) - median(duration),
#     median = median(duration),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = c("slowest", "fastest", "diff_to_med", "median"), values_to = "duration", names_to = "type") %>%
#   mutate(name = as.numeric(name)) %>%
#   arrange(name)
#
# p <- ggplot(work_all, aes(x = sim_time, y = duration / 1e3, color = as.factor(type))) +
#   geom_point(shape = '.', alpha = 0.5) +
#   facet_wrap(~name, scales = "fixed") +
#   ggtitle("Time spent in 'work' phase with 1024 Processes for different sample sizes") +
#   ylab("Duration of 'work' phase [\u00B5s]") +
#   guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
#   theme_light()
# p

# comm_1024 <- data_tracing_all %>%
#   filter(name == "102.4") %>%
#   filter(func %in% comm_filter) %>%
#   pivot_wider(names_from = func, values_from = duration) %>%
#   mutate(duration = `Send` + `Receive` + `Handle`) %>%
#   group_by(sim_time, size) %>%
#   summarize(
#     slowest = max(duration),
#     fastest = min(duration),
#     diff_min_max = max(duration) - min(duration),
#     median = median(duration),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = c("slowest", "fastest", "diff_min_max", "median"), values_to = "duration", names_to = "type") %>%
#   arrange(size)
#
# p <- ggplot(comm_1024, aes(x = sim_time, y = duration / 1e3, color = as.factor(type))) +
#   geom_point(shape = '.', alpha = 0.5) +
#   facet_wrap(~size, scales = "free") +
#   ggtitle("Time spent in 'communicaton' phase for different #Processes for 102.4% sample") +
#   ylab("Duration of 'communication' phase [\u00B5s]") +
#   guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
#   theme_light()
# p

comm_100 <- data_tracing_all %>%
  filter(name == "10.0") %>%
  filter(func %in% comm_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = `Send` + `Receive` + `Handle`) %>%
  group_by(sim_time, size) %>%
  summarize(
    slowest = max(duration),
    fastest = min(duration),
    diff_min_max = max(duration) - min(duration),
    median = median(duration),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("slowest", "fastest", "diff_min_max", "median"), values_to = "duration", names_to = "type") %>%
  filter(size == 16 | size == 64 | size == 1024) %>%
  filter(type == "slowest") %>%
  arrange(size)

p <- ggplot(comm_100, aes(x = sim_time, y = duration / 1e3, color = as.factor(type))) +
  geom_point(shape = '.', alpha = 0.5) +
  facet_wrap(~size, scales = "fixed") +
  ggtitle("Time spent in 'communicaton' phase for different #Processes for 10.0% sample") +
  ylab("Duration of 'communication' phase [\u00B5s]") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
  theme_light()
p

comm_all <- data_tracing_all %>%
  filter(size == 1024) %>%
  filter(func %in% comm_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = `Send` + `Receive` + `Handle`) %>%
  group_by(sim_time, name) %>%
  summarize(
    slowest = max(duration),
    fastest = min(duration),
    diff_to_med = max(duration) - median(duration),
    median = median(duration),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("slowest", "fastest", "diff_to_med", "median"), values_to = "duration", names_to = "type") %>%
  mutate(name = as.numeric(name)) %>%
  arrange(name)

p <- ggplot(comm_all, aes(x = sim_time, y = duration / 1e3, color = as.factor(type))) +
  geom_point(shape = '.', alpha = 0.5) +
  facet_wrap(~name, scales = "fixed") +
  ggtitle("Time spent in 'communicaton' phase for 1024 Processes for different sample sizes") +
  ylab("Duration of 'communication' phase [\u00B5s]") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
  theme_light()
p
