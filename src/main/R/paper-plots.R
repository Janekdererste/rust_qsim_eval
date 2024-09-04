library(tidyverse)
library(scales)
library(extrafont)
source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

# Import and load fonts
#extrafont::font_import(prompt = FALSE)
#fonts() this would list all installed fonts
#loadfonts(device = "pdf")

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
function_filter <- c("Send", "Receive", "Handle", "Finish activities", "Teleport", "Move nodes", "Move links")
work_filter <- c("Finish activities", "Teleport", "Move nodes", "Move links")
comm_filter <- c("Send", "Receive", "Handle")
func_to_label <- setNames(labels, function_names)

match_func_labels <- function(func_value) {
  func_to_label[func_value]
}

on_load_tracing <- function(data) {
  data %>%
    mutate(func = sapply(func, match_func_labels)) %>%
    filter(bin_start > 0) %>%
    filter(func %in% function_filter) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    select(size, sim_time, rank, func, duration)
}

plot_line <- function(data, x_var, y_var, color_var, title, x_label, y_label) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
    geom_line() +
    geom_point() +
    #geom_label(data = min_rtr, aes(label = rtr), vjust = -0.3, hjust = 0.35, show.legend = FALSE) +
    #geom_label(data = max_rtr, aes(label = rtr), vjust = 1.3, hjust = 0.25, show.legend = FALSE) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
    scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
    scale_color_manual(values = palette()) +
    xlab(x_label) +
    ylab(y_label) +
    labs(color = "Setup") +
    ggtitle(title) +
    theme_light(base_size = 10) +
    theme(legend.position = "inside",
          legend.justification = c(0.98, 0.02),
          legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
          #text = element_text(family="Roboto Slab"),
          plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
          axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
          axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
          axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
          axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
          legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
          legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot

  return(p)
}

# include timings of rvr-10%, rvr-1%, rvr-matsim-10%, and dry run
rvr_timings <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "Prototype 10%")

rvr_1pct_timings <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-1pct/output-pre-cmp") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "Prototype 1%")

rvr_matsim_timings <- read_matsim_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/matsim-benchmark") %>%
  mutate(run_time = duration / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  select(size, run_time, rtr) %>%
  mutate(name = "QSim 10%")

berlin_empty_timings <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-empty/output") %>%
  filter(func == "rust_q_sim::simulation::simulation::run") %>%
  group_by(size) %>%
  summarize(run_time = mean(median_dur) / 1e9) %>%
  mutate(rtr = 129600 / run_time) %>%
  mutate(name = "Baseline Run")

combined_timings <- bind_rows(rvr_timings, rvr_1pct_timings, rvr_matsim_timings, berlin_empty_timings)
max_rtr <- combined_timings %>%
  group_by(name) %>%
  filter(size < 8000) %>%
  filter(rtr == max(rtr)) %>%
  mutate(rtr = round(rtr, 0))
min_rtr <- combined_timings %>%
  group_by(name) %>%
  filter(size < 8000) %>%
  filter(rtr == min(rtr)) %>%
  mutate(rtr = round(rtr, 0))

print(combined_timings)

combined_timings <- combined_timings %>%
  group_by(name) %>%
  mutate(ref_run_time = run_time[size == 1]) %>%
  mutate(speedup = ref_run_time / run_time) %>%
  mutate(efficiency = speedup / size) %>%
  ungroup()

up_to_thousand <- combined_timings %>%
  filter(name != "QSim 10%")

p <- ggplot(up_to_thousand, aes(size, run_time, color = as.factor(name))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +  # Format x-axis
  scale_color_manual(values = palette()) +
  theme_light()
p

p <- plot_line(
  data = combined_timings, x_var = "size", y_var = "rtr", color_var = "as.factor(name)",
  title = "Real Time Ratio for benchmark runs",
  x_label = "Number of processes",
  y_label = "Real Time Ratio"
) +
  geom_label(data = min_rtr, aes(label = rtr), vjust = -0.3, hjust = 0.35, show.legend = FALSE) +
  geom_label(data = max_rtr, aes(label = rtr), vjust = 1.3, hjust = 0.25, show.legend = FALSE)
ggsave("rtr-hlrn.pdf", plot = p, device = "pdf", width = 118, height = 100, units = "mm")
ggsave("rtr-hlrn.png", plot = p, device = "png", width = 118, height = 100, units = "mm")
p
embed_fonts("rtr-hlrn.pdf", outfile = "rtr-hlrn-embedded.pdf")

# # load tracing data for rvr-10%
# tracing_rvr <- read_binary_tracing_files(c(
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-1",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-2",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-4",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-8",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-16",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-32",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-64",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-256",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-512",
#   "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-1024"
#   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-2048",
#   # "/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/rvr-v1.4-10pct/output-trace-pre-cmp/size-4096"
# ), on_load = on_load_tracing, parallel = TRUE)

tracing_rvr <- read_binary_tracing_files(c(
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-1",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-2",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-4",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-8",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-16",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-32",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-64",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-128",
  "/Users/janek/hlrn/strong-scaling/rvr/rvr-102.4pct/output-tracing/size-256"
), on_load = on_load_tracing, parallel = TRUE)

acc_runtimes <- tracing_rvr %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))
ordered_data <- acc_runtimes %>%
  mutate(category = if_else(size > 256, "large", "small")) %>%
  #mutate(category = fct_relevel(category, c("small", "large"))) %>%
  mutate(func = fct_relevel(func, function_filter))

p <- ggplot(ordered_data, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Processes") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Phase") +
  ggtitle("Durations of algorithm phases - Prototype-10%") +
  scale_fill_manual(values = c(yellows(), blues())) +
  theme_light(base_size = 10) +
  theme(legend.position = "inside",
        legend.justification = c(0.98, 0.95),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
ggsave("acc_runtimes.pdf", plot = p, device = "pdf", width = 118, height = 100, units = "mm")
ggsave("acc_runtimes.png", plot = p, device = "png", width = 118, height = 100, units = "mm")
p

work <- tracing_rvr %>%
  filter(func %in% work_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = `Finish activities` +
    `Teleport` +
    `Move nodes` +
    `Move links`) %>%
  mutate(phase = "Waiting") %>%
  group_by(sim_time, size, phase) %>%
  summarize(dur = max(duration) - min(duration), .groups = 'drop')

comm <- tracing_rvr %>%
  filter(func %in% comm_filter) %>%
  pivot_wider(names_from = func, values_from = duration) %>%
  mutate(duration = Receive) %>%
  mutate(phase = "Max. comm") %>%
  group_by(sim_time, size, phase) %>%
  summarize(dur = max(duration), .groups = 'drop')

work_comm <- bind_rows(work, comm) %>%
  filter(size %in% c(16, 64, 256)) %>%
  pivot_wider(names_from = phase, values_from = dur) %>%
  mutate(`Msg. exchange` = `Max. comm` - `Waiting`) %>%
  pivot_longer(cols = c("Max. comm", "Waiting", "Msg. exchange"), names_to = "phase", values_to = "dur") %>%
  mutate(sim_time_posix = as.POSIXct(sim_time, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(phase = factor(phase, levels = c("Waiting", "Max. comm", "Msg. exchange"))) %>%
  arrange(phase)

p <- ggplot(work_comm, aes(sim_time_posix, dur / 1e3, color = as.factor(phase))) +
  geom_point(shape = '.', alpha = 0.8) +
  facet_wrap(~size, scales = "fixed") +
  scale_y_log10() +
  #ylim(0, 300) +
  scale_x_datetime(labels = date_format("%H:%M", tz = "UTC")) +
  #scale_x_continuous(labels = scales::comma) +  # Format x-axis labels
  ggtitle("Timings for individual timesteps") +
  ylab("Duration [\u00B5s]") +
  xlab("Simulation Time") +
  labs(color = "Phase") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1.0, shape = 19))) +
  scale_color_manual(values = palette()) +
  theme_light(base_size = 10) +
  theme(legend.position = "inside",
        legend.justification = c(0.98, 0.95),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Set x-axis text to 8pt
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
ggsave("work-wait-hlrn.pdf", plot = p, device = "pdf", width = 210, height = 100, units = "mm")
ggsave("work-wait-hlrn.png", plot = p, device = "png", width = 210, height = 100, units = "mm")
p

# load tracing data for dry run
tracing_dry <- read_binary_tracing_files("/Users/janek/Documents/writing/RustQSim/data-files-nextcloud/instrumenting/berlin-v6.0-empty/output-with-tracing",
                                         on_load = on_load_tracing, parallel = TRUE)

acc_runtimes_dry <- tracing_dry %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))
ordered_data_dry <- acc_runtimes_dry %>%
  mutate(category = if_else(size > 256, "large", "small")) %>%
  mutate(func = fct_relevel(func, function_filter))

p <- ggplot(ordered_data_dry, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Processes") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Phase") +
  ggtitle("Durations of algorithm phases - Baseline Run") +
  scale_fill_manual(values = c(yellows(), blues())) +
  theme_light(base_size = 8) +
  theme_light(base_size = 10) +
  theme(legend.position = "none",
        legend.justification = c(0.02, 0.95),
        legend.box.background = element_rect(fill = "#FFFFFF", color = "gray"),
        plot.title = element_text(size = 12, face = "bold"),  # Set plot title to 8pt
        axis.title.x = element_text(size = 10),  # Set x-axis title to 8pt
        axis.title.y = element_text(size = 10),  # Set y-axis title to 8pt
        axis.text.x = element_text(size = 10),  # Set x-axis text to 8pt 
        axis.text.y = element_text(size = 10),  # Set y-axis text to 8pt
        legend.title = element_text(size = 10, face = "bold"),  # Set legend title to 8pt
        legend.text = element_text(size = 10))  # Sets legend at the bottom of the plot
ggsave("dry_acc_runtimes.pdf", plot = p, device = "pdf", width = 118, height = 100, units = "mm")
ggsave("dry_acc_runtimes.png", plot = p, device = "png", width = 118, height = 100, units = "mm")
p