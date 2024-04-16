#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
library(patchwork)

source("./src/main/R/read_tracing_files.R")
source("./src/main/R/colors.R")

on_load <- function(data) {
  data %>%
    filter(bin_start > 0) %>%
    filter(
      func == "rust_q_sim::simulation::messaging::communication::communicators::handle_msgs" |
        func == "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs" |
        func == "rust_q_sim::simulation::messaging::communication::communicators::send_msgs" |
        func == "rust_q_sim::simulation::network::sim_network::move_links" |
        func == "rust_q_sim::simulation::network::sim_network::move_nodes" |
        func == "rust_q_sim::simulation::simulation::wakeup" |
        func == "rust_q_sim::simulation::simulation::terminate_teleportation") %>%
    mutate(func_name = sub(".*::", "", func)) %>%
    mutate(sim_time = bin_start) %>%
    mutate(duration = median_dur) %>%
    mutate(func = func_name) %>%
    select(size, rank, func, duration)
}

data <- read_binary_tracing_files(roots = c(
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-1",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-2",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-4",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-8",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-16",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-32",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-64",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-128",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-256",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-512",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-1024",
  "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-2048"
), on_load = on_load, parallel = TRUE) %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))

# split this into two reads, becuase otherwise the largest tibble can't be bound (I don't have enough ram)
data_8192 <- read_binary_tracing_files(roots =
                                         "/Users/paulheinrich/git/rust_qsim_eval/assets/hlrn-all/update-do-replan-multi-wght/size-8192"
  , on_load = on_load, parallel = TRUE) %>%
  group_by(size, func) %>%
  summarize(mean_dur = mean(duration), sum_dur = sum(duration))

data <- data %>%
  bind_rows(data_8192)

order <- c("receive_msgs", "send_msgs", "handle_msgs", "move_links", "move_nodes", "wakeup", "terminate_teleportation")
colors_msging <- c("#feb70e", "#fece5a", "#fee5a6")
colors_work <- c("#0e54fe", "#4178fe", "#739cfe", "#a6c0fe")
ordered_data <- data %>%
  mutate(category = if_else(size > 256, "large", "small")) %>%
  mutate(category = fct_relevel(category, c("small", "large"))) %>%
  mutate(func = fct_relevel(func, order))


p1 <- ggplot(ordered_data, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_y_log10() +
  facet_wrap(~category, scales = "free") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Functions") +
  ggtitle("Mean duration of performing one sim step") +
  scale_fill_manual(values = c(colors_msging, colors_work)) +
  theme_light()
p1

work <- ordered_data %>% filter(
  func == "wakeup" |
    func == "terminate_teleportation" |
    func == "move_nodes" |
    func == "move_links"
)

p2 <- ggplot(work, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~category, scales = "free") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Functions") +
  ggtitle("Mean duration of performing simulation work for one simulation time step") +
  scale_fill_manual(values = colors_work) +
  theme_light()
p2

messaging <- data %>% filter(
  func == "handle_msgs" |
    func == "receive_msgs" |
    func == "send_msgs"
)

p3 <- ggplot(messaging, aes(x = as.factor(size), y = mean_dur / 1e3, fill = as.factor(func))) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Number of Cores") +
  ylab("Mean Duration [\u00B5s]") +
  labs(fill = "Functions") +
  ggtitle("Mean duration of performing messaging for one simulation time step") +
  scale_fill_manual(values = colors_msging) +
  theme_light()
p3

combined <- p1 / p2 / p3
combined
ggsave("mean-dur.pdf", combined, device = "pdf", width = 210, height = 297, units = "mm")