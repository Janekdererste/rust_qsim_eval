#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("./src/main/R/colors.R")
source("./src/main/R/parsing.R")
source("./src/main/R/tracing.R")
source("./src/main/R/replanning/routing_utils.R")

plot_data <- function (data, title_, x_axis, y_axis) {
  ggplot(data, aes(x = index, y = duration/1e6, color=func_name)) +
    geom_point() +
    ylim(0,5) +
    labs(title = title_, x = x_axis, y = y_axis)
}

filter_and_plot <- function(data, func_filter, title_, x_axis, y_axis) {
  subset_data <- data %>%
    filter(func == func_filter) %>%
    mutate(index = row_number())

  # Plotten nur jeden zweiten Punkt
  # Plotten ungerader Indizes in blau und gerader Indizes in rot
  ggplot(subset_data, aes(x = index, y = duration/1e6, color = ifelse(index %% 2 == 1, "Odd", "Even"))) +
    geom_point() +
    labs(title = title_, x = x_axis, y = y_axis) +
    ylim(0, 5) +
    scale_color_manual(
      values = c("Odd" = "blue", "Even" = "red"),
      labels = c("With sync", "Without sync"),
      breaks = c("Odd", "Even"),
    )
}

traces <- load_rust_tracing_data("assets/hlrn-all/update-no-replan",
                              num_cores = 16, file_name_filter = c("_0.csv"), node_count_filter = 256)

general_filter <- c(
                    # "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles",
                    "rust_q_sim::simulation::replanning::replanner::replan",
                    "rust_q_sim::simulation::replanning::replanner::find_route",
                    "rust_q_sim::simulation::simulation::wakeup"
                    # "rust_q_sim::simulation::replanning::replanner::replan_access_egress",
                    # "rust_q_sim::simulation::replanning::replanner::replan_main"
)

filtered_data <- filter_by_func(traces, c(GATHER_LENGTHS_KEY, GATHER_TT_KEY, DESERIALIZE_KEY))

# filter_and_plot(traces, GATHER_TT_KEY, "Routing", "Index", "Duration (ms)")
plot_data(filtered_data, "Routing", "Index", "Duration (ms)")

comm_data <- filter_by_func(traces, GATHER_TT_KEY)
comm_data <- tail(comm_data, -2)
message("Overall mean is ", mean(comm_data$duration)/1e6, " ms")

comm_w_sync <- comm_data %>% filter(index%%2==1)
message("Sync mean is ", mean(comm_w_sync$duration)/1e6, " ms")

comm_wo_sync <- comm_data %>% filter(index%%2==0)
message("No sync mean is ", mean(comm_wo_sync$duration)/1e6, " ms")


# plot_data(filtered_data, "Routing", "Index", "Duration (ms)")