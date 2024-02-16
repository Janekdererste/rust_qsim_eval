#tidyverse https://www.tidyverse.org/ which is the stuff we use to wrangle and plot our data
library(tidyverse)
# json library recommendet by tidyverse https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)

source("../colors.R")
source("../parsing.R")
source("../tracing.R")
source("routing_utils.R")

plot_data <- function (data, title_, x_axis, y_axis) {
  ggplot(data, aes(x = index, y = duration/1e6, color=func_name)) +
    geom_point() +
    labs(title = title_, x = x_axis, y = y_axis) +
    ylim(0,50)
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
    ylim(0, 50) +
    scale_color_manual(
      values = c("Odd" = "blue", "Even" = "red"),
      labels = c("With sync", "Without sync"),
      breaks = c("Odd", "Even"),
    )
}

# traces <- load_csv_instrument("/Users/paulheinrich/math/perf_rust_qsim/berlin-1pct/output/size-2/", num_cores = 2, file_name_filter = c("0.csv"))

traces <- load_csv_instrument("../../../../assets/hlrn-25",
                              num_cores = 16, file_name_filter = c("_0.csv"), node_count_filter = 256)

COMMUNICATION_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::communicate_travel_times"
GATHER_LENGTHS_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_time_lengths"
GATHER_TT_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_times_var_count"

general_filter <- c(
                    # "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles",
                    "rust_q_sim::simulation::replanning::replanner::replan",
                    "rust_q_sim::simulation::replanning::replanner::find_route",
                    "rust_q_sim::simulation::simulation::wakeup"
                    # "rust_q_sim::simulation::replanning::replanner::replan_access_egress",
                    # "rust_q_sim::simulation::replanning::replanner::replan_main"
)

filtered_data <- filter_by_func(traces, routing_filter)

filter_and_plot(traces, GATHER_LENGTHS_KEY, "Routing", "Index", "Duration (ms)")
# plot_data(filtered_data, "Routing", "Index", "Duration (ms)")

comm_data <- filter_by_func(traces, COMMUNICATION_KEY)
comm_data <- tail(comm_data, -2)
message("Overall mean is ", mean(comm_data$duration)/1e6, " ms")

comm_w_sync <- comm_data %>% filter(index%%2==1)
message("Sync mean is ", mean(comm_w_sync$duration)/1e6, " ms")

comm_wo_sync <- comm_data %>% filter(index%%2==0)
message("No sync mean is ", mean(comm_wo_sync$duration)/1e6, " ms")


# plot_data(filtered_data, "Routing", "Index", "Duration (ms)")