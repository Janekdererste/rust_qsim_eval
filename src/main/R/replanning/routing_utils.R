COMMUNICATION_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::communicate_travel_times"
GATHER_LENGTHS_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_time_lengths"
GATHER_TT_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_times_var_count"
COLLECT_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::collect_travel_times"
INSERTION_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::get_travel_times_by_mode_to_send"
HANDLE_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::handle_traffic_info_messages"
DESERIALIZE_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::deserialize_travel_times"
HANDLE_INSERT <- "rust_q_sim::simulation::replanning::routing::graph::insert_new_travel_times_by_link"
COMMUNICATION_ALL_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::communicate_all"

filter_by_func <- function(data, func_filter){
  filtered_data <- data %>%
    filter(func %in% func_filter) %>%
    mutate(index = row_number())
  return(filtered_data)
}


routing_filter <- c(COMMUNICATION_KEY, COLLECT_KEY, INSERTION_KEY, HANDLE_KEY, GATHER_LENGTHS_KEY, GATHER_TT_KEY,
                    DESERIALIZE_KEY, COMMUNICATION_ALL_KEY)