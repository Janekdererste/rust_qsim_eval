COMMUNICATION_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::communicate_travel_times"
GATHER_LENGTHS_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_time_lengths"
GATHER_TT_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::gather_travel_times_var_count"
COLLECT_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::collect_travel_times"
INSERTION_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::get_travel_times_by_mode_to_send"
HANDLE_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::handle_traffic_info_messages"
DESERIALIZE_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::deserialize_travel_times"
HANDLE_INSERT <- "rust_q_sim::simulation::replanning::routing::graph::insert_new_travel_times_by_link"
COMMUNICATION_ALL_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::communicate_all"
HANDLE_ALL_KEY <- "rust_q_sim::simulation::replanning::routing::travel_times_collecting_alt_router::handle_all"
ROUTING_KEY <- "rust_q_sim::simulation::replanning::replanner::find_route"
REPLAN_MAIN_KEY <- "rust_q_sim::simulation::replanning::replanner::replan_main"
WAKEUP_KEY <- "rust_q_sim::simulation::simulation::wakeup"
SEND_RECEIVE_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::send_receive_vehicles"
RECEIVE_KEY <- "rust_q_sim::simulation::messaging::communication::communicators::receive_msgs"
MOVE_LINKS_KEY <- "rust_q_sim::simulation::network::sim_network::move_links"
MOVE_NODES_KEY <- "rust_q_sim::simulation::network::sim_network::move_nodes"

filter_by_func <- function(data, func_filter){
  filtered_data <- data %>%
    filter(func %in% func_filter) %>%
    mutate(index = row_number())
  return(filtered_data)
}


routing_filter <- c(COMMUNICATION_KEY, COLLECT_KEY, INSERTION_KEY, HANDLE_KEY, GATHER_LENGTHS_KEY, GATHER_TT_KEY,
                    DESERIALIZE_KEY, COMMUNICATION_ALL_KEY)