# Plot first tt exchanges durations
first_tt_exchange <- comm_traces %>%
  filter(func == GATHER_TT_KEY) %>%
  filter(row_number() %% 2 == 1) %>%
  mutate(index = row_number() %% (97 * 2)) %>%
  filter(index <= 97 & index != 1)

ggplot(first_tt_exchange, aes(x = index, y = duration / 1e6, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  xlab("Simulation Time") +
  ylab("Median Duration ms") +
  ggtitle("Execution Times for First Data Exchange (Gather All)") +
  theme_light()

# Plot first tt exchanges durations
first_deserialize <- comm_traces %>%
  filter(func == DESERIALIZE_KEY) %>%
  filter(row_number() %% 2 == 1) %>%
  mutate(index = row_number() %% (97 * 2)) %>%
  filter(index <= 97 & index != 1)

ggplot(first_deserialize, aes(x = index, y = duration / 1e6, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  xlab("Simulation Time") +
  ylab("Median Duration ms") +
  ggtitle("Execution Times for First Deserialization") +
  theme_light()

# Plot first communicate durations
first_comm <- comm_traces %>%
  filter(func == COMMUNICATION_KEY) %>%
  filter(row_number() %% 2 == 1) %>%
  mutate(index = row_number() %% (97 * 2)) %>%
  filter(index <= 97 & index != 1)

ggplot(first_comm, aes(x = index, y = duration / 1e6, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  xlab("Simulation Time") +
  ylab("Median Duration ms") +
  ggtitle("Execution Times for First Communication") +
  theme_light()

# Plot communication all durations
first_comm_all <- comm_traces %>%
  filter(func == COMMUNICATION_ALL_KEY) %>%
  filter(sim_time != 0)

ggplot(first_comm_all, aes(x = sim_time/900, y = duration / 1e6, group = rank, color = factor(rank), fill = factor(rank))) +
  geom_line() +
  facet_wrap(~size) +
  scale_color_viridis_d(name = "Rank") +
  xlab("Simulation Time") +
  ylab("Median Duration ms") +
  ggtitle("Execution Times for All Communication") +
  theme_light()