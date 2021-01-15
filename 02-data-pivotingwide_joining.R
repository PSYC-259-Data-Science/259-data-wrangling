#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)

rm(list = ls()) #Clean out workspace

col_names = c("trial_num","speed_actual","speed_response","correct")
file_names <- list.files("data_example_2", full.names = T)
ds <- vroom(file_names, id = "file", skip = 8, col_names = col_names)
header <- vroom(file_names, delim = " ", n_max = 8, col_names = c("field","value"),id = "file")

header <- header %>%  pivot_wider(id_cols = file, names_from = field, values_from = value)

ds_joined <- left_join(header, ds, by = "file")

ds_joined <- ds_joined %>% separate(file, into = c(NA,NA, NA, "Participant_file","Block_file", NA))
ds_joined <- ds_joined %>% rename_with(make_clean_names)

speed_labels = c("slower", "faster")
ds_joined <- ds_joined %>% 
  mutate(
    speed_actual = factor(speed_actual, levels = c("slo","fas"), labels = speed_labels),
    speed_response = factor(speed_response, levels = c("slower","faster"), labels = speed_labels),
  )


####  SOME THINGS FOR LATER -------------------

checks <- ds_joined %>% 
  group_by(participant, block) %>% 
    summarize(
      match_participant = sum(as.numeric(participant_file == participant)),
      match_block = sum(as.numeric(block == block_file)),
      match_response = sum((speed_actual == speed_response) == correct, na.rm = T),
      trial_order = sum((trial_num - lag(trial_num, n = 1)), na.rm = T) + 1,
      num_trials = n(),
    )

checks %>% filter(
  !(match_participant == num_trials) | 
  !(match_block == num_trials) |
  !(match_response == num_trials) | 
  !(trial_order == num_trials))

ds_cleaned <- ds_joined %>% select(participant:correct)

rm(list = c("ds", "header","ds_joined"))
