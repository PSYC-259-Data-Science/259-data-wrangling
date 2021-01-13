#Load packages
library(tidyverse)
library(vroom)
library(lubridate)
library(here)

col_names = c("trial_num","speed_actual","speed_response","correct")
file_names <- list.files("data_example_2", full.names = T)
ds <- vroom(file_names, id = "file", skip = 8, col_names = col_names)
header <- vroom(file_names, delim = " ", n_max = 8, col_names = c("field","value"),id = "file")

header <- header %>%  pivot_wider(id_cols = file, names_from = field, values_from = value)

ds_joined <- left_join(header, ds, by = "file")

ds_joined <- ds_joined %>% separate(file, into = c("dir", "file"), sep = "/")
ds_joined <- ds_joined %>% separate(file, into = c("file", "ext"), sep = ".")
