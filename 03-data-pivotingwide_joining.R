#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)

rm(list = ls()) #Clean out workspace

#Get all files in the data_example 3 directory
file_names <- list.files("data_example_3", full.names = T)

#Use vroom to read the data (skipping 8 lines of header)
ds <- vroom(file_names, id = "file", skip = 8, col_names = c("trial_num","speed_actual","speed_response","correct"))
#Use vroom to read the header (just reading the first 8 lines)
header <- vroom(file_names, delim = " ", n_max = 8, col_names = c("field","value"),id = "file")

#How can we put the header and data frame together?
#One way would be to add columns for each field in ds, then use mutate and logic to match up the values
#This would be very tedious

#A better option is to reshape the header so that it is tidy
print(header)
#One observation per row, and variables all in their own columns
#"Field" currently has different variables all bunched in the same column
#Any time you have a whole lot of different data types in a column (like field),
#that's a cue to reshape from longer to wider format
#We need to spread them out!

#Time to pivot_wider
#id_cols = column that is unique for each observation (in our case, the file name)
#names_from = column that we will get new column names from (variable names come from field)
#values_from = column that will provide the values (in our case it is called values)
header <- header %>%  pivot_wider(id_cols = file, names_from = field, values_from = value)

print(header)

#Now let's merge the header info into ds
#We can't just add columns. Why not? Look at the # of observations in each tibble

#left_join will merge datasets, and will fill the right number of rows if you can 
#match each data frame by a key (in this case, file name)
ds_joined <- left_join(header, ds, by = "file")
print(ds_joined)
#ds_joined has the right # of observations (240), and pickd up all 9 variables from the header

#Let's use "separate" to clean up the file name into something more useful
#Takes a character variable into multiple columns based on delimiters
# "data_example_3/6191_1.txt" becomes "data" "example" "3" "6191" "1" "txt"
#NA is saying "throw this out", we only want participant and block
ds_joined <- ds_joined %>% separate(file, into = c(NA,NA, NA, "Participant_file","Block_file", NA))
ds_joined <- ds_joined %>% rename_with(make_clean_names) #handy fuction from janitor package

speed_labels = c("slower", "faster")
ds_joined <- ds_joined %>% 
  mutate(
    speed_actual = factor(speed_actual, levels = c("slo","fas"), labels = speed_labels),
    speed_response = factor(speed_response, levels = c("slower","faster"), labels = speed_labels),
  )

ds_joined %>% write_csv(here("data_example_3","joined_data.csv"))
#Why both converting to factors and then writing to text? 
#Factors get written as their labels, so next time we import this it will read in more cleanly
