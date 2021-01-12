library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(visdat)
library(janitor)
library(stringr) #help us deal with some messy data

rm(list = ls()) #Clean out workspace

#Some things we could do better
files <- list.files(path = "data_raw/", pattern = "*.txt", full.names =  T)
id <- str_extract(files, "\\d\\d\\d") #Pull the id from the filename \\d\\d\\d means "find 3 digits"

#Let's figure out the names automatically
#Rename the ones we don't like
col_names <- read_delim(files, delim = " ", skip = 6) 
col_names <- col_names %>% 
  rename(
    scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
    por_time = `porQTtime(d:h:m:s.tv/ts)`) %>% 
  names()

#Read the data in, this time skip the header lines and use our pre-specified names
ds <- read_delim(files, delim = " ", skip = 7, col_names = col_names) %>% clean_names()

#Let's filter our data again, but use the metadata as a guide
header <- read_delim('data_raw/101.txt', delim = ":", n_max = 5, col_names = F) #Not the most useful yet
header <- header %>% rename(field = X1) #Give this a better name
header <- header %>% unite("value", X2:X4, remove = T, na.rm = T) #Fix the weird parsing error from the extra colon in the date

#Use our header and filename to add metadata to ds

#Resolution
resolution <- header %>% filter(field == "scene resolution") %>% pull(value) #Let's get the resolution
resolution <- str_split(resolution, "x", simplify = T) #Split the X and Y components
#Add resolution boundaries from header
ds <- ds %>% 
  add_column(min_x = 0,
             max_x = as.numeric(resolution[1]),
             min_y = 0,
             max_y = as.numeric(resolution[2]),
             .before = "record_frame_count")

#Get the test date
test_date <- header %>% filter(field == "record started at") %>% pull(value) #Pull the date/time string
test_date <- test_date %>% 
  str_trim() %>%    #Trim the white space at the beginning
  str_split(" ", simplify = T) %>% #Split by the space
  .[1] %>% #Pull the first item
  as.Date(format = "%m-%d-%y") #Format it as a date

#Add the id and the test date to the beginning of the file
ds <- ds %>% add_column(id = id, test_date = test_date, .before = "min_x")

#Set NAs, but this time use the header-derived values
ds_cleaned <- ds %>% 
  mutate(por_x = ifelse(por_x < min_x | por_x > max_x, NA, por_x),
         por_y = ifelse(por_y < min_y | por_y > max_y, NA, por_y))

#Remove unwanted columns
ds_cleaned <- ds_cleaned %>% select(record_frame_count:pupil_y)

#Check that the data cleaned data work (note the warning)
ds_cleaned %>% 
  ggplot(aes(x = por_x, y = por_y)) + 
  geom_density2d_filled() + 
  theme_minimal() + 
  theme(legend.position = "none")
#Save the plot to eda as a data check
ggsave(paste0("eda/et_distribution_figs/",id,"_check.png"), width = 8, height = 6, units = "in")

#Write the data, but generate the output file from the id to make it more extensible
ds_cleaned %>% write_csv(paste0("data_cleaned/",id,".csv"))

#Endless possibilities for filenames
#glue provides a nicer way to compose filename (or any) strings
#If you need more control over formatting numbers (esp decimals) in a string, sprintf is another example
str_glue("data_cleaned/{id}_{test_date}.csv")
str_glue("data_cleaned/{id}/et_cleaned.csv")

#But what about our header info? Should we save that too? Or maybe save an RDS file to cleaned instead
save(file = paste0("data_cleaned/",id,".RData"), list = c("ds_cleaned", "header"))

#Clear workspace and check that we can reload our saved data
rm(list = ls())
load(paste0("data_cleaned/101.RData"))



