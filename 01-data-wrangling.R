library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(stringr) #help us deal with some messy data
library(here)

rm(list = ls()) #Clean out workspace

#datafile is ~13k lines long so let's just read in 1k
ds <- read_csv(here("data_raw","training_data.csv"), n_max = 2000)

glimpse(ds)

##### FACTORS --------- 

#class and class_rel are both categories that were coded as numbers
#let's turn this into a factor

#store a vector of category numbers
class_num <- c(1,2,3,4,5,6,7,8,9,10) # class_num <- 1:10
#store a vector of category labels as strings
class_lab <-c("upright", "walking", "prone", "crawling","held_walk","held_stat","sit_surf","sit_cg","sit_rest","supine")

ds$class <- factor(ds$class, levels = class_num, labels = class_lab)
ds$class_rel <- factor(ds$class_rel, levels = class_num, labels = class_lab)

#Let's see how much of each category we have
fct_count(ds$class)
#Note that tables/figures will include unused levels

#Let's drop the levels we don't have 
ds$class <- fct_drop(ds$class)
ds$class_rel <- fct_drop(ds$class_rel)
fct_count(ds$class) #Note the change

#Let's reshuffle some categories
ds$class <- fct_collapse(ds$class, 
                         prone = "prone", supine = "supine",
                         held = c("held_walk","held_stat"),
                         sit = c("sit_surf", "sit_cg", "sit_rest"))
ds$class_rel <- fct_collapse(ds$class_rel, 
                         prone = "prone", supine = "supine",
                         held = c("held_walk","held_stat"),
                         sit = c("sit_surf", "sit_cg", "sit_rest"))

##### ARRANGE, FILTER, SELECT --------- 

#Let's sort our dataset by class (and then class_rel)
ds <- ds %>% arrange(class, class_rel)

#Helpful for looking for matches, but makes more sense to stay sorted by time
ds <- ds %>% arrange(time)

#Let's create a variable that checks whether class and class_rel match
ds$match <- ds$class == ds$class_rel #logic of ==, what's the type of our new var?
class(ds$match)

#Maybe a numeric variable would be better
ds$match <- ifelse(ds$class == ds$class_rel, 1, 0)
class(ds$match)
mean(ds$match)

#Let's filter to just look at the mismatches
ds %>% filter(match == 0) #Note that ds hasn't changed, we didn't save anything

#Do we really need 306 variables right now? 
ds %>% filter(match == 0) %>% select(time:class_prop_rel)

#Other select variants
ds %>% filter(match == 0) %>% select(time, class, class_rel)
ds %>% filter(match == 0) %>% select(c("time", "class", "class_rel"))
ds %>% filter(match == 0) %>% select(time, starts_with("class"))

#Other types of logic for filters
ds %>% filter(class_prop < 1) %>% select(class_prop) %>% arrange(class_prop)
ds %>% filter(class_prop > .5 & class_prop < .6) %>% select(class_prop) %>% arrange(class_prop)
ds %>% filter(class == "held" | class == "supine")
ds %>% filter(class %in% c("held","supine"))

#filter, select, etc. all return tibbles, but aren't saved by default. 
ds_mismatch <- ds %>% filter(match == 0) %>% select(time:class_prop_rel)
#ds <- ds %>% filter(class_prop == 1) #Overwrites original dataset

##### MUTATE, RENAME--------- 

#We did this already in base R
ds$match <- ds$class == ds$class_rel
ds <- ds %>% mutate(match = class == class_rel) #have to pipe ds in but also save the result back

#Why bother with that clunky mutate syntax? You can mutate multiple variables at once
ds <- ds %>% mutate(
  class_greater_50 = as.numeric(class_prop > 50),
  class_rel_greater_50 = as.numeric(class_prop_rel > 50),
 )

#You can also use helper functions to make multiple transformations at once
#We'll spend more time with across when we get to summarize
ds_mismatch <- ds_mismatch %>% 
  mutate(across(contains("prop"), floor))

ds_mismatch <- ds_mismatch %>% 
  mutate(across(where(is.numeric), as.character))
glimpse(ds_mismatch)

#Rename columns 
ds_mismatch <- ds_mismatch %>% 
  rename(class_bin = class_prop, class_bin_rel = class_prop_rel)
         