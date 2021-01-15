library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(here)

rm(list = ls()) #Clean out workspace

#datafile is ~13k lines long so let's just read in 2k
ds <- read_csv(here("data_example_1","training_data.csv"), n_max = 2000)

##### FACTORS --------- 

#class and class_rel are both categories that were coded as numbers
#let's turn this into a factor

#store a vector of category numbers
class_num <- c(1,2,3,4,5,6,7,8,9,10) # class_num <- 1:10
#store a vector of category labels as strings
class_lab <- c("upright", "walking", "prone", "crawling","held_walk",
               "held_stat","sit_surf","sit_cg","sit_rest","supine")

class(class_num)
class(class_lab)

ds$class <- factor(ds$class, levels = class_num, labels = class_lab)
ds$class_rel <- factor(ds$class_rel, levels = class_num, labels = class_lab)

#Let's see how much of each category we have
fct_count(ds$class)
#Note that tables/figures will include unused levels
#fct_count is coming from the "forcats" package, loaded w/ tidyverse

#Let's drop the levels we don't have 
ds$class <- fct_drop(ds$class)
ds$class_rel <- fct_drop(ds$class_rel)
fct_count(ds$class) #Note the change

#Let's remap some factor levels
ds$class <- fct_collapse(ds$class, 
                         prone = "prone", supine = "supine",         
                         held = c("held_walk","held_stat"),          
                         sit = c("sit_surf", "sit_cg", "sit_rest"))
ds$class_rel <- fct_collapse(ds$class_rel, 
                         prone = "prone", supine = "supine",
                         held = c("held_walk","held_stat"),
                         sit = c("sit_surf", "sit_cg", "sit_rest"))

#Let's glance at some of the other options in "forcats"
?forcats
#Reordering levels, combining levels, turning rare categories into other.

##### USING LOGIC TO ARRANGE, FILTER, SELECT --------- 

#### Arrange means sort ----- 

#Let's sort our dataset by class (and then class_rel)
ds %>% arrange(class, class_rel) #Sorted, but changes not saved
#Remember, the pipe statement above is just a shorthand for:
#arrange(ds, class, class_rel)

#Let's sort our dataset by class (and then class_rel)
ds <- ds %>% arrange(class, class_rel) #Did it stay that way? Yes
ds %>% arrange(class, class_rel) -> ds2 #You can do this if it makes more sense to you

#Makes more sense to stay sorted by time, so let's go back to that
ds <- ds %>% arrange(time)

#Let's create a variable that checks whether class and class_rel match
ds$match <- ds$class == ds$class_rel 
class(ds$match) #What type is match?

#Applying logical operators
1 == 1
2 > 3
"sit" == "sit"
"sit" == "stand"

#Works element-wise on vectors
some_ints <- c(1:5, 5:1)
class_num  == some_ints
class_num  > some_ints

#Maybe a numeric variable would be better?
ds$match <- ifelse(ds$class == ds$class_rel, 1, 0)
class(ds$match)
?ifelse #just like excel's if

#### Filter = choosing rows ----

#Base R 
subset(ds, match == 0)
ds[ds$match == 0,]

#Tidyverse uses "filter" 
ds %>% filter(match == 0)

#Other filtering examples
ds %>% filter(class == "prone" | class == "supine") #Example of using "or" aka |
ds %>% filter(class == "supine" & class_rel == "sit") #Example of using "and" aka &
ds %>% filter(time > 319 & time < 322) #Example of using "and" with numeric ranges
ds %>% filter(class == "prone") %>% filter(time > 319) %>% filter(match == 1) #endless chaining (same as &)
ds %>% filter(class %in% c("held","supine")) #if class is in a list

#### Select = choosing columns ---- 

#Base R, select a single column
m1 <- ds$match #returns vector, not a dataframe/tibble
#Tidyverse, select a single column
m2 <- select(ds, match) #returns tibble even if it's 1 column
#Tidyverse, pull a single column
m3 <- pull(ds, match) #returns a vector, not a tibble

#Base R, select multiple columns, not very flexible
ds[, c("match", "class")]
ds[, time:class_rel] #no
ds[, starts_with("class")] #no
ds[, 1:4] #yes, but not very useful (what if your column order changes)


#Tidy select, endless options for how to identify columns
ds %>% select(time, class, class_rel) #list of unquoted columns
ds %>% select(c("time", "class", "class_rel")) #vector of string column names
ds %>% select(time:class_rel) #range of adjacent columns (from time to class_rel)
ds %>% select(time, starts_with("class")) #name matches pattern (also ends_with)
ds %>% select(contains("rel"))
ds %>% select(-starts_with("class")) # - means "everything but"
ds %>% select(where(is.factor))
?tidyselect::language #documentation for even more options

#Piping through filters/selections to narrow down our data set
ds_mismatch <- ds %>% filter(match == 0) %>% select(time:class_prop_rel)

##### MUTATE, RENAME--------- 

#Let's start with a slightly narrower dataset
ds_means <- ds %>% select(class:class_prop_rel, mean_x1:mean_z3)
print(ds_means)

#We did this already in base R
ds_means$match <- ds_means$class == ds_means$class_rel
ds_means$match <- NULL #let's delete that column
#Mutate is the tidyverse way of creating/editing columns
ds_means <- ds_means %>% mutate(match = class == class_rel) 

#Unlike in base R, you can mutate multiple variables at once
ds_means <- ds_means %>% mutate(
  class_greater_50 = as.numeric(class_prop > 50),
  class_rel_greater_50 = as.numeric(class_prop_rel > 50),
  class_less_50 = as.numeric(class_prop < 50),
  class_rel_less_50 = as.numeric(class_prop_rel < 50),
 )

#More powerful mutate options
ds_means %>% mutate(across(c("mean_x1","mean_y1", "mean_z1"), abs))
ds_means %>% mutate(across(where(is.numeric), abs))
ds_means %>% mutate(across(ends_with("3"), abs))
ds_means %>% mutate(across(where(is.factor), as.character))
ds_means %>% mutate(across(everything(), as.character))

#Rename columns 
ds_means %>% rename(M_X1 = mean_x1, M_Y1 = mean_y1, M_Z1 = mean_z1)

#More powerful rename options
ds_means %>% rename_all(toupper)
ds_means %>% rename_with(toupper, ends_with("3"))

##### SUMMARIZE, GROUP -------- 

#Let's get a clean ds to work withm this time with a few correlations
ds_corr <- ds %>% select(time, class, corr_xy, corr_xz, corr_yz)
#Make a factor (1st half, time is < median, 2nd half, time is > median)
ds_corr$half  <-  ds$time > median(ds$time)
ds_corr$half <- factor(ds_corr$half, levels = c(FALSE, TRUE), labels = c("1st", "2nd"))

#Summarize to calculate stats across rows (collapses to a single value)
ds_corr %>% summarise(xy_mean = mean(corr_xy), xy_sd = sd(corr_xy), xy_n = n(), xy_se = xy_sd/sqrt(xy_n))

#Chain summarize with filter
ds_corr %>% 
  filter(class == "sit") %>% 
  summarise(xy_mean = mean(corr_xy), xy_sd = sd(corr_xy), xy_n = n(), xy_se = xy_sd/sqrt(xy_n))

#More often, pair summarize with group_by
ds_corr %>% 
  group_by(class) %>% 
  summarise(xy_mean = mean(corr_xy), xy_sd = sd(corr_xy), xy_n = n(), xy_se = xy_sd/sqrt(xy_n))
#group_by means, within each group, summarize
#treat each group separately when calculating

#Group by as many variable (combinations) as you want
ds_corr %>% 
  group_by(class, half) %>% 
  summarise(xy_mean = mean(corr_xy), xy_sd = sd(corr_xy), xy_n = n(), xy_se = xy_sd/sqrt(xy_n))

#Across again to save us from typing
ds_corr %>% 
  group_by(class, half) %>% 
  summarise(across(corr_xy:corr_yz, mean))
#across(selected vars, function)

#I'm so sick of typing out the formula for SE, so let's make it a function
#MUCH more of this in a few weeks
se <- function(x) sd(x)/sqrt(length(x))

#Now we can use across with a list of functions for even more automation
results <- ds_corr %>% 
  group_by(class, half) %>% 
  summarise(across(starts_with("corr"), list(mean = mean, sd = sd, se = se))) %>% 
  ungroup()

#Great thing about saving your results as tibbles -> easy to select/filter! 
results %>% filter(class == "prone") %>% select(ends_with("se"))
papaja::apa_table(results)
