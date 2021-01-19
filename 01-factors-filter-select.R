library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(here)

rm(list = ls()) #Clean out workspace

#datafile is ~13k lines long so let's just read in 2k
ds <- read_csv(here("data_example_1_2","training_data.csv"), n_max = 2000)

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
ds %>% arrange(class, class_rel) -> ds #You can do this if it makes more sense to you

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

#Handy for changing a subset of a variable based on a condition
ds$class_prop <- ifelse(ds$class_prop == 1, NA, ds$class_prop)

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

          