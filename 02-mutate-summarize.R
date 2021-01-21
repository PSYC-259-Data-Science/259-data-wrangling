library(tidyverse) #dplyr, tidyr, ggplot2, readr
library(here)
library(janitor)

rm(list = ls()) #Clean out workspace

##### FILE SETUP FROM EXAMPLE 1 ------------
ds <- read_csv(here("data_example_1_2","training_data.csv"), n_max = 2000)

#store a vector of category numbers
class_num <- c(1,2,3,4,5,6,7,8,9,10) # class_num <- 1:10
#store a vector of category labels as strings
class_lab <- c("upright", "walking", "prone", "crawling","held_walk",
               "held_stat","sit_surf","sit_cg","sit_rest","supine")
ds$class <- factor(ds$class, levels = class_num, labels = class_lab)
ds$class_rel <- factor(ds$class_rel, levels = class_num, labels = class_lab)
ds$class <- fct_drop(ds$class)
ds$class_rel <- fct_drop(ds$class_rel)

ds$class <- fct_collapse(ds$class, 
                         prone = "prone", supine = "supine",         
                         held = c("held_walk","held_stat"),          
                         sit = c("sit_surf", "sit_cg", "sit_rest"))
ds$class_rel <- fct_collapse(ds$class_rel, 
                         prone = "prone", supine = "supine",
                         held = c("held_walk","held_stat"),
                         sit = c("sit_surf", "sit_cg", "sit_rest"))

##### MUTATE --------- 

#Let's start with a narrower dataset
ds_means <- ds %>% select(class:class_prop_rel, mean_x1:mean_z3)
print(ds_means)

#Assigning a variable with base R
ds_means$match <- ds_means$class == ds_means$class_rel
ds_means$match <- NULL #let's delete that column

#Mutate is the tidyverse way of creating/editing columns
ds_means <- ds_means %>% mutate(match = class == class_rel) 

#Unlike in base R, you can mutate multiple variables at once
ds_means <- ds_means %>% mutate(
  class_greater_50 = as.numeric(class_prop > .5),
  class_rel_greater_50 = as.numeric(class_prop_rel > .5),
  class_less_50 = as.numeric(class_prop < .5),
  class_rel_less_50 = as.numeric(class_prop_rel < .5)
 )

#You can also mutate the same variable multiple times
ds_means <- ds_means %>% mutate(
  class_both_greater_50 = as.numeric(class_greater_50 + class_rel_greater_50),
  class_both_greater_50 = class_both_greater_50 == 2,
  class_both_greater_50 = factor(class_both_greater_50, levels = c(TRUE, FALSE), labels = c("greater","less than"))
)

#More powerful mutate options
#Across saves us from repetitive typing
#across(selected vars, function) means "apply this function to the selected variables"
ds_means %>% mutate(across(c("mean_x1","mean_y1", "mean_z1"), abs))
ds_means %>% mutate(across(where(is.numeric), abs))
ds_means %>% mutate(across(ends_with("3"), abs))
ds_means %>% mutate(across(where(is.factor), as.character))
ds_means %>% mutate(across(everything(), as.character))

##### RENAME ------- 

#Rename uses the format rename(new_name = old_name) without quotes
ds_means %>% rename(M_X1 = mean_x1, M_Y1 = mean_y1, M_Z1 = mean_z1)

#More powerful rename options
ds_means %>% rename_all(toupper)
ds_means %>% rename_with(toupper, ends_with("3"))


#I forgot to cover this, but I'm leaving the example here if you want to take a look 
#make_clean_names from the 'janitor' package can automatically make everything snake_case
iris
iris %>% rename_with(make_clean_names) 

##### SUMMARIZE, GROUP -------- 

#Let's get a clean ds to work withm this time with a few correlations
ds_corr <- ds %>% select(time, class, corr_xy, corr_xz, corr_yz)
print(ds_corr)

#Make a factor (1st half, time is < median, 2nd half, time is > median)
temp_var <-  ds$time > median(ds$time)
temp_var <- factor(temp_var, levels = c(FALSE, TRUE), labels = c("1st", "2nd"))

#Use .before/.after with mutate if you want to decide where it goes in your tibble
ds_corr <- ds_corr %>% mutate(half = temp_var, .before = "class") 
print(ds_corr)

#Summarize to calculate stats across rows (collapses to a single value)
ds_corr %>% summarize(xy_mean = mean(corr_xy))

#If any item in a column is NA, your summary stats will be NA unless you set na.rm = TRUE
ds_corr_withNA <- ds_corr %>% mutate(corr_xy = ifelse(corr_xy < 0, NA, corr_xy))
ds_corr_withNA %>% summarise(xy_mean = mean(corr_xy))
ds_corr_withNA %>% summarise(xy_mean = mean(corr_xy, na.rm = T))

#Like mutate, summarize can make as many summary stats as you want
ds_corr %>% summarise(
  xy_mean = mean(corr_xy), 
  xy_sum = sum(corr_xy),
  xy_sd = sd(corr_xy), 
  xy_n = n(), 
  xy_se = xy_sd/sqrt(xy_n)) #can use earlier calculated vars later in your computation

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

#Group by as many factor combinations as you want
ds_corr %>% 
  group_by(class, half) %>% 
  summarise(xy_mean = mean(corr_xy), xy_sd = sd(corr_xy), xy_n = n(), xy_se = xy_sd/sqrt(xy_n))

#Across saves us from repetitive typing
#across(selected vars, function)
ds_corr %>% 
  group_by(class, half) %>% 
  summarise(across(corr_xy:corr_yz, mean))

### NOT COVERED

#I'm sick of typing out the formula for SE, so let's make it a function
#Much more of this in a few weeks
se <- function(x) sd(x)/sqrt(length(x))

#Now we can use across with a list of functions for even more automation
results <- ds_corr %>% 
  group_by(class, half) %>% 
  summarise(across(starts_with("corr"), list(mean = mean, sd = sd, se = se)))
print(results)

#Great thing about saving your results as tibbles -> easy to select/filter! 
results %>% filter(half == "1st") %>% select(ends_with("mean"))

#Note that if you don't use a "named" list (mean = mean), the output isn't as nice
results <- ds_corr %>% 
  group_by(class, half) %>% 
  summarise(across(starts_with("corr"), list(mean, sd, se)))
print(results)

#This has to do with R's "list" type
fs_named_list <- list(mean = mean, sd = sd, se = se) #Named elements
fs_list <- list(mean, sd, se) #Numbered elements are more like vectors
fs_vector <- c("mean", "sd", "se")




          