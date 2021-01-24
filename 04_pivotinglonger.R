#Load packages
library(tidyverse)

rm(list = ls()) #clean variables out of environment

ds <- read_csv('data_example_4/vocab.csv')

print(ds)

#Perfect example of "columns as values"
#Age should be a variable, words should be a variable
#Each age-word pair is an observation that should appear as a row

#Wide to long with pivot_longer
#cols = which columns should be stacked
#names_to = what to call the variable that will contain our former column names
#values_to = what to call the variable that will contain the values
ds_long <- pivot_longer(ds, cols = everything(), names_to = "age", values_to = "word")

#Can drop NA values when pivoting
ds_long <- pivot_longer(ds, cols = everything(), names_to = "age", values_to = "word", values_drop_na = T)

#Can use tidy_select to choose which cols to select to stack 
#Uunstacked columns are retained, which can lead to some strange output
ds_long <- pivot_longer(ds, cols = ends_with(".5"), names_to = "age", values_to = "word", values_drop_na = T)

#More realistic example

#let's add some 'fixed' columns
ds <- ds %>% mutate(
  id = "Jonah",
  dob = as.Date("2017-08-30"),
  .before = "12"
)

#id/dob get repeated to match the new length of the data set
ds_long <- pivot_longer(ds, cols = -(id:dob), names_to = "age", values_to = "word", values_drop_na = T)
ds_long$age <- as.numeric(ds_long$age)
